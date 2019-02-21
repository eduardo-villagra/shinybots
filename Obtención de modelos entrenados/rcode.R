## Entrenamiento y test

#Librerias
library(tm)
library(stringi)
library(caret)
library(tidyverse)
library(dplyr)
library(readxl)



#Recursos necesarios
source("Funciones/clean_corpus.R")
source("Funciones/clean_corpus_senti.R")

#Cargar Datos
data<-readRDS("data/PQRS.rds") # La data es privada no puedo adjuntarla en el github
diccionario_sentimientos<-read_excel("input/diccionario sentimientos.xlsx")
data<-bind_rows(diccionario_sentimientos,diccionario_sentimientos,data)
vector_dimension<-readRDS("variables/vector_dimension.rds")



#Formato
data<-subset(data,is.na(Sentido1)==F)
data$Sentido1<-factor(data$Sentido1)
data$Dimension1<-factor(data$Dimension1)
data$Razones<-sapply(data$Razones, function(x) stri_trans_general(x, "Latin-ASCII"))



#Trasformar a corpus y limpiar 
comentarios_corpus<-VCorpus(VectorSource(data$Razones))
comentarios_corpus_clean<-limpiar_corpus(comentarios_corpus)

#Transformar corpus a DTM
comentarios_dtm<-DocumentTermMatrix(comentarios_corpus_clean)

#Muestras train y test (80% 20%)
n<-nrow(data)
data_train<-data[1:(n-284),]
data_test<-data[((n-283):n),]
 
nn<- nrow(comentarios_dtm)
comentarios_dtm_train<-comentarios_dtm[1:(nn-284),]
comentarios_dtm_test<-comentarios_dtm[(nn-283):nn,]

#Covertimos en dataframe los dtm
comentarios_df_train <- comentarios_dtm_train %>% as.matrix() %>% as.data.frame()
comentarios_df_test <- comentarios_dtm_test %>% as.matrix() %>% as.data.frame()

dfclear<-comentarios_df_test[1,]
saveRDS(dfclear[-1,],"variables/df_test_diccionario.rds")

###################Modelado sentimiento########################

##SVM train y test

trctrl <- trainControl(method = "none") #No se utiliza remuestreo 


svm_mod_senti <- train(x = comentarios_df_train,   #Entrenando Modelo
                 y = data_train$Sentido1,
                 method = "svmLinearWeights2",
                 trControl = trctrl,
                 tuneGrid = data.frame(cost = 1, 
                                       Loss = 0, 
                                       weight = 1))

svm_pred_senti <- predict(svm_mod_senti,          #Predicci?n Modelo
                    newdata = comentarios_df_test)


svm_cm <- confusionMatrix(svm_pred_senti, data_test$Sentido1) #Tabla de confusi?n
svm_cm

dataN<-subset(data_test,svm_pred_senti!=Sentido1)

saveRDS(svm_mod_senti,"Modelos/svm_senti_diccionario")



## Naive-Bayes train y test 

naive_bayes_mod <- train(x = comentarios_df_train,  #Entrenando Modelo 
                y = data_train$Sentido1,
                method = "naive_bayes",
                trControl = trctrl,
                tuneGrid = data.frame(laplace = 0,
                                      usekernel = FALSE,
                                      adjust = FALSE))


naive_bayes_pred_senti <- predict(naive_bayes_mod,          #Predicci?n Modelo
                          newdata = comentarios_df_test)

glimpse(comentarios_df_test)
nb_cm <- confusionMatrix(naive_bayes_pred_senti, data_test$Sentido1) #Tabla de confusi?n
nb_cm

saveRDS(naive_bayes_mod,"Modelos entrenados/nave_bayes_senti")

## LogitBoost train y test 

logitboost_mod_senti <- train(x =  comentarios_df_train,                #Entrenando Modelo 
                        y = data_train$Sentido1, 
                        method = "LogitBoost",
                        trControl = trctrl)


logitboost_pred_senti <- predict(logitboost_mod_senti,          #Predicci?n Modelo
                                  newdata = comentarios_df_test)


lb_cm <- confusionMatrix(logitboost_pred_senti, data_test$Sentido1) #Tabla de confusi?n
lb_cm

saveRDS(logitboost_mod_senti,"Modelos entrenados/logitboost_senti")


## Random forest train y test 

rf_mod_senti <- train(x = comentarios_df_train, 
                y = data_train$Sentido1, 
                method = "ranger",
                trControl = trctrl,
                tuneGrid = data.frame(mtry = floor(sqrt(dim(comentarios_df_train)[2])),
                                      splitrule = "gini",
                                      min.node.size = 1))

rf_pred_senti<- predict(rf_mod_senti,          #Predicci?n Modelo
                                 newdata = comentarios_df_test)


rf_cm <- confusionMatrix(rf_pred_senti, data_test$Sentido1) #Tabla de confusi?n
rf_cm

saveRDS(rf_mod_senti,"Modelos entrenados/rf_senti")

## nnet train y test 

nnet_mod_senti <- train(x = comentarios_df_train,
                  y = data_train$Sentido1,
                  method = "nnet",
                  trControl = trctrl,
                  tuneGrid = data.frame(size = 1,
                                        decay = 5e-4),
                  MaxNWts = 120000)

data_test$nnet_pred_senti<- predict(nnet_mod_senti,          #Predicci?n Modelo
                        newdata = comentarios_df_test)


nnet_cm <- confusionMatrix(nnet_pred_senti, data_test$Sentido1) #Tabla de confusi?n
nnet_cm

dataN2<-subset(data_test,nnet_pred_senti!=Sentido1)

saveRDS(nnet_mod_senti,"Modelos entrenados/nnet_senti")

### Comparando modelos

mod_results <- rbind(
  svm_cm$overall, 
  nb_cm$overall,
  lb_cm$overall,
  #rf_cm$overall,
  nnet_cm$overall
) %>%
  as.data.frame() %>%
  mutate(model = c("SVM", "Naive-Bayes", "LogitBoost", "Neural network"))

mod_results %>%
  ggplot(aes(model, Accuracy)) +
  geom_point() +
  ylim(0, 1) +
  geom_hline(yintercept = mod_results$AccuracyNull[1],
             color = "red")


###################Modelado sentimiento######################## 


review_ted_model <- train(x = comentarios_df_train,
                          y = data_train$Dimension1, method = "svmLinear3")



svml_pred_dimension<- predict(review_ted_model,          #Predicci?n Modelo
                          newdata = comentarios_df_train)


svml_cm <- confusionMatrix(svml_pred_dimension, data_train$Dimension1) #Tabla de confusi?n
svml_cm

saveRDS(review_ted_model,"Modelos/svm_mod_dimension")
