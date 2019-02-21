library(shiny)
library(shinydashboard)
library(tm)
#library(stringi)
#library(tidyverse)
#library(dplyr)


source("clear_corpus.R")
source("clean_corpus_senti.R")
source("predic_svm_senti.R")
source("predic_svm_dimension.R")

svm_mod_senti<-readRDS("svm_senti_diccionario.rds")
svm_mod_dimension<-readRDS("svm_mod_dimension.rds")
test<-readRDS("df_test.rds")
test1<-readRDS("df_test_diccionario.rds")
vector_dimension<-readRDS("vector_dimension.rds")

server<-function(input, output, session) {
  
  #Texto ingresado por el usuario
  respuestas_pred_senti<-reactive({req(input$texto)
  #Preccin modelo SVM senti (Positivo Negativo)  
  predic_svm_senti(input$texto)})
  
  #Texto ingresado por el usuario
  respuestas_pred_dimen<-reactive({req(input$texto)
  #Preccin modelo SVM senti (Positivo Negativo)  
  predic_svm_dimen(input$texto)})

  
  # observeEvent(input$enviar,{
    
    output$respuesta=renderText(paste0("El cliente opina ",respuestas_pred_senti()," de ",respuestas_pred_dimen()))
 
   #}) 
}
