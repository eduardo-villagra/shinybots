predic_svm_dimen<-function(texto){
  
  nueva<-data.frame(texto)
  colnames(nueva)<-"Razones"
  nueva<-sapply(nueva, function(x) stringi::stri_trans_general(x, "Latin-ASCII"))
  
  #Trasformar a corpus y limpiar 
  nueva_corpus<-VCorpus(VectorSource(nueva))
  nueva_copur_limpio<-limpiar_corpus(nueva_corpus)
  
  #Transformar corpus a DTM
  obs_dtm<-DocumentTermMatrix(nueva_copur_limpio)
  obs_df <-as.data.frame(as.matrix(obs_dtm))
  test_nueva<-dplyr::bind_rows(test,obs_df)
  test_nueva[is.na(test_nueva)] <- 0
  
  svm_pred_dimen <- predict(svm_mod_dimension,          
                            newdata = test_nueva)
  
  prediccion<-as.character(svm_pred_dimen)
  return(prediccion)
}