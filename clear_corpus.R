###Funcion Limpiar corpus

mi_funcion<-content_transformer(function(x,pattern) gsub(pattern," ",x))

limpiar_corpus <- function(corpus){
  corpus <- tm_map(corpus, mi_funcion, "[^\x01-\x7F]")
  corpus <- tm_map(corpus, mi_funcion, "/")
  corpus <- tm_map(corpus, mi_funcion, "@")
  corpus <- tm_map(corpus, mi_funcion, "\\|")
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,stopwords("spanish"))
  return(corpus)
}
