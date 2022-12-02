# Name: Oliver De Sa
# SciNet username: tmp_odesa
# Description:
#   Functions to be used for Assignment 10

word_count <- function(words, stop_words=NULL){
  if (is.null(stop_words)){
    words <- strsplit(words, "[^a-zA-Z']+")
    wordDF <- data.frame(table(wordList[[1]]))
    colnames(wordDF) <- c('word', 'count')
  } else {
    words <- strsplit(words, "[^a-zA-Z']+")
    wordDF <- data.frame(table(wordList[[1]]))
    colnames(wordDF) <- c('word', 'count')
    index <- wordDF$word %in% stop_words
    wordDF <- wordDF[!index,]
  }
}