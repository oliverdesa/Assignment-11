# Name: Oliver De Sa
# SciNet username: tmp_odesa
# Description:
#   Functions to be used for Assignment 10

word_count <- function(words, stop_words = NULL){
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

read_file <- function(file, flag = NULL){
  if (is.null(flag)){
    my.file <- file("shakespeare.sonnets.txt", "r")
    file.content <- readLines(my.file, warn = FALSE)
    close(my.file)
    one.string <- paste(file.content, collapse = "")
    return(one.string)
    
  } else {
    my.file <- file("shakespeare.sonnets.txt", "r")
    file.content <- tolower(readLines(my.file, warn = FALSE))
    close(my.file)
    one.string <- paste(file.content, collapse = "")
    return(one.string)
    }
}

plotTopWords <- function(wordcount, n = 10, filename = "word_count.pdf") {
  library(ggplot2)
  
  wordcount_sorted <- wordcount[order(-wordcount$count),]
  
  topN <- wordcount_sorted[1:n,]
  
  ggplot(topN, aes(x = count, xend = 0, y = word, yend = word, colour = word)) + 
    geom_segment() +
    geom_point() +
    scale_y_discrete(limits = rev(topN$word)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Number of occurrences",
         y = "Words") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(), legend.position = "off")
  
  ggsave(filename, height = n/4)