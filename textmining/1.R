install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

filePath <- "http://www.gutenberg.org/cache/epub/103/pg103.txt"
text <- readLines(filePath)
text

docs <- Corpus(VectorSource(test))
