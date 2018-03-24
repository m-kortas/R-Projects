install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("slam")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

filePath <- "http://www.gutenberg.org/cache/epub/103/pg103.txt"
text <- readLines(filePath)
docs <- Corpus(VectorSource(text))

class(docs) #klasa
typeof(docs) #typ
inspect(docs) #inspekcja

docs[[3]]       
docs[[3]]$content  
docs[[3]]$meta      #metadata

docs <- tm_map(docs, tolower) #mniejszy rozmiar
docs <- tm_map(docs, removeNumbers) #numerki
docs <- tm_map(docs, removeWords, stopwords("english")) #usuwanie
docs <- tm_map(docs, removePunctuation) #punktuacja
docs <- tm_map(docs, stripWhitespace) #białeznaki

stopwords("spanish")

inspect(docs)

stem_doc <- stemDocument(c("win", "winning", "winner", "window"))
sample_text <- ("winns")
completion_results <- stemCompletion(stem_doc, sample_text)

docs2 <-tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs2)
m   <- as.matrix(dtm)                   #na matrycę
v   <- sort(rowSums(m), decreasing=TRUE)   #sortowanie
d   <- data.frame(word=names(v), freq=v)  #nowa data frame
head(d,10)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=TRUE, rot.per = 0.1,
          colors=brewer.pal(20, "Dark2"))
