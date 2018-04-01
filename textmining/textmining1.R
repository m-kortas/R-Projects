install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("slam")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)

filePath <- "/Users/ja/R-Projects/textmining/parties_en.txt"
text <- read_lines(filePath)
docs <- Corpus(VectorSource(text))

docs <- tm_map(docs, tolower) #mniejszy rozmiar
docs <- tm_map(docs, removeNumbers) #numerki
docs <- tm_map(docs, removeWords, stopwords("english")) #usuwanie
docs <- tm_map(docs, removePunctuation) #punktuacja
docs <- tm_map(docs, stripWhitespace) #białeznaki

docs2 <-tm_map(docs, stemDocument)   #słowa kluczowe
dtm <- TermDocumentMatrix(docs2)      #matryca słów
m   <- as.matrix(dtm)                   #na matrycę
v   <- sort(rowSums(m), decreasing=TRUE)   #sortowanie według ilości dec
d   <- data.frame(word=names(v), freq=v)  #nowa data frame: słowo, ilość
head(d,50)  # top50


# bar chart
ggplot(data = head(d,50), mapping = aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity") +
  xlab("Word") +
  ylab("Word frequency") +
  coord_flip()

############################### associations

ass <- findAssocs(dtm, terms="win", corlimit = 0.8)
ass
typeof(ass)

