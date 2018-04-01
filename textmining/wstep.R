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

stem_doc <- stemDocument(c("win", "winning", "winner", "window"))
sample_text <- ("winns")
completion_results <- stemCompletion(stem_doc, sample_text)       #completion z stem_doc i sample_text

docs2 <-tm_map(docs, stemDocument)   #słowa kluczowe
dtm <- TermDocumentMatrix(docs2)      #matryca słów
m   <- as.matrix(dtm)                   #na matrycę
v   <- sort(rowSums(m), decreasing=TRUE)   #sortowanie według ilości dec
d   <- data.frame(word=names(v), freq=v)  #nowa data frame: słowo, ilość
head(d,10)  # top10

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=TRUE, rot.per = 0.1,    #wordcloud
          colors=brewer.pal(20, "Dark2"))

# flip coordinates
ggplot(data = filter(d, freq > 100), mapping = aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity") +
  xlab("Word") +
  ylab("Word frequency") +
  coord_flip()

# rotate label
ggplot(data = filter(d, freq > 100), mapping = aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity") +
  xlab("Word") +
  ylab("Word frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

findFreqTerms(dtm, lowfreq = 200)
findAssocs(dtm, terms="sleep", corlimit = 0.3)

# SENTIMENT
install.packages("syuzhet")
library('syuzhet')

d$word
df_sentiment<-get_nrc_sentiment(as.String(d$word)) # sentiment from unique words
df_sentiment     #sentiment
class(df_sentiment)  #to jest class 

df_sentiment_transposed <- t(df_sentiment) # transpose data frame from columns to rows
df_sentiment_final <- data.frame(sentiment=row.names(df_sentiment_transposed),sent_value=df_sentiment_transposed, row.names=NULL) # prepare final data frame with emotions in 1st column, values in 2nd

df_emotions <- df_sentiment_final[1:8,]    #emocje
df_sentiments <- df_sentiment_final[9:10,]  #pozytywne i negatywne

df_emotions   # emocje
df_sentiments  # pozytywne i negatywne

ggplot(data= df_emotions, mapping= aes(x=sentiment, y = sent_value, color=sentiment, fill = sent_value))+
  geom_bar(stat="identity") +
  xlab("emotion")+
  ylab("words count") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
