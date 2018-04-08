#https://earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/

install.packages("rtweet")
install.packages("httpuv")

library(rtweet)
library(httpuv)

# whatever name you assigned to your created app
appname <- "magda_sentiment_analysis"

## api key (example below is not a real key)
key <- "3d09h36rBQoSThXzaHqnIaezI"

## api secret (example below is not a real key)
secret <- "omOF174fJEj8OfwlHUNWfrdtt4NlY9tPyxH7igQcvy3dscOEb3"


twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

smolensk_tweets <- search_tweets(q = "#smolensk",
                               n = 20)

# pierwsze rekordy
head(smolensk_tweets, n = 30)
head(smolensk_tweets$screen_name)
head(smolensk_tweets$text)
head(smolensk_tweets$country_code)

# struktura
names(smolensk_tweets)
str(smolensk_tweets)
dim(smolensk_tweets)

library(dplyr)

#dataTable: username + occurencies

smolensk_tweets %>%
  group_by(user_id) %>%
  summarise(liczba = n())

#dataTable: sources + occurencies

smolensk_tweets %>%
  group_by(source) %>%
  summarise(liczba = n())

#dataTable: top 20 tweets based on favourites count

top20 <- arrange(smolensk_tweets, desc(favorite_count))
head(top20, n = 20)
