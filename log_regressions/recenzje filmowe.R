library(text2vec)
library(data.table)
library(magrittr)

data('movie_review')
setDT(movie_review)
setkey(movie_review)

set.seed(2018)
dim(movie_review)

all_ids = movie_review$id
train_ids = sample(all_ids, 4000)
test_ids = setdiff(all_ids, train_ids)
train = movie_review[J(train_ids)]
test = movie_review[J(test_ids)]

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$review,
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = train$id,
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)

vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)
dim(dtm_train)
 
####################
library(glmnet)
NFOLDS = 4

glmnet_classifier = cv.glmnet(x = dtm_train, y= train[['sentiment']],
          family = 'binomial',
          alpha = 1,
          type.measure = 'auc',
          nfolds = NFOLDS)

it_test = test$review %>%
  prep_fun %>% tok_fun %>%
  itoken(ids = test$id, progressbar = FALSE)

dtm_test = create_dtm(it_test, vectorizer)
preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1]
preds_class = predict(glmnet_classifier, dtm_test, type = 'class')[,1]

head(preds)
head(preds_class)

glmnet:::auc(test$sentiment, preds)

ROCRpred <- prediction(preds, test$sentiment)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
par(mfrow = c(1,1))
plot(ROCRperf, colorize = TRUE)

table(test$sentiment, preds>0.5)

data_with_predicted_sentiment <- cbind(test, preds)

write.csv(data_with_predicted_sentiment, 'data_with_predicted_sentiment.csv')
getwd()
