library(ROCR)
library(titanic)

data('titanic_train')
######

titanic_data <- titanic_train[c("Survived", "Sex", "Age", "Pclass")]

mapply(anyNA,titanic_data)

titanic_data <- na.omit(titanic_data)  #odrzucenie pustych

titanic_data$Sex <- factor(titanic_data$Sex)
titanic_data$Pclass <- factor(titanic_data$Pclass)

glm_model <- glm(Survived ~ Sex + Age + Pclass, 
                 data = titanic_data, family = "binomial")

surv_prob <- predict(glm_model, titanic_data, type="response")

titanic_results <- cbind(titanic_data, surv_prob)

table(titanic_data$Survived, surv_prob > 0.9)

ROCRpred <- prediction(surv_prob, titanic_data$Survived)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')

par(mfrow= c(1,1))
plot(ROCRperf, colorize = TRUE)

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

########## correspondence analysis
library(FactoMineR)
library(factoextra)
library(gplots)
head(titanic_train)

titanic_ca <- titanic_train[c("Fare", "Pclass")]

# dt <- as.table(as.matrix(titanic_ca))   # to table 
dt <- table(titanic_ca)   # to table 

titanic_result <- CA(dt, graph =FALSE)

eig.val <- get_eigenvalue(titanic_result)  #eigenvalue wyjaśnienie zależności

fviz_ca_biplot(titanic_result, col.row = "contrib")

