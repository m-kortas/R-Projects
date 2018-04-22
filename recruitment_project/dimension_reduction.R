library(gridExtra)
library(corrplot)
library(ggplot2)

rectask <- read.csv("/Users/ja/R-Projects/recruitment_project/train.csv", header = T, row.names = 1, sep=",", dec="." )

dim(rectask) #rekordy i kolumny
sum(is.na(rectask))  #ile pustych
