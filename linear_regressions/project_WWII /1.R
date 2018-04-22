library(caroline)
library(devtools)
library(openxlsx)
library(XML)
library(RCurl)
library(e1071)


# Is there a relationship between the daily minimum and maximum temperature? 
# Can you predict the maximum temperature given the minimum temperature? 

weather

head(weather)
str(weather)

weather2 <- weather[c("MinTemp", "MaxTemp")]

sum(is.na(weather2))  #ile pustych: 0

#####

scatter.smooth(x=weather2$MaxTemp, y=weather2$MinTemp, main="Max & Min") 
abline(lm(weather2$MinTemp~ weather2$MaxTemp), col="blue", lwd=3)

cor(weather2$MinTemp, weather2$MaxTemp) # 0.87 wysoka korelacja


par(mfrow=c(2,2))
boxplot(weather2$MinTemp, main="min") #  dużo outlayerów poniżej
boxplot(weather2$MaxTemp, main="max") #  dużo outlayerów poniżej i powyżej 


par(mfrow=c(2,2))
plot(density(weather2$MinTemp), main="Min", ylab="Częstotliwość",
     sub=paste("Skośność:", round(e1071::skewness(mtcars$mpg), 1)))  # duża skośność
plot(density(weather2$MaxTemp), main="Max", ylab="Częstotliwość",
     sub=paste("Skośność:", round(e1071::skewness(mtcars$cyl), 1)))  # duża skośność



# Is there a relationship between the daily minimum and maximum temperature? YES
# Can you predict the maximum temperature given the minimum temperature? YES   min_temp*1.87=max_temp
