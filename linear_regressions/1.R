# 1 Analiza CARS

  # 1.1. Dataset cars - wyświetl pierwszych kilka oberwacji ze zbioru cars.
require("datasets")
data("cars")
?cars
head(cars)

# 1.2. Scatter plot - sprawdzamy czy może istnieć liniowa zależnośc pomiędzy zmiennymi.
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist & Speed")

# 1.3. BoxPlot – sprawdzamy czy istnieją wartości mocno odstające od reszty
par(mfrow=c(1,2))
boxplot(cars$speed, main="Speed")
boxplot(cars$dist, main="Distance")

# 1.4. Funkacja gęstośći - sprawdzamy czy zmienna zależna ma rozkład normalny
install.packages('e1071', dependencies = TRUE)
library(e1071)

par(mfrow=c(1,2))

plot(density(cars$speed), main="Speed", ylab="Częstotliwość",
     sub=paste("Skośność:", round(e1071::skewness(cars$speed), 1)))
polygon(density(cars$speed), col="violet")

plot(density(cars$dist), main="Distance", ylab="Częstotliwość",
     sub=paste("Skośność:", round(e1071::skewness(cars$dist), 1)))
polygon(density(cars$dist), col="blue")

# 1.5. Korelacja

cor(cars$speed, cars$dist)

# 1.6. Model liniowy

linearMod <- lm(dist ~ speed, data=cars)
print(linearMod)






# 2. Airquality

  # 2.1. Ładowanie i eksploracja datasetu Air Quality
  data("airquality")
?airquality

  str(airquality)
  head(airquality)

# 2.2. Przygotowanie danych

# Sprawdź dokumentację dla funkcji mapply
  ?mapply

# Sprawdź czy występują kolumny z brakującymi danymi
  col1 <-mapply(anyNA, airquality)
  col1

# Zmień brakujące wartości na miesięczną średnią, osobno dla Ozonu i Solar.R
for (i in 1:nrow(airquality)){
  if(is.na(airquality[i, "Ozone"])){
    airquality[i,"Ozone"] <- mean(airquality[which(airquality[,"Month"]==airquality[i, "Month"]),
                                   "Ozone"],na.rm = TRUE)
}
  if(is.na(airquality[i, "Solar.R"])){
    airquality[i,"Solar.R"] <- mean(airquality[which(airquality[,"Month"]==airquality[i, "Month"]),
                                             "Solar.R"],na.rm = TRUE)
  }
}

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

airquality$Ozone <- normalize(airquality$Ozone)
airquality$Solar.R <- normalize(airquality$Solar.R)

str(airquality)

# 2.3. Regresja liniowa z wykorzystaniem funkcli lm


# Badamy jak promieniowanie słoneczne wpływa na ozon

Y <- airquality[, "Ozone"]
X <- airquality[, "Solar.R"]

model1<- lm(Y~X)
model1

plot(Y~X)
abline(model1, col="blue", lwd=3)
?abline

# Badamy jak wiatr wpływa na ozon

Y <- airquality[, "Ozone"]
Z <- airquality[, "Wind"]

model2<- lm(Y~Z)
model2

plot(Y~Z)
abline(model2, col="blue", lwd=3)
?abline

# 2.4. Predykcja 
# Predykcja ozonu dla promieniowania słonecznego = 10

p1 <- predict(model1, data.frame("X"=0.1))
p1

# Predykcja ozonu dla wiatru = 5

p2 <- predict(model2, data.frame("Z"=0.2))
p2



