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

linear






# 2. Airquality

  # 2.1. Ładowanie i eksploracja datasetu Air Quality
  data("airquality")
?airquality


# 2.2. Przygotowanie danych

# Sprawdź dokumentację dla funkcji mapply

# Sprawdź czy występują kolumny z brakującymi danymi

# Zmień brakujące wartości na miesięczną średnią, osobno dla Ozonu i Solar.R




# 2.3. Regresja liniowa z wykorzystaniem funkcli lm


# Badamy jak promieniowanie słoneczne wpływa na ozon

# Badamy jak wiatr wpływa na ozon



# 2.4. Predykcja 


# Predykcja ozonu dla promieniowania słonecznego = 10

# Predykcja ozonu dla wiatru = 5





# 3. Implementacja gradientu

  # 3.1. Generujemy losowy ciąg danych, w których y jest (zniekształconą) funkcją x
  
  # 3.2. Dopasowanie modelu regresji liniowej do danych
  
  # 3.3. Wizualizacja modelu
  
  # 3.4. Definiujemy funkcję kosztu
  
  # 3.5. Learning rate i limit iteracji
  
# 3.6. Historia (do wykresu później)

# 3.7. Inicjalizacja parametrów

# 3.8. Dodajemy kolumnę z 1'kami dla wyrazu wolnego

# 3.9. Gradient prosty

# 3.10. Wizualizacja danych

# 3.11. Funkcja kosztu dla kolejnych iteracji




# ZADANIE
  
  # Zadanie: zeksploruj dataset mtcars. Poszukaj interesujących zależności pomiędzy danymi.
  # Zaprezentuje wyniki, np w Shiny.
  
  data("mtcars")
head(mtcars)
?mtcars