
# 3. Implementacja gradientu

# 3.1. Generujemy losowy ciąg danych, w których y jest (zniekształconą) funkcją x

x <- runif(1000, -5, 5)
y <- x + rnorm(1000) + 3

#x <- cars$speed
#x <- cars$dist

# 3.2. Dopasowanie modelu regresji liniowej do danych

res <- lm(y~x)
print(res)

# 3.3. Wizualizacja modelu

par(mfrow=c(1,1))
plot(x,y, col=rgb(0.2,0.4,0.6, 0.4), main='Regresja liniowa ~ gradient prost')
abline(res, col='blue')

# 3.4. Definiujemy funkcję kosztu

cost <- function(X, y, theta) {
  sum( (X %*% theta - y )^2)/(2*length(y))
}

# 3.5. Learning rate i limit iteracji

alpha <- 0.01
num_iters <- 1000

# 3.6. Historia (do wykresu później)

cost_history <- double(num_iters)
theta_history <- list(num_iters)

# 3.7. Inicjalizacja parametrów

theta <- matrix(c(0,0), nrow=2)

# 3.8. Dodajemy kolumnę z 1'kami dla wyrazu wolnego
X <- cbind(1, matrix(x))

# 3.9. Gradient prosty

for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X,y,theta)
  theta_history[[i]] <- theta
}

print(theta)

# 3.10. Wizualizacja danych

plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue')


# 3.11 Funkcja kosztu dla kolejnych iteracji

plot(cost_history, type='line', col='blue', lwd=2, main='Funkcja kosztu',
     ylab='cost', xlab='Iteracja')