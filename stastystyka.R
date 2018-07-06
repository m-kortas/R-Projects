?binomial

dbinom(20,40,0.5)
pbinom(20,30,0.5)


#Zad 1  Jakie jest prawdopodobieństwo wyrzucenia 1 orła w 3 próbach

dbinom(1,3,0.5)

#Zad 2  Niech zmienna X oznacza prawdopodobieństwo wyciągnięcia
#dwóch króli w dziewięciu losowaniach z talii kart bez zwracania. Czy
#X jest zmienną o rozkładzie dwumianowym? Uzasadnij.

#jak wyciągniemy jedną kartę , to już każda próba będzie inna, bo już zbiór będzie naruszony


#Drużyna piłkarska strzela rzuty karne ze skutecznością 70 procent.
#Jakie jest prawdopodobieństwo, że w serii 5 rzutów karnych
#drużyna trafi:
#  a. dokładnie 2 razy
dbinom(2,5,0.7)

#b. nie więcej niż 2 razy 
pbinom(2,5,0.7)

dbinom(0,5,0.7)+dbinom(1,5,0.7)+dbinom(2,5,0.7)


pnorm(-3)
1-pnorm(-3)
1-2*pnorm(-3)

dnorm(0)

pnorm(1.52)
pnorm(-1.44)
pnorm(32,57.24,17.42)

rnorm(10)
rnorm(10)
mydist = rnorm(1000)
plot(density(mydist))
hist(mydist)

ecdf(mydist)(2)
pnorm(2)

X <- c(1, 11, 2, 3, 1, 3, 3, 1, 1, 1)
X
plot(density(X))
plot(density(log(X)))

data <- data("USArrests")
USArrests$Murder
USArrests$Assault
USArrests$Rape
plot(density(USArrests$Murder))
plot(density(USArrests$Assault))
plot(density(USArrests$Rape))
plot(density(USArrests$UrbanPop))

patients <- c(1.5, 2.9, 0.9, 3.9, 3.2, 2.1)
patients.mean <- mean(patients)
patients.sd <- sqrt(sum((patients - patients.mean)^2/(6-1)))
patients.sd


election <- c(rep(0,57), rep(1,43))
election
election.mean <- mean(election)
election.sd <- sqrt(sum((election - election.mean)^2/(100-1)))
election.sd
sd(election)
est.sd <- election.sd/10

pnorm(-4)+pnorm(4, lower.tail = F)
options(scipen=50)
pnorm(-4)+pnorm(4, lower.tail = F)
