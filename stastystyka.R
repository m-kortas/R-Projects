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
