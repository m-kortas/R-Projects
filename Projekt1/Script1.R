install.packages("caroline")
library(caroline)

install.packages("devtools")
library(devtools)

install_github("rstudio/shiny")

read.table(file = "doR.csv", sep=",",dec=".", header=TRUE)
wnioski_csv <- read.table(file = "doR.csv", sep=",",dec=".", header=TRUE)
wnioski_csv
