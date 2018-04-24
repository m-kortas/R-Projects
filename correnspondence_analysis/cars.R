
library(e1071)
library(FactoMineR)
library(factoextra)
library(gplots)
data("mtcars")
?mtcars
mtcars
str(mtcars)

####

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

mtcars$mpg <- normalize(mtcars$mpg)
mtcars$cyl <- normalize(mtcars$cyl)
mtcars$disp <- normalize(mtcars$disp)
mtcars$hp <- normalize(mtcars$hp)
mtcars$drat <- normalize(mtcars$drat)
mtcars$wt <- normalize(mtcars$wt)
mtcars$qsec <- normalize(mtcars$qsec)
mtcars$vs <- normalize(mtcars$vs)
mtcars$am <- normalize(mtcars$am)
mtcars$gear <- normalize(mtcars$gear)
mtcars$carb <- normalize(mtcars$carb)

mtcars





dt <- as.table(as.matrix(mtcars))   # to table 


balloonplot(t(dt), main = "mtcars", xlab="", ylab="", 
            label = TRUE, show.margins = FALSE)  #plot




ca_result <- CA(dt, graph =FALSE)
print(ca_result)     #CA 

eig.val <- get_eigenvalue(ca_result)  #eigenvalue wyjaśnienie zależności
eig.val

fviz_screeplot(ca_result, addlabels = TRUE, ylim = c(0,50)) #wyjaśnienie zależności

fviz_contrib(ca_result, choice = "row", axes = 1, top = 10) #contrib dim1
fviz_contrib(ca_result, choice = "row", axes = 2, top = 10) #contrib dim2

fviz_ca_biplot(ca_result, col.row = "contrib", repel = TRUE)
