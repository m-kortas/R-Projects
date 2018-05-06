
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
  return((x-min(x))/(max(x)-min(x)))     #normalazing dataset
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
            label = TRUE, show.margins = FALSE)  #graph of contingency table
#size of balloon means numeber of occurences - similar sizes of balloons in case of mtcars

ca_result <- CA(dt, graph =FALSE)
print(ca_result)     #CA results: The row variable has  32  categories; the column variable has 11 categories
                     # The chi square of independence between the two variables is equal to 91.80067 (p-value =  1 )

eig.val <- get_eigenvalue(ca_result)  #eigenvalue correlation explanation
eig.val # Two first dimensions explain 87% (cumulative variance percent), typical boundary: 80% cumulative variance
fviz_screeplot(ca_result, addlabels = TRUE, ylim = c(0,50)) #correlation explanation plotted 

fviz_contrib(ca_result, choice = "row", axes = 1, top = 10) #contribution to  dim1: Honda Civic the highest contribution to Dim-1
# hovewer many cars share similar contribution # the highest influence if point goes left or right
fviz_contrib(ca_result, choice = "row", axes = 2, top = 10) #contribution to  dim2: Masarati Bora the highest contributuon to Dim-2
# hovewer many cars share similar contributuion # the highest influence if point goes up or down

fviz_ca_biplot(ca_result, col.row = "contrib", repel = TRUE) #symmetric plot
#Points with similar profile are closer on the factor map
