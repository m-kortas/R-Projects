library(FactoMineR)
library(factoextra)
library(gplots)

data(housetasks)
head(housetasks)

dt <- as.table(as.matrix(housetasks))   # to table 
balloonplot(t(dt), main = "housetasks", xlab="", ylab="", 
      label = TRUE, show.margins = FALSE)  #plot

ca_result <- CA(dt, graph =FALSE)
print(ca_result)     #CA dla housetasks

eig.val <- get_eigenvalue(ca_result)  #eigenvalue wyjaśnienie zależności
eig.val

fviz_screeplot(ca_result, addlabels = TRUE, ylim = c(0,50)) #wyjaśnienie zależności

fviz_contrib(ca_result, choice = "row", axes = 1, top = 10) #contrib dim1
fviz_contrib(ca_result, choice = "row", axes = 2, top = 10) #contrib dim2

fviz_ca_row(ca_result) #just rows
fviz_ca_col(ca_result) #just columns

fviz_ca_biplot(ca_result, col.row = "contrib")

