
library("RPostgreSQL")
library(FactoMineR)
library(factoextra)
library(gplots)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres", host = "localhost", port = 5432,
                 user = "postgres",
                 password = "postgresql") 
dbExistsTable(con, "wnioski")

###
wnioski_db <- dbGetQuery(con, "SELECT typ_wniosku, partner from wnioski")
wnioski_db
dt <- table(wnioski_db)   # to table 
wnioski_result <- CA(dt, graph =FALSE)
balloonplot(t(dt), main = "wnioski", xlab="", ylab="", 
            label = TRUE, show.margins = FALSE) 

eig.val <- get_eigenvalue(wnioski_result)  #eigenvalue wyjaśnienie zależności
eig.val

fviz_screeplot(wnioski_result, addlabels = TRUE, ylim = c(0,50)) #wyjaśnienie zależności
fviz_contrib(wnioski_result, choice = "row", axes = 1, top = 10) #contrib dim1
fviz_contrib(wnioski_result, choice = "row", axes = 2, top = 10) #contrib dim2

fviz_ca_biplot(wnioski_result, col.row = "contrib")


####
trasy_db <- dbGetQuery(con, "SELECT wylot_kod_regionu, przylot_kod_regionu from o_trasy")
head(trasy_db)
trasy_db
dt2 <- table(trasy_db) 
trasy_result <- CA(dt2, graph =FALSE)
balloonplot(t(dt2), main = "Regions", xlab="", ylab="", 
            label = TRUE, show.margins = FALSE)   #najwięcej EU1 do EU1
eig.val2 <- get_eigenvalue(trasy_result) 
eig.val2  #15 dimensions, 4 tłumaczą 99%
fviz_screeplot(trasy_result, addlabels = TRUE, ylim = c(0,50)) # to samo

fviz_contrib(trasy_result, choice = "row", axes = 1, top = 10) #contrib dim1 - najbardziej tlumaczą EU2, reszta poniżej (czy punkt pojdzie w prawo czy w lewo ma wplyw)
fviz_contrib(trasy_result, choice = "row", axes = 2, top = 10) #contrib dim2 - najbardziej tlumaczy Af4  (czy punkt pojdzie w gore czy w dol tlumaczy Af4)

fviz_ca_biplot(trasy_result, col.row = "contrib") # loty z Eastern Africa do Eastern Africa 
#Eastern/Central Europe do EC Europe
#CentralAsia między EU2 a resztą
#reszta razem
#Latin America outlayer

