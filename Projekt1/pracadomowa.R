# load packages devtools, openxlsx, RPostgreSQL, dplyr

library(devtools)
library(openxlsx)
library(RPostgreSQL)
library(dplyr)

# read and build fnction active_packages, which will read all packages from prvious point. 
#Print the text "packages ready" at the end of function

active_packages <- function() 
{
  library(devtools)
  library(openxlsx)
  library(RPostgreSQL)
  library(dplyr)
  print("packages ready")
}

# run function active_packages in concolse and check whether "packages ready" text appreared

# load all data from szczegoly_rekompensat table into data frame called df_compensations

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres", host = "localhost", port = 5432,
                 user = "postgres",
                 password = "postgresql") 
compensations <- dbGetQuery(con, "SELECT * from szczegoly_rekompensat")
df_compensations <- as.data.frame(compensations)

# check if table tab_1 exists in a connection defined in previous point

dbExistsTable(con, "tab_1")

# print df_compensations data frame summary vectors

print(df_compensations)
summary(df_compensations)

# create vector sample_vector which contains numbers 1,21,41 (don't use seq function)

sample_vector <- c(1,21,41)
sample_vector


# create vector sample_vector_seq which contains numbers 1,21,41 (use seq function)

sample_vector_seq <- seq(from = 1, to = 41, by = 20)
sample_vector_seq
