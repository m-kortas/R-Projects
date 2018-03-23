install.packages("tidyverse")
library(tidyverse)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres", host = "localhost", port = 5432,
                 user = "postgres",
                 password = "postgresql") 
rekompensaty_db <- dbGetQuery(con, "SELECT * from szczegoly_rekompensat")

