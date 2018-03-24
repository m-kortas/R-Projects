install.packages("tidyverse")
library(tidyverse)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres", host = "localhost", port = 5432,
                 user = "postgres",
                 password = "postgresql") 
rekompensaty_db <- dbGetQuery(con, "SELECT * from szczegoly_rekompensat")
wnioski <- dbGetQuery(con, "SELECT * from wnioski")

ggplot(data = wnioski, mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) +
  geom_point( mapping = aes(color=kanal)) +
  geom_smooth( mapping = aes(linetype = jezyk, color=jezyk)) + 
  facet_wrap(~ jezyk, ncol = 3)
