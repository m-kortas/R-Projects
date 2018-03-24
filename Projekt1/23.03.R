install.packages("tidyverse")
library(tidyverse)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres", host = "localhost", port = 5432,
                 user = "postgres",
                 password = "postgresql") 
rekompensaty_db <- dbGetQuery(con, "SELECT * from szczegoly_rekompensat")

ggplot(data = rekompensaty_db) + 
  geom_point(mapping= aes(x=konto, y=kwota))

ggplot(data= rekompensaty_db) +
  geom_point(mapping= aes(
    x=data_utworzenia, 
    y=kwota,
    color=konto))

wnioski <- dbGetQuery(con, "SELECT * from wnioski")

ggplot(data = wnioski) +
  ylim(0, 10000) + 
  geom_point(mapping= aes(
    x=data_utworzenia, 
    y=kwota_rekompensaty,
    color=typ_wniosku))

ggplot(data = wnioski) +
  geom_point(mapping= aes(
    x=data_utworzenia, 
    y=kwota_rekompensaty,
    color=jezyk)) + 
  facet_grid(kanal ~ typ_wniosku)


ggplot(data = wnioski) +
  ylim(0, 15000) + 
  xlim(0,20) + 
  geom_point(mapping = aes(
    x = liczba_pasazerow,
    y = kwota_rekompensaty,
    color=jezyk)) +
  geom_smooth(mapping = aes(
    x = liczba_pasazerow,
    y = kwota_rekompensaty,
    linetype = jezyk)) + 
  facet_wrap(~ jezyk, ncol = 3)



