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

ggplot(data = wnioski, mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) +
  geom_point( mapping = aes(color=kanal)) +
  geom_smooth(data = filter(wnioski, kod_kraju=="PL"), aes(color = "PL")) + 
  geom_smooth(data = filter(wnioski, kod_kraju=="ES"), aes(color = "ES")) +
  scale_color_manual(name="legend", values=c("red", "blue", "black", "orange"))
  
#span im większy tym mniejszy przedział ufności ś

ggplot(data = wnioski) +
  geom_bar(mapping = aes(x = typ_wniosku))

library(scales)
ggplot(data = wnioski) +
  geom_bar(mapping = aes(x = typ_wniosku, y=..prop.., group = 1))+
  scale_y_continuous(labels=percent_format())  +  #grupowanie i procent z całości
  facet_wrap(~ kanal, ncol = 5)

ggplot(data = rekompensaty_db) +
  stat_summary(
    mapping = aes(x = konto,
                   y= kwota),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = rekompensaty_db, mapping = aes(x = konto, y = kwota))+
  geom_boxplot()


ggplot(data = wnioski, mapping = aes(x = jezyk, y = kwota_rekompensaty))+
  ylim(0,5000) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1)+
  coord_flip()