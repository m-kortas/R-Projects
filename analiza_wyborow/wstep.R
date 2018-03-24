library(caroline)

library(devtools)

read.table(file = "doR.csv", sep=",",dec=".", header=TRUE)
wnioski_csv <- read.table(file = "doR.csv", sep=",",dec=".", header=TRUE)
wnioski_csv

library(openxlsx)

wnioski_xlsx <- read.xlsx(xlsxFile = "doR.xlsx", sheet = 1)
head(wnioski_xlsx)

library(XML)
library(RCurl)

link <- "http://www.x-rates.com/table/?from=USD&amount=1" #definiujemy URL
typeof(link)
xData <- getURL(link) # pobieramy dane
xData
dane_z_html <- readHTMLTable(xData, stringsAsFactors = FALSE) #probujemy sparsowac HTMl do tabeli
length(dane_z_html) # ile tabel udalo sie uzyskac?
dane_z_html
dane_z_html[1]
top10 <- dane_z_html[[1]]
top10
typeof(top10)

library("rjson") # uaktywniamy konieczne pakiety

json_link <-
  "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
kraje <- fromJSON(file=json_link) # wczytujemy JSON
kraje #wyświetlamy cały obiekt
kraje[[2]][[3]]$name # wyswietlamy tylko nazwe dla trzeciego kraju z listy
kraje[[2]][[1]]

library("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres", host = "localhost", port = 5432,
                 user = "postgres",
                 password = "postgresql") 
dbExistsTable(con, "wnioski")
wnioski_db <- dbGetQuery(con, "SELECT * from wnioski")

library(dplyr)
przefiltrowane <- filter(wnioski_db, wnioski_db$kanal == 'bezposredni')
posortowane <- arrange(przefiltrowane, partner, desc(data_utworzenia))
podsumowanie <- summary(posortowane)
podsumowanie

posortowane %>%
  group_by(typ_wniosku) %>%  #groupby
  summarise(sr_rekomp = mean(kwota_rekompensaty),
            med = median(kwota_rekompensaty), liczba = n())

#zadanie1

df_stany_wnioskow <- posortowane %>%
  group_by(stan_wniosku) %>%  #groupby
  summarise(liczba = n(), rekompensata = sum(kwota_rekompensaty))
typeof(df_stany_wnioskow)
df_stany_wnioskow

#funkcja
pobierz_dane <- function(typ_wejscia, link) {
  if(identical(typ_wejscia,"csv")) {
    dane <- read.table(file = as.character(link),sep=",", dec=".", header=TRUE)
  } else if(identical(typ_wejscia,"xlsx")) {
    library(openxlsx)
    dane <- read.xlsx(xlsxFile = as.character(link), sheet = 1)
  } else if(identical(typ_wejscia,"html")) {
    library(XML) #wczytujemy pakiety
    library(RCurl)
    xData <- getURL(link) # pobieramy dane
    dane <- readHTMLTable(xData, stringsAsFactors = FALSE) # probujemy sparsowac HTMl do tabeli
  } else if(identical(typ_wejscia,"json")) {
    library("rjson") # uaktywniamy konieczne pakiety
    dane <- fromJSON(file=link) # wczytujemy JSON
  } else if(identical(typ_wejscia,"db")) {
    library("RPostgreSQL")
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = "pg_2", host = "localhost", port = 5432,
                     user = "postgres", 
                     password = "postgres") # to nie jest dobra praktyka na haslo, powinno byc w zmiennych srodowiskowych
    dane <- dbGetQuery(con, paste0("SELECT * from ",link))
  }
  return(dane)
}

#zad2

wylicz_wartosc <- function(x, y) 
  {
  print(x)
  print(y)
  return(sqrt(x^2+y^2))
}

wylicz_wartosc(4,5)
