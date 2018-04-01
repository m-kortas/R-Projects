#install.packages("shinydashboard")
library(shinydashboard)
library(RPostgreSQL)
library(plyr)
library(dplyr)
library(scales)
library(wordcloud)
library(syuzhet)
library(tidyverse)
library(XML)
library(RCurl)
library(tidyr)



ui <- dashboardPage(
  # HEADER
  ################################################################################################
  dashboardHeader(title = "Polls analysis"
                  #,dropdownMenu(type = "tasks",
                  #             taskItem("ABC")
                  #             )
  ),
  # SIDEBAR
  ################################################################################################
  dashboardSidebar(
    sidebarMenu(
      menuItem("Political party", tabName = "tab_partie", icon = icon("th")),
      
      menuItem("Results", tabName = "tab_wyniki_ogolne", icon = icon("dashboard"),  badgeColor = "green",
               menuSubItem("General results", tabName = "tab_wyniki_ogolne", icon = icon("th")),
               menuSubItem("Detailed results", tabName = "tab_wyniki_szczegolowe", icon = icon("th"))
      ),
      
      menuItem("Text mining (DEV)", tabName = "tab_text_mining", icon = icon("th"),
               menuSubItem("Word frequencies", tabName = "tab_text_mining_czestosc_slow", icon = icon("th")),
               menuSubItem("Word cloud", tabName = "tab_text_mining_chmura_slow", icon = icon("th")),
               menuSubItem("Find freq terms", tabName = "tab_text_mining_czestosci", icon = icon("th")),
               menuSubItem("Associacis", tabName = "tab_text_mining_asocjacje", icon = icon("th")),
               menuSubItem("Emotions", tabName = "tab_text_mining_emocje", icon = icon("th")),
               menuSubItem("Sentiment", tabName = "tab_text_mining_sentyment", icon = icon("th"))
      ),
      
      menuItem("Creators", tabName = "tab_creators", icon = icon("th"),
               menuSubItem("Monika Serkowska", tabName = "tab_creators_ms", icon = icon("th")),
               menuSubItem("Magdalena Kortas", tabName = "tab_creators_mk", icon = icon("th")),
               menuSubItem("Wojciech Artichowicz", tabName = "tab_creators_wa", icon = icon("th"))
      )      
      
    )
  ),
  
  # BODY
  ################################################################################################
  dashboardBody(
    tabItems(
      # Partie
      ################################################################################################
      tabItem(tabName = "tab_partie",
              fluidRow(
                h2("Political parties")
              ),
              fluidRow(
                infoBox("TEST",  "TEST", icon = icon("credit-card")),
                infoBoxOutput("winRate")
              )
      ),
      
      # Wyniki
      ################################################################################################
      #ogolne
      tabItem(tabName = "tab_wyniki_ogolne",
              fluidRow(
                h2("General results")
              ),
              
              fluidRow(
                column(width = 10,
                       dataTableOutput("results")
                )
              )
      ),
      
      tabItem(tabName = "tab_wyniki_szczegolowe",
              fluidRow(h2("Detailed results")),
              fluidRow(
                radioButtons("in_rb_partie", label = h3("Partia polityczna"),
                             choices = list("A","B"),
                             inline = TRUE
                )
              )
      ),      
      # Text mining
      ################################################################################################
      tabItem(tabName = "tab_text_mining",
              fluidRow(
                h2("Text mining")
              )
      ), 
      tabItem(tabName = "tab_text_mining_czestosc_slow",
              fluidRow(
                h2("Text mining :: word frequencies")
              ),
              fluidRow(
                column(width = 6, plotOutput("word_freq_magda")
                )
              )
      ), 
      tabItem(tabName = "tab_text_mining_chmura_slow",
              fluidRow(
                h2("Text mining :: word cloud")
              )
      ),      
      tabItem(tabName = "tab_text_mining_czestosci",
              fluidRow(
                h2("Text mining :: findFreqTerms")
              )
      ), 
      tabItem(tabName = "tab_text_mining_asocjacje",
              fluidRow(
                h2("Text mining :: associacions")
              ),
              
              fluidRow(
                textInput(inputId = "ass_text",
                             label = "Terms:",
                             value = "win"),
                
                numericInput(inputId = "ass_cor",
                             label = "CorLimit:",
                             value = 1 )
              ),
              fluidRow(
                verbatimTextOutput("find_ass")
              )
            
      ),   
      tabItem(tabName = "tab_text_mining_emocje",
              fluidRow(
                h2("Text mining :: emotions")
              )
      ),  
      tabItem(tabName = "tab_text_mining_sentyment",
              fluidRow(
                h2("Text mining :: sentiment")
              )
      ),
      
      # Credits
      ################################################################################################
      tabItem(tabName = "tab_creators", fluidRow(h2("Tworcy") ),
              menuSubItem("Monika Serkowska", tabName = "tab_creators_ms", icon = icon("th")),
              menuSubItem("Magdalena Kortas", tabName = "tab_creators_mk", icon = icon("th")),
              menuSubItem("Wojciech Artichowicz", tabName = "tab_creators_wa", icon = icon("th"))
      )      
    )
  )
)



bool_app_init <- TRUE

server <- function(input, output,session) {
  
  if (bool_app_init){
    print("init - loading data")
    
    bool_app_init<-FALSE
    
    link <- "https://docs.google.com/spreadsheets/d/1P9PG5mcbaIeuO9v_VE5pv6U4T2zyiRiFK_r8jVksTyk/htmlembed?single=true&gid=0&range=a10:o400&widget=false&chrome=false" 
    xData <- getURL(link)  #get link
    dane_z_html <- readHTMLTable(xData, stringsAsFactors = FALSE, skip.rows = c(1,3), encoding = "utf8") #read html
    df_dane <- as.data.frame(dane_z_html)   #data frame
    colnames(df_dane) <- df_dane[1,]  #nazwy kolumn
    df2 <- df_dane[2:nrow(df_dane),]  
    rm(df_dane)
    rm(dane_z_html)
    rm(xData)
    for (i in 8:16)
      df2[[i]] <- as.numeric(gsub(",",".",df2[[i]]))      # remove commas
    colnames(df2)[2]<- "Osrodek"  
    
    
    observe({
      # Can also set the label and select items
      updateRadioButtons(session, "in_rb_partie",
                         choices = as.list(colnames(df2)[8:16] ),
                         inline = TRUE
      )
    })     
    
  }
  
  # Associations (Magda)

  find_ass()
  output$find_ass <- renderPrint({
    findAssocs(dtm, terms = input$ass_text, corlimit = input$ass_cor)
  })

  
  results()
  output$results <-renderDataTable(df2)
  
  output$word_freq_magda <- renderPlot({
    ggplot(data = head(d,50), mapping = aes(x = reorder(word, freq), y = freq)) +
      geom_bar(stat = "identity") +
      xlab("Word") +
      ylab("Word frequency") +
      coord_flip() 
  }, bg="transparent")
  
}

results <- function() {      #funkcja results Magdy
  library(XML)
  library(RCurl)
  library(tidyr)
  link <- "https://docs.google.com/spreadsheets/d/1P9PG5mcbaIeuO9v_VE5pv6U4T2zyiRiFK_r8jVksTyk/htmlembed?single=true&gid=0&range=a10:o400&widget=false&chrome=false" 
  xData <- getURL(link)  #get link
  dane_z_html <- readHTMLTable(xData, stringsAsFactors = FALSE, skip.rows = c(1,3), encoding = "utf8") #read html
  df_dane <- as.data.frame(dane_z_html)   #data frame
  colnames(df_dane) <- df_dane[1,]  #nazwy kolumn
  df2 <- df_dane[2:nrow(df_dane),]  #pominięcie pierwszego wiersza
  for (i in 8:16)
    df2[[i]] <- as.numeric(gsub(",",".",df2[[i]]))      #przecinki
  colnames(df2)[2]<- "Osrodek"  #zmiana bo z polskim znakiem nie dzia?a 
  head(df2)
  df2
}


word_freq_magda <- function() {   # word frequency
  
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(RColorBrewer)
  library(tidyverse)
  
  filePath <- "parties_en.txt"
  text <- read_lines(filePath)
  docs <- Corpus(VectorSource(text))
  
  docs <- tm_map(docs, tolower) #mniejszy rozmiar
  docs <- tm_map(docs, removeNumbers) #numerki
  docs <- tm_map(docs, removeWords, stopwords("english")) #usuwanie
  docs <- tm_map(docs, removePunctuation) #punktuacja
  docs <- tm_map(docs, stripWhitespace) #białeznaki
  
  docs2 <-tm_map(docs, stemDocument)   #słowa kluczowe
  dtm <<- TermDocumentMatrix(docs2)      #matryca słów
  m   <- as.matrix(dtm)                   #na matrycę
  v   <- sort(rowSums(m), decreasing=TRUE)   #sortowanie według ilości dec
  d   <<- data.frame(word=names(v), freq=v)  #nowa data frame: słowo, ilość
  
  # bar chart
  ggplot(data = head(d,50), mapping = aes(x = reorder(word, freq), y = freq)) +
    geom_bar(stat = "identity") +
    xlab("Word") +
    ylab("Word frequency") +
    coord_flip() }

 # find associations

find_ass <- function () {
  library(tm)
  findAssocs(dtm, terms = input$ass_text, corlimit = input$ass_cor)
}

shinyApp(ui, server)