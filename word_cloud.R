library(wordcloud)
library(tidyverse)
library(tidytext)
library(shiny)
yelp_clean_data <- read.csv("yelp_cleaned_data.csv")
yelp_clean_data2 <- yelp_clean_data

yelp_clean_data2$american <- ifelse(str_detect(yelp_clean_data2$country,"american"),1,0)
yelp_clean_data2$mexican <- ifelse(str_detect(yelp_clean_data2$country,"mexican"),1,0)
yelp_clean_data2$italian <- ifelse(str_detect(yelp_clean_data2$country,"italian"),1,0)
yelp_clean_data2$chinese <- ifelse(str_detect(yelp_clean_data2$country,"chinese"),1,0)
yelp_clean_data2$japanese <- ifelse(str_detect(yelp_clean_data2$country,"japanese"),1,0)
yelp_clean_data2$middleeastern <- ifelse(str_detect(yelp_clean_data2$country,"middleeastern"),1,0)
yelp_clean_data2$french <- ifelse(str_detect(yelp_clean_data2$country,"french"),1,0)
yelp_clean_data2$korean <- ifelse(str_detect(yelp_clean_data2$country,"korean"),1,0)
yelp_clean_data2$spanish <- ifelse(str_detect(yelp_clean_data2$country,"spanish"),1,0)
yelp_clean_data2$thai <- ifelse(str_detect(yelp_clean_data2$country,"thai"),1,0)
yelp_clean_data2$greek <- ifelse(str_detect(yelp_clean_data2$country,"greek"),1,0)
yelp_clean_data2$canadian <- ifelse(str_detect(yelp_clean_data2$country,"canadian"),1,0)
yelp_clean_data2$vietnamese <- ifelse(str_detect(yelp_clean_data2$country,"vietnamese"),1,0)

newdata1 <- yelp_clean_data2[,41:72]
yelp_tidy <- gather(data = newdata1, key = "category", value = "value", 1:32)
yelp_tidy = yelp_tidy[yelp_tidy$value==1,]
yelp_tidy <- na.omit(yelp_tidy)

ui  <- fluidPage(
  titlePanel('Shiny App: Word Cloud for Yelp Key Words'),
  sidebarLayout(
    sidebarPanel(
      sliderInput('ngramCount', 'Number of Grams', min = 1, max = 5, value = 1),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 30, value = 15), hr(),
      sliderInput("max",
                  "Maximum Number of Words:", min = 4, max = 32, value = 10)
    ),
    mainPanel(
      plotOutput('wordcloud') 
    )
  )
)

server  <- function(input, output) {
  ngrams  <- reactive({
    input$ngramCount
  })
  output$wordcloud  <- renderPlot({
    yelp_tidy %>%
      select(category) %>%
      unnest_tokens(ngram, category, token="ngrams", n=ngrams()) %>%
      count(ngram) %>%
      with(wordcloud(ngram, n, min.freq = input$freq, max.words=input$max, rot.per=0.2, colors=c("#b4b3b3", "#969696", "#484848", "#8dc7d3")))
  })
}

library(rsconnect)
deployApp()
#shinyApp(ui, server)