

library(shiny)
library(shinythemes)
library(maps)
library(mapproj)

source("helpers.R")

stany <- readRDS("data/stany.rds")

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Tworzenie mapy demograficznej na podstawie 
               informacji ze spisu powszechnego w USA w 2010 r."),
      
      selectInput("var", 
                  label = "Wybierz zmienna do wyswietlenia",
                  choices = c("Procent Bialych", "Procent Czarnoskorych",
                              "Procent Latynosow", "Procent Azjatow"),
                  selected = "Procent Bialych"),
      
      sliderInput("zakres", 
                  label = "Wybrany zakres:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(plotOutput("mapa"))
  )
)

server <- function(input, output) {
  output$mapa <- renderPlot({
    data <- switch(input$var, 
                   "Procent Bialych" = stany$white,
                   "Procent Czarnoskorych" = stany$black,
                   "Procent Latynosow" = stany$hispanic,
                   "Procent Azjatow" = stany$asian)
    
    color <- switch(input$var, 
                    "Procent Bialych" = "darkgreen",
                    "Procent Czarnoskorych" = "black",
                    "Procent Latynosow" = "darkorange",
                    "Procent Azjatow" = "darkviolet")
    
    legend <- switch(input$var, 
                     "Procent Bialych" = "% Bialych",
                     "Procent Czarnoskorych" = "% Czarnoskorych",
                     "Procent Latynosow" = "% Latynosow",
                     "Procent Azjatow" = "% Azjatow")
    
    percent_map(var=data, color=color, legend.title = legend, max = input$zakres[2], min = input$zakres[1])
  })
}




shinyApp(ui, server)
