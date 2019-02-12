library(shiny)
library(dplyr)
library(lubridate)
library(glue)
library(ggplot2)
library(plotly)

isp <- read.csv2("./../../../data/BaseEstadoTaxaMes.csv") %>% 
  mutate(data = glue("{vano}-{mes}") %>% ymd(truncated = 1))

# ui

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "lista",
                  label = "Selecione uma infração:",
                  choices = isp %>% select(hom_doloso:apreensao_drogas_sem_autor) %>% names()
                  )
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# server

server <- function(input, output){
  output$plot <- renderPlotly({
    isp %>% 
      plot_ly(x = ~data, y = ~get(input$lista), mode = "line") %>% 
      layout(xaxis = list(title = "Data"),
             yaxis = list(title = "Taxa por 100.000 hab.")) %>% 
      rangeslider()
  })
}

# executar

shinyApp(ui = ui, server = server)