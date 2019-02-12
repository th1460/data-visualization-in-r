require(shiny)
require(DBI)
require(dplyr)
require(glue)
require(DT)
require(plotly)

# canal

conn <- dbConnect(RSQLite::SQLite(), dbname = "dados.sqlite")

if(!dbExistsTable(conn, "perfil")){
  
 dbWriteTable(conn, "perfil", value = tibble(Nome = character(),
                                             Idade = numeric(),
                                             Escolaridade = character(),
                                             Matematica = character(),
                                             Programa = character())) 
  
}

# ui

ui <- fluidPage(
    sidebarPanel(
      titlePanel("Formulário"),
      textInput(inputId = "nome", 
                label = "Qual é o seu nome?"),
      numericInput(inputId = "idade", 
                   label = "Qual a sua idade?", 
                   value = 0, min = 0, max = 99),
      selectInput(inputId = "escolar", 
                  label = "Qual a sua escolaridade?", 
                  choices = c("", "Médio", "Superior", "Pós"),  
                  multiple = F),
      checkboxInput(inputId = "mate", 
                    label = "Gosta de matemática?", 
                    value = F),
      sliderInput(inputId = "program", 
                  label = "Qual o seu nível em programação?", 
                  min = 0, 
                  max = 10, 
                  step = 1, 
                  value = 0),
      actionButton("enviar", "Enviar")
      ),
    mainPanel(
      tags$h1("Tabela com os dados de entrada"),
      dataTableOutput("table"),
      tags$h1("Alguns resultados descritivos"),
      plotlyOutput("plot1"),
      plotlyOutput("plot2")
      )
  )

# server

server <- function(input, output){
  
  dados <- reactive({
    data.frame(Nome = input$nome, 
               Idade = input$idade,
               Escolaridade = input$escolar,
               Matematica = ifelse(input$mate, "Sim", "Não"),
               Programa = input$program)
  })
  
  observeEvent(input$enviar, {
    dbWriteTable(conn, "perfil", value = dados(), append = TRUE)
    showNotification("Enviado",
                     action = a(href = "javascript:location.reload();", "Atualizar tabela")
    )
  })
  
  output$table <- renderDataTable({
    datatable(tbl(conn, "perfil") %>% as_tibble())
  })
  
  output$plot1 <- renderPlotly({
    tbl(conn, "perfil") %>% as_tibble() %>% 
      plot_ly(x = ~Matematica) %>% 
      layout(xaxis = list(title = "Gosta de matemática?"))
  })
  
  output$plot2 <- renderPlotly({
    tbl(conn, "perfil") %>% as_tibble() %>% 
      plot_ly(x = ~Escolaridade) %>% 
      layout(xaxis = list(title = "Qual a sua escolaridade?"))
  })
  
}

# executar

shinyApp(ui = ui, server = server)
