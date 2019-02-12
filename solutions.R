# Soluções dos exercícios

## básico -------------------------------------------------------------

### 1 -----------------------------------------------------------------

require(magrittr)

BOD %$% plot(Time, demand, type = "b", col = "blue", 
             lwd = 3, pch = 19,
             xlab = "Tempo", ylab = "Demanda",
             panel.first = {grid(lty = 1)}, axes = FALSE)
axis(1)
axis(2, las = 2)

### 2 -----------------------------------------------------------------

f <- function(x) sin(x)
g <- function(x) cos(x)

curve(f, -5, 5, col = "blue", ylab = "y", lwd = 2, axes = FALSE, panel.first = {grid(lty = 1)})
curve(g, col = "red", lwd = 2, add = TRUE)
legend(3, .7, c("sin(x)", "cos(x)"), col = c("blue", "red"), lty = 1, lwd = 2, bty = "n")
axis(1, pos = 0, at = seq(-6, 6, 2)[-4])
axis(2, pos = 0, las = 2, at = seq(-1, 1, .5)[-3])

### 3 -----------------------------------------------------------------

hist(WWWusage, col = "blue", border = FALSE)

## ggplot2 ------------------------------------------------------------

### 1 -----------------------------------------------------------------

require(dplyr)
require(ggplot2)
require(reshape2)

VADeaths %>% melt() %>%
  ggplot(aes(x = Var1, y = value, group = Var2, colour = Var2)) + 
  geom_line() +
  labs(x = "Faixa etária", y = "Taxa de mortalidade", colour = "")

### 2 -----------------------------------------------------------------

require(titanic)

titanic_train %>% 
  filter(Embarked != "") %>% 
  ggplot(aes(x = Embarked, y = ..count../sum(..count..))) + 
  geom_bar() +
  labs(y = "Percentual") +
  scale_y_continuous(labels = scales::percent)

### 3 -----------------------------------------------------------------

titanic::titanic_train %>% 
  mutate(Survived = factor(Survived, labels = c("Não", "Sim"))) %>% 
  filter(Embarked != "") %>% 
  ggplot(aes(x = Embarked, y = ..count../sum(..count..), fill = Survived)) + 
  geom_bar(position = "fill") +
  labs(y = "Percentual", fill = "Sobrevivente") +
  scale_y_continuous(labels = scales::percent)

### 4 -----------------------------------------------------------------

require(gtrendsR)
require(lubridate)

lula <- 
  gtrends("Lula", geo = "BR", time = "2018-01-01 2018-08-31")

lula$interest_over_time %>% 
  ggplot(aes(date, hits)) + 
  geom_line() + 
  geom_text(aes(ymd_hms("2018-01-24 00:00:00"), 110, label = "Julgamento")) +
  geom_text(aes(ymd_hms("2018-04-07 00:00:00"), 110, label = "Se entregou")) +
  geom_text(aes(ymd_hms("2018-07-08 00:00:00"), 110, label = "Habeas Corpus"))

## plotly -------------------------------------------------------------

### 1 -----------------------------------------------------------------

require(plotly)

titanic_train %>% 
  filter(Embarked != "") %>% 
  group_by(Embarked) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n/sum(n) * 100) %>% 
  plot_ly(labels = ~Embarked, values = ~perc, 
          type = "pie", 
          hoverinfo = "percent+label")

### 2 -----------------------------------------------------------------

titanic_train %>% 
  mutate(Survived = factor(Survived, labels = c("Não", "Sim"))) %>% 
  plot_ly(x = ~as.factor(Survived), y = ~Age, type = "box") %>% 
  layout(xaxis = list(title = "Sobrevivente"))

### 3 -----------------------------------------------------------------

airquality %>% 
  melt(id = c("month", "day"), na.rm = TRUE) %>% 
  filter(variable == "temp") %>% 
  acast(day ~ month) %>% 
  plot_ly(x = 5:9, y = 1:31, 
          z = ., type = "heatmap") %>% 
  config(displayModeBar = FALSE) %>% 
  layout(xaxis = list(title = "Meses"),
         yaxis = list(title = "Dias"))

## shiny --------------------------------------------------------------

### 1 -----------------------------------------------------------------

require(shiny)
require(glue)

# ui

ui <- fluidPage(
      sidebarPanel(
      titlePanel("Formulário"),
      textInput(inputId = "nome", 
                label = "Qual é o seu nome?"),
      numericInput(inputId = "idade", 
                   label = "Qual a sua idade?", 
                   value = 0, min = 0, max = 99),
      radioButtons(inputId = "sexo", 
                   label = "Qual o seu sexo?",
                   c("Masculino" = "M",
                     "Feminino" = "F"))
    ),
    mainPanel(
      tags$h1(textOutput("texto"))
    )
)

# server

server <- function(input, output){
  
  output$texto <- renderText({
    glue("{ifelse(input$sexo == 'M', 'Querido', 'Querida')} {input$nome}, daqui a 2 anos você terá {input$idade + 2} anos de idade.")
  }) 
  
}

# executar

shinyApp(ui = ui, server = server)
