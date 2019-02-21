library(shiny)
library(shinydashboard)
library(tm)


ui<-dashboardPage(
    
  dashboardHeader(title = "Shiny Bots"),
  dashboardSidebar(
    
  ),
  dashboardBody(
    fluidRow(
      box(
        status = "primary",width = 8,solidHeader = TRUE,
        title = "Ingrese mensaje",
        textInput("texto",label = "Ingrese texto"),
        submitButton("Enviar"))
      
    ),
      fluidRow(
        box(
          status = "warning",solidHeader = TRUE,
          title = "Respuesta",
          textOutput("respuesta")
      )
      
      
    )))

