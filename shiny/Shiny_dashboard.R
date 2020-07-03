###First steps developing a shiny dashboard for statistical analysis and graphical analysis##
#installing and loading packages
pck <- c("shiny","ggplot2", "rsconnect","shinydashboard")
for (i in 1:length(pck)){
  if(pck[i] %in% rownames(installed.packages()) == FALSE) {install.packages(pck[i])}
}
lapply(pck, require, character.only = TRUE)

library(shiny)
library(ggplot2)
library(rsconnect)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "R Oberfläche"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Daten", tabName = "daten", icon = icon("dashboard")),
      menuItem("Statistische Tests", tabName = "t-test", icon = icon("th")),
      menuItem("Grafische Auswertung", tabName = "Boxpllot", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "daten",
              h2("Datenauswaelen"),
              h6("Herzlich Wilkommen im Graphic Manager
                 
                 bevor Sie Graphiken erstellen können benötigt der Graphic Creator einen Datensatz in Form einer CSV Datei.
                 Bitte waehlen Sie nun Ihre Daten aus, die sie visualisieren möchten.
                 Klicken Sie danach in der Sidebar auf die Art von Graphik die Sie erstellen möchten"),
              fluidPage(
                titlePanel("Daten auswaehlen"),
                  sidebarLayout(
                    sidebarPanel(
                      fileInput("file1", "CSV Datei Auswaehlen",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    tags$hr(),
                    checkboxInput("header", "Header", TRUE),
                    radioButtons("sep", "Trennzeichen",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    tags$hr(),
                    radioButtons("disp", "Display",
                                 choices = c(Head = "head",
                                             Alle = "all"),
                                 selected = "head")
                  ),
                  mainPanel(
                    tableOutput("contents")
                    )
                )
              )),
      tabItem(tabName = "Gafiken", h5("Histogramm"),  fluidPage(),
      tabItem(tabName = "Statistische Tests", h5("T-Test"),  fluidPage())
  ))
))

server <- function(input, output) {
    output$contents <- renderTable({
        req(input$file1)
        tryCatch({
        data <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
      },
      error = function(e) {
            stop(safeError(e))
      }
    )
    if(input$disp == "head") {
      return(head(data))
    }
    else {
      return(data)
    }
    
  })
  
}

shinyApp(ui, server)
