#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(leaflet)

salesData <- read.csv(file = "vgsales.csv", header = TRUE)
salesData[salesData == "N/A"]  <- NA
salesData0 <- na.omit(salesData)

ui <- dashboardPage(
  dashboardHeader(title = "Video Game Sales"),
  
  dashboardSidebar(
    sidebarMenu(
      
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  dashboardBody(
    box(title = "Main Data Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("mainDataTable")),
    box(title = "Main Data Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("pubGrossSalesTable"))
  ) # end dashboardBody
) # end dashBoardPage

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$mainDataTable <- DT::renderDataTable({
    t <- salesData0
    
    DT::datatable(t, options = list(pageLength = 12, lengthChange = FALSE, searching = FALSE))
  })
  
  output$pubGrossSalesTable <- DT::renderDataTable({
    t <- group_by(salesData0, Publisher)
    t <- summarise(t, Gross = sum(Global_Sales))
    t <- arrange(t, desc(Gross))
    
    DT::datatable(t, options = list(pageLength = 12, lengthChange = FALSE, searching = FALSE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

