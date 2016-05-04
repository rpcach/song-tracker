library(shiny)
source("US-Radio-Updates.r")

ui <- fluidPage(
  dateRangeInput(inputId = "range",
                 label = "Date Range",
                 start = (Sys.Date()-30),
                 end = Sys.Date(),
                 min = "2011-05-12",
                 max = Sys.Date()),
  uiOutput(outputId = "songs"),
  plotOutput("songSpinPlot")
)

server <- function(input,output) {
  output$songs <- renderUI({
    mainData <- loadData(input$range[1],input$range[2])
    titles <- levels(mainData$Title)
    checkboxGroupInput(inputId = "songs2",
                       label = "SongsX",
                       choices = titles)
  })
  output$songSpinPlot <- reactivePlot(function() {
    print(input$range)
  })
}

shinyApp(ui = ui, server = server)