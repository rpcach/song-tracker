library(shiny)
#source("US-Radio-Updates.r")

ui <- fluidPage(
  dateRangeInput(inputId = "range",
                 label = "Date Range",
                 start = (Sys.Date()-30),
                 end = Sys.Date(),
                 min = "2011-05-12",
                 max = Sys.Date()),
  checkboxGroupInput(inputId = "songs",
                     label = "Songs",
                     choices = c("alpha","beta","gamma"),
                     selected = "alpha"
                     ),
  plotOutput("songSpinPlot")
)

server <- function(input,output) {
  output$songSpinPlot <- reactivePlot(function() {
    
  })
}

shinyApp(ui = ui, server = server)