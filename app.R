library(shiny)
source("US-Radio-Updates.r")

ui <- fluidPage(
  titlePanel("Song Tracker"),
  sidebarLayout(
   sidebarPanel(
     dateRangeInput(inputId = "range",
                    label = "Date Range",
                    start = (Sys.Date()-30),
                    end = Sys.Date(),
                    min = "2011-05-12",
                    max = Sys.Date()),
     uiOutput(outputId = "songs")
   ),
   mainPanel(
     plotOutput("songSpinPlot")
   )
  )
  
)

server <- function(input,output) {
  output$songs <- renderUI({
    assign("mainData",loadData(input$range[1],input$range[2]), envir=.GlobalEnv)
    titles <- as.character(mainData$Title)
    checkboxGroupInput(inputId = "songs2",
                       label = "SongsX",
                       choices = titles,
                       selected = titles[1:5])
  })
  output$songSpinPlot <- reactivePlot(function() {
    df <- songs2df(input$songs2,input$range[1],input$range[2])
    colnames(df) <- c("Title","Date","Spins")
    p <- ggplot() + geom_line(data=df, aes(x=Date,y=Spins,col=Title))
    print(p)
  })
}

shinyApp(ui = ui, server = server)