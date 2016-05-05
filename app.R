library(shiny)
library(rCharts)
source("US-Radio-Updates.r")

ui <- fluidPage(
  titlePanel("Song Spins Tracker"),
  sidebarLayout(
   sidebarPanel(
     dateRangeInput(inputId = "range",
                    label = "Date Range",
                    start = (Sys.Date()-30),
                    end = Sys.Date(),
                    min = (Sys.Date()-180), #"2011-05-12",
                    max = Sys.Date()),
     sliderInput(inputId = "setWidth",
                 label = "Set Graph Width %",
                 value = 100, min = 50, max = 100),
     sliderInput(inputId = "setHeight",
                 label = "Set Graph Height %",
                 value = 75, min = 50, max = 150),
     uiOutput(outputId = "songTitles")
   ),
   mainPanel(
     plotOutput("plot1", height = "1px"),
     showOutput("nChart","nvd3")
   )
  )
  
)

server <- function(input,output,session) {
  output$songTitles <- renderUI({
    assign("subData",mainData[,c("Title",as.character(as.Date(input$range[2]:input$range[1],origin="1970-01-01")))], envir=.GlobalEnv)
    assign("subData",subData[rowSums(is.na(subData)) != (ncol(subData)-1),], envir=.GlobalEnv)
    assign("subData",subData[order(subData[2], decreasing = TRUE),])
    titles <- as.character(subData$Title)
    
    checkboxGroupInput(inputId = "songs2",
                       label = "Songs w/ Spins",
                       choices = titles,
                       selected = titles[1:5])
  })
  output$nChart <- renderChart2({
    df <- songs2df(input$songs2,input$range[1],input$range[2],subData)

    n1 <- nPlot(Spins ~ Date,
                group = "Title",
                data = df,
                type = "lineChart")
    n1$xAxis(axisLabel = "Dates",
             tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date( (d+1) * 86400000 ));}!#",
             rotateLabels = -45)
    n1$yAxis(axisLabel = "Spins") #, width=62)
    n1$set(width = .01*input$setWidth*session$clientData$output_plot1_width,
           height = .01*input$setHeight*session$clientData$output_plot1_width)
    n1$chart(margin=list(left = 80,bottom = 100))
    return(n1)
    
  })
}

shinyApp(ui = ui, server = server)