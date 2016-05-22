library(shiny)
library(rCharts)
source("US-Radio-Updates.r")

ui <- fluidPage(
  titlePanel("Song Tracker"),
  sidebarLayout(
   sidebarPanel(
     selectInput(inputId = "genre",
                 label = "Genre",
                 choices = c("Pop","HAC","Rhythmic","Urban","Alternative"),
                 selected = "Pop"),
     dateRangeInput(inputId = "range",
                    label = "Date Range",
                    start = (Sys.Date()-30),
                    end = Sys.Date()-!todayDataExists(),
                    min = as.Date("2015-01-01"),
                    max = Sys.Date()-!todayDataExists()),
     textInput(inputId = "songSelectText",
               label = "Select songs here:",
               value = "1-5"),
     uiOutput(outputId = "songTitles"),
     sliderInput(inputId = "setWidth",
                 label = "Width %",
                 value = 100, min = 50, max = 100),
     sliderInput(inputId = "setHeight",
                 label = "Height %",
                 value = 75, min = 50, max = 150)
   ),
   mainPanel(
     plotOutput("plot1", height = "1px"),
     showOutput("nChart","nvd3")
   )
  )
  
)

server <- function(input,output,session) {
  output$songTitles <- renderUI({
    pullNewData(tolower(input$genre))
    assign("mainData",loadData(Sys.Date()-180,Sys.Date()-!todayDataExists(),station=tolower(input$genre)), envir=.GlobalEnv)
    assign("subData",mainData[,c("Title","Artist",as.character(as.Date(input$range[2]:input$range[1],origin="1970-01-01")))], envir=.GlobalEnv)
    assign("subData",subData[rowSums(is.na(subData)) != (ncol(subData)-2),], envir=.GlobalEnv)
    assign("subData",subData[order(subData[3], decreasing = TRUE),])
    titles <- as.character(subData$Title)
    artists <- as.character(subData$Artist)

    for(i in 1:length(titles)) {
      titles[i] <- paste(i,". ",titles[i],", by ",artists[i],sep="")
    }

    checkboxGroupInput(inputId = "songs2",
                       label = paste(length(titles),"songs available"),
                       choices = titles,
                       selected = titles[parseSongText(input$songSelectText)])
  })
  output$nChart <- renderChart2({
    titles <- input$songs2
    for(i in 1:length(titles)) {
      newTitle <- gsub("^[0-9]*. ","",titles[i])
      newTitle <- gsub(", by.*$","",newTitle)
      titles[i] <- newTitle
    }
    
    df <- songs2df(titles,input$range[1],input$range[2],subData)

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