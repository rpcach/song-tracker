library(shiny)
library(rCharts)
source("US-Radio-Updates.r")

ui <- fluidPage(
  titlePanel("Song Tracker"),
  sidebarLayout(
   sidebarPanel(
     selectInput(inputId = "station",
                 label = "Station",
                 choices = c("Pop","HAC","Rhythmic","Urban"),
                 selected = "Pop"),
     dateRangeInput(inputId = "date_range",
                    label = "Date Range",
                    start = (Sys.Date()-30),
                    end = Sys.Date()-!todayDataExists(),
                    min = as.Date("2015-01-01"),
                    max = Sys.Date()-!todayDataExists()),
     textInput(inputId = "song_selection",
               label = "Select songs here:",
               value = "1-5"),
     uiOutput(outputId = "song_titles"),
     sliderInput(inputId = "width",
                 label = "Width %",
                 value = 100, min = 50, max = 100),
     sliderInput(inputId = "height",
                 label = "Height %",
                 value = 75, min = 50, max = 150)
   ),
   mainPanel(
     plotOutput("dummy", height = "1px"),
     showOutput("nChart","nvd3")
   )
  )
  
)

server <- function(input,output,session) {
  output$song_titles <- renderUI({
    assign("mainData",readRDS(paste("data/",input$station,".rds",sep="")))

    assign("subData",mainData[,c("Title","Artist",as.character(as.Date(input$date_range[2]:input$date_range[1],origin="1970-01-01")))], envir=.GlobalEnv)
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
                       selected = titles[parseSongText(input$song_selection,subData$Title,subData$Artist)])
  })
  output$nChart <- renderChart2({
    titles <- input$songs2
    for(i in 1:length(titles)) {
      newTitle <- gsub("^[0-9]*. ","",titles[i])
      newTitle <- gsub(", by.*$","",newTitle)
      titles[i] <- newTitle
    }
    
    #values <- subData$Title[as.numeric(gsub(" .*$","",titles))]
    #titles <- values
    #
    #titles <- subData$Title[parseSongText(input$song_selection,subData$Title,subData$Artist)]
    
    
    #df <- songs2df(titles,input$date_range[1],input$date_range[2],subData)
    
    if(length(titles) == 0) {
      df <- songs2df(subData$Title[1:5],input$date_range[1],input$date_range[2],subData)
    }
    else if (length(titles) > 1) {
      df <- songs2df(titles,input$date_range[1],input$date_range[2],subData)
    }
    else { # == 1
      df <- multiStationDF(titles,input$date_range[1],input$date_range[2])
    }
    
    n1 <- nPlot(Spins ~ Date,
                group = "Title",
                data = df,
                type = "lineChart")
    n1$xAxis(axisLabel = "Dates",
             tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date( (d+1) * 86400000 ));}!#",
             rotateLabels = -45)
    n1$yAxis(axisLabel = "Spins") #, width=62)
    n1$set(width = .01*input$width*session$clientData$output_dummy_width,
           height = .01*input$height*session$clientData$output_dummy_width)
    if (length(titles) == 1) {
      n1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
      #n1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      n1$set(title = titles)
    }
    n1$chart(margin=list(left = 80,bottom = 100))
    return(n1)
    
  })
}

shinyApp(ui = ui, server = server)