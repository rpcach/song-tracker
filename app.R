library(shiny)
library(rCharts)
source("US-Radio-Updates.r")

ui <- fluidPage(
  titlePanel("Song Tracker"),
  sidebarLayout(
   sidebarPanel(
     uiOutput(outputId = "station2"),
     selectInput(inputId = "station",
                 label = "Station",
                 choices = c("Pop","HAC","Rhythmic","Urban","Alternative"),
                 selected = "Pop"),
     dateRangeInput(inputId = "date_range",
                    label = "Date Range",
                    #start = (Sys.Date()-30),
                    #end = Sys.Date()-!todayDataExists(),
                    #min = as.Date("2012-06-23"),
                    #max = Sys.Date()-!todayDataExists()),
                    start = '2018-11-19',
                    end = '2018-12-18',
                    min = '2012-06-23',
                    max = '2018-12-18'),
     textInput(inputId = "song_selection",
               label = "Select songs here:",
               value = "1-5"),"i.e. 1-5,10,Gangnam Style,by Drake",
     uiOutput(outputId = "song_titles_artists"),
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
  output$station2 <- renderUI({
    
  })
  output$song_titles_artists <- renderUI({
    assign("mainData",readRDS(paste("data/",tolower(input$station),".rds",sep="")))

    assign("subData",mainData[,c("Title","Artist",as.character(as.Date(input$date_range[2]:input$date_range[1],origin="1970-01-01")))], envir=.GlobalEnv)
    assign("subData",subData[rowSums(is.na(subData)) != (ncol(subData)-2),], envir=.GlobalEnv)
    assign("subData",subData[order(subData[3], decreasing = TRUE),])
    
    titles <- subData$Title
    artists <- subData$Artist
    
    titles_artists <- paste(1:nrow(subData),". ",titles,", by ",artists,sep="")

    checkboxGroupInput(inputId = "song_titles_artists",
                       label = paste(nrow(subData),"songs available"),
                       choices = titles_artists,
                       selected = titles_artists[parseSongText(input$song_selection,titles,artists)])
  })
  output$nChart <- renderChart2({
    titles <- input$song_titles_artists
    for(i in 1:length(titles)) {
      newTitle <- gsub("^[0-9]*. ","",titles[i])
      newTitle <- gsub(", by.*$","",newTitle)
      titles[i] <- newTitle
    }

    # OR
    #
    #values <- subData$Title[as.numeric(gsub(" .*$","",titles))]
    #titles <- values
    #
    # OR
    #
    #titles <- subData$Title[parseSongText(input$song_selection,subData$Title,subData$Artist)]
    
    
    #df <- songs2df(titles,input$date_range[1],input$date_range[2],subData)
    
    # if(length(titles) == 0) {
    #   df <- songs2df(subData$Title[1],input$date_range[1],input$date_range[2],subData)
    # }
    # else 
    if (length(titles) > 1) {
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