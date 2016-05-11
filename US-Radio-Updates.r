mainData <- NULL
subData <- NULL

#gets data.frame for date
library("rvest")
pullDayData <- function(date,station) {
  dateString <- gsub("-","",as.character.Date(date))
  url <- paste("http://kworb.net/radio/",station,"/archives/",dateString,".html",sep="")
  dayDT <- url %>% read_html() %>% html_nodes(xpath="/html/body/table") %>% html_table()
  dayDT <- dayDT[[1]]
  return(dayDT)
}

#writes CSV files for new dates
library("RCurl")
pullNewData <- function(station) {
  date <- Sys.Date()
  while(TRUE) {
    dateString <- gsub("-","",as.character.Date(date))
    if(!url.exists(paste("http://kworb.net/radio/",station,"/archives/",dateString,".html",sep=""))) {
      print(paste("Date for",date,"is not yet available"))
      date <- date-1
      next
    }
    if(file.exists(paste("data/",station,"/",date,".csv",sep=""))) break;
    
    write.csv(pullDayData(date,station), file=paste("data/",station,"/",date,".csv",sep=""), row.names=FALSE)
    print(paste(date,"for",station,"added"))
    date <- date-1
  }
  print("done")
}

todayDataExists <- function() {
  dateString <- gsub("-","",as.character.Date(Sys.Date()))
  if(url.exists(paste("http://kworb.net/radio/pop/archives/",dateString,".html",sep=""))) {
    return(TRUE)
  }
  return(FALSE)
}

loadData <- function(start, end, station,cats="Spins") {
  days <- as.numeric(end-start+1)
  date <- end
  main <- read.csv(paste("data/",station,"/",date,".csv",sep=""))
  main <- main[c("Title","Artist",cats)]
  #5/12/2011 to 6/22/2012 2 col is "Artist and Title", with Artist data ALL CAPS
  #6/23/2012 to Now 2,3 cols are "Artist","Title"
  
  #5/12/2011 to 7/18/2011 no "iTunes" col
  #7/19/2011 to Now "iTunes" col between "Days" and "PkPos"
  
  colnames(main)[3] <- as.character.Date(date)
  
  for(i in 2:days) {
    date <- date-1
    temp <- read.csv(paste("data/",station,"/",date,".csv",sep=""))
    temp <- temp[c("Title","Artist",cats)]
    colnames(temp)[3] <- as.character.Date(date)
    main <- merge(main, temp, by=c("Title","Artist"),all=TRUE)
    colnames(main)[ncol(main)] <- as.character.Date(date)
  }
  
  main  <- main[order(main[3], decreasing = TRUE),]
  
  return(main)
}

song2df <- function(title, start, end, data) {
  data <- data[,-2]
  days <- as.numeric(end-start+1)
  date <- as.Date.character(colnames(data)[2:(days+1)])
  spins <- as.integer(t(data[data$Title == title,2:(days+1)]))
  
  df <- data.frame(title,date,spins)
  #note: factors in df are determined alphabetically

  return(df)
}

songs2df <- function(titles, start, end, data) {
  df <- NULL
  
  for(title in titles) {
    temp <- song2df(title,start,end,data)
    df <- rbind(df,temp)
  }
  colnames(df) <- c("Title","Date","Spins")
  
  return(df)
}

parseSongText <- function(x) {
  x <- gsub("\\s","",x)
  x <- gsub(","," ",x)
  x2 <- strsplit(x," ")
  x2 <- x2[[1]]
  z <- NULL
  for(i in x2) {
    x3 <- strsplit(x2,"-")
    for(j in x3) {
      if(length(j) != 1) {
        for(k in as.numeric(j[1]):as.numeric(j[2])) {
          z <- c(z,as.numeric(k))
        }
      }
      else {
        z <- c(z,as.numeric(j))
      }
    }
  }
  sort(z,decreasing = FALSE)
  
  return(z)
}

main <- function() {
  pullNewData("pop")
  pullNewData("hac")
  pullNewData("rhythmic")
  pullNewData("urban")
  assign("mainData",loadData(Sys.Date()-180,Sys.Date()-!todayDataExists(),station="pop"), envir=.GlobalEnv)
  #change above line to as.Date("2015-01-01")
}

main()

#top 5 songs in the last 30 days
demo <- function() {
  assign("mainData",loadData(start=(Sys.Date()-30),end=Sys.Date(),station="pop",cats="Spins"), envir=.GlobalEnv)
  assign("subData",mainData[,c("Title",as.character(as.Date(Sys.Date():(Sys.Date()-30),origin="1970-01-01")))], envir=.GlobalEnv)

  numSongs <- 5
  
  df <- NULL
  for(i in 1:numSongs) {
    temp <- song2df(mainData[i,1], (Sys.Date()-30), Sys.Date(), mainData)
    df <- rbind(df,temp)
  }
  
  colnames(df) <- c("Title","Date","Spins")
  library(ggplot2)
  p <- ggplot() + geom_line(data=df, aes(x=Date,y=Spins,col=Title)) + ggtitle("Current Top 5 Songs")
  print(p)
  
}

standAlone <- function() {
  library(rCharts)
  
  assign("subData",mainData[,c("Title",as.character(Sys.Date():(Sys.Date()-30),origin="1970-01-01"))], envir=.GlobalEnv)
  assign("subData",subData[rowSums(is.na(subData)) != (ncol(subData)-1),], envir=.GlobalEnv)
  assign("subData",subData[order(subData[2], decreasing = TRUE),])
  titles <- as.character(subData$Title)
  
  df <- songs2df(titles,(Sys.Date()-30),Sys.Date(),subData)
  
  n1 <- nPlot(Spins ~ Date,
              group = "Title",
              data = df,
              type = "lineChart")
  n1$xAxis(axisLabel = "Dates",
           tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date( (d+1) * 86400000 ));}!#",
           rotateLabels = -45)
  n1$yAxis(axisLabel = "Spins") #, width=62)
  #n1$set(width = .01*input$setWidth*session$clientData$output_plot1_width,
   #      height = .01*input$setHeight*session$clientData$output_plot1_width)
  n1$chart(margin=list(left = 80,bottom = 100))
  n1$save('myplot.html')
}

