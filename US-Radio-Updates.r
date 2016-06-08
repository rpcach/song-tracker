mainData <- NULL
subData <- NULL

#gets data.frame for date (Title, Artist, Spins)
library("rvest")
pullDayData <- function(date,station) {
  dateString <- gsub("-","",as.character.Date(date))
  url <- paste("http://kworb.net/radio/",station,"/archives/",dateString,".html",sep="")
  temp <- url %>% read_html() %>% html_nodes(xpath="/html/body/table") %>% html_table()
  temp <- temp[[1]]
  temp <- temp[c("Title","Artist","Spins")]
  colnames(temp)[3] <- as.character.Date(date)
  return(temp)
}

#adds new data to RDS files
pullNewData <- function(station) {
  date <- Sys.Date()-!todayDataExists()
  main <- readRDS(paste("data/",station,".rds",sep=""))

  while(date > as.Date(colnames(main)[3])) {
    print(paste(colnames(main)[3]))
    
    temp <- pullDayData((as.Date(colnames(main)[3])+1),station)
    main <- merge(temp, main, by=c("Title","Artist"), all=TRUE)
    
  }
  
  main  <- main[order(main[3], decreasing = TRUE),]
  saveRDS(main,paste("data/",station,".rds",sep=""))
  print(paste(station,"done"))
}

library("RCurl")
todayDataExists <- function() {
  dateString <- gsub("-","",as.character.Date(Sys.Date()))
  if(url.exists(paste("http://kworb.net/radio/pop/archives/",dateString,".html",sep=""))) {
    return(TRUE)
  }
  return(FALSE)
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

getData <- function(station) {
  date <- Sys.Date()
  main <- pullDayData(date,station)
  print(as.character(date))
  date <- date-1
  while(date >= as.Date("2015-01-01")) {
    print(as.character(date))
    temp <- pullDayData(date,station)
    main <- merge(main, temp, by=c("Title","Artist"), all=TRUE) # main, temp or temp, main?
    date <- date-1
  }
  main  <- main[order(main[3], decreasing = TRUE),]
  
  saveRDS(main,paste("data/",station,".rds",sep=""))
  print(station)
}

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

