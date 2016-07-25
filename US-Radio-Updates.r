mainData <- NULL
subData <- NULL

#gets data.frame for date (Title, Artist, Spins)
library("rvest")
pullDayData <- function(date,station) {
  dateString <- gsub("-","",as.character.Date(date))
  url <- paste("http://kworb.net/radio/",tolower(station),"/archives/",dateString,".html",sep="")
  temp <- url %>% read_html() %>% html_nodes(xpath="/html/body/table") %>% html_table()
  temp <- temp[[1]]
  temp <- temp[c("Title","Artist","Spins")]
  colnames(temp)[3] <- as.character.Date(date)
  return(temp)
}

#adds new data to RDS files
#needs to be made much faster...
#edit: it has been made much faster??
pullNewData <- function(stations) {
  date <- Sys.Date()-!todayDataExists()

  for(station in stations) {
    main <- readRDS(paste("data/",station,".rds",sep=""))
    latestDate <- as.Date(colnames(main[3]))
    if(date > latestDate) {
      combined <- pullDayData((as.Date(colnames(main)[3])+1),station)
      combined <- combined[,1:2]
      while(date > latestDate) {
        temp <- pullDayData((latestDate+1),station)
        combined <- merge(temp, combined, by=c("Title","Artist"), all=TRUE)
        #main <- merge(temp, main, by=c("Title","Artist"), all=TRUE)
        latestDate <- latestDate+1
      }
      main <- merge(combined,main, by=c("Title","Artist"), all=TRUE)
      main  <- main[order(main[3], decreasing = TRUE),]
    }
    
    saveRDS(main,paste("data/",station,".rds",sep=""))
    print(paste(station,"station has been updated"))
  }

}

library("RCurl")
todayDataExists <- function() {
  return(url.exists(paste("http://kworb.net/radio/pop/archives/",gsub("-","",Sys.Date()),".html",sep="")))
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

multiStationDF <- function(title, start, end) {
  df <- NULL
  temp <- NULL

  for (station in c("pop","hac","rhythmic","urban","alternative")) {
    main <- readRDS(paste("data/",station,".rds",sep=""))
    sub <- main[,c("Title","Artist",as.character(as.Date(end:start,origin="1970-01-01")))]
    sub <- sub[rowSums(is.na(sub)) != (ncol(sub)-2),]
    sub <- sub[order(sub[3], decreasing = TRUE),]
    
    if(title %in% sub$Title) {
      temp <- songs2df(title, start, end, sub)
      levels(temp$Title) <- station  
      df <- rbind(df,temp)
    }
  }

  return(df)
}

parseSongText <- function(x, titles, artists) {
  x <- strsplit(x,",")
  x <- x[[1]]
  x <- gsub("^\\s+|\\s+$", "", x)
  temp <- NULL

  for(i in x) {
    if(i == "") next
    else if(substr(i,1,3) == "by ") {
      #temp <- c(temp,which(tolower(strtrim(artists,(nchar(i)-3))) == tolower(substr(i,4,nchar(i)))))
      temp <- c(temp,which(tolower(artists) == tolower(substr(i,4,nchar(i)))))
    }
    else if(grepl("[a-zA-Z]",i)) {
      #temp <- c(temp,as.numeric(which(tolower(strtrim(titles,nchar(i))) == tolower(i))))
      temp <- c(temp,as.numeric(which(gsub(" f/.*","",tolower(titles)) == tolower(i))))
    }
    else {
      i <- gsub("\\s","",i)
      i <- strsplit(i,"-")
      for(j in i) {
        temp <- c(temp,as.numeric(j[1]:as.numeric(j[length(j)])))
      }
    }
  }
  
  sort(temp,decreasing = FALSE)
  return(temp)
}

getData <- function(station) {
  date <- Sys.Date()-2
  #main <- pullDayData(date,station)
  #print(as.character(date))
  #date <- date-1
  while(date >= as.Date("2012-06-23")) {
    #if(url.exists(paste("http://kworb.net/radio/alternative/archives/",gsub("-","",date),".html",sep=""))) {
    print(as.character(date))
    temp <- pullDayData(date,station)
    main <- merge(main, temp, by=c("Title","Artist"), all=TRUE) # main, temp or temp, main?
    #}
    date <- date-1
  }
  main  <- main[order(main[3], decreasing = TRUE),]
  
  saveRDS(main,paste("data/",station,".rds",sep=""))
  print(station)
}

main <- function() {
  if(readRDS("data/latest.rds") != {latest <- (Sys.Date()-!todayDataExists())}) {
    pullNewData(c("pop","hac","rhythmic","urban","alternative"))
    saveRDS(latest,"data/latest.rds")
  }
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

