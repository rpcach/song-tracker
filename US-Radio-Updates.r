mainData <- NULL

#gets data.frame for date
pullDayData <- function(date) {
  library("rvest")
  dateString <- gsub("-","",as.character.Date(date))
  url <- paste("http://kworb.net/radio/pop/archives/",dateString,".html", sep="")
  dayDT <- url %>% read_html() %>% html_nodes(xpath="/html/body/table") %>% html_table()
  dayDT <- dayDT[[1]]
  return(dayDT)
}

#writes CSV files for new dates
pullNewData <- function() {
  date <- Sys.Date()
  while(TRUE) {
    if(file.exists(paste("data/",date,".csv",sep=""))) break;
    
    write.csv(pullDayData(date), file=paste(date,".csv",sep=""), row.names=FALSE)
    print(paste(date,"added"))
    date <- date-1
  }
  print("done")
}

loadData <- function(days) {
  date <- Sys.Date()
  main <- read.csv(paste("data/",date,".csv",sep=""))
  main <- main[c("Title","Spins")]
  #5/12/2011 to 6/22/2012 2 col is "Artist and Title", with Artist data ALL CAPS
  #6/23/2012 to Now 2,3 cols are "Artist","Title"
  
  #5/12/2011 to 7/18/2011 no "iTunes" col
  #7/19/2011 to Now "iTunes" col between "Days" and "PkPos"
  
  colnames(main)[2] <- as.character.Date(date)
  
  for(i in 2:days) {
    date <- date-1
    temp <- read.csv(paste("data/",date,".csv",sep=""))
    temp <- temp[c("Title","Spins")]
    colnames(temp)[2] <- as.character.Date(date)
    main <- merge(main, temp, by="Title")
    colnames(main)[ncol(main)] <- as.character.Date(date)
  }

  return(main)
}

song2df <- function(title,days) {
  date <- as.Date.character(colnames(mainData)[2:(days+1)])
  spins <- as.integer(t(mainData[mainData$Title == title,2:(days+1)]))
  
  df <- as.data.frame(cbind(date,spins))
  df[,1] <- as.Date(df[,1], origin="1970-01-01")

  return(df)
}

plotSong <- function(title, days) {
  df <- song2df(title,days)

  library(ggplot2)
  ggplot(df,aes(x=date,y=spins)) + geom_line()
}

demo <- function() {
  assign("mainData",loadData(17), envir=.GlobalEnv)
  plotSong(as.character(as.character(mainData[1,1])),17)
}

demo()

####################################
library("rvest")
TOTALDAYS <- 30

setwd("radio/radio-data") #must be in directory with CSV files
date <- Sys.Date()
mainData <- read.csv(paste(date,".csv",sep=""))
mainData <- mainData[c("Title","Spins")]
colnames(mainData)[2] <- as.character.Date(date)

for(i in 2:TOTALDAYS) {
  date <- date-1
  tempData <- read.csv(paste(date,".csv",sep=""))
  tempData <- tempData[c("Title","Spins")]
  colnames(tempData)[2] <- as.character.Date(date)
  
  mainData <- merge(mainData, tempData, by="Title")
}

mainData <- mainData[order(mainData[2], decreasing = TRUE),]
mainDataMatrix <- data.matrix(mainData[-1])

persp(x = 1:nrow(mainData), y = 1:TOTALDAYS, z = mainDataMatrix, phi=40, theta=10)

library("plot3D")
persp3D(x = 1:nrow(mainData), y = 1:TOTALDAYS, z = mainDataMatrix)
#size of matrix must be equal to x-by-y in persp function

library("rgl")
persp3d(x = 1:nrow(mainData), y = 1:TOTALDAYS, z = mainDataMatrix, col=rainbow(1000), xlab="Song Position", ylab="Time Ago in Days", zlab="Spins")
#browseURL(paste("file://", writeWebGL(dir=file.path("radio", "webGL"), width=700), sep=""))
#creates html file holding interactable 3d-plot

barplot(mainData[[2]], col=rainbow(50))

library(data.table)
setcolorder(mainData, c("Title", names(mainData[(TOTALDAYS+1):2])))
#reverses the order of the date columns

plot(as.numeric(mainData[1,2:(TOTALDAYS+1)]), type="l", xlab="Date", ylab="Spins")

plot(as.numeric(mainData[1,2:(TOTALDAYS+1)]), type="o", xaxt="n", xlab="Date", ylab="Spins")
axis(1, at=1:TOTALDAYS,labels=colnames(mainData[2:(TOTALDAYS+1)]), las=2)
lines(as.numeric(mainData[2,2:(TOTALDAYS+1)]), type="o")

plotSong <- function(title) {
  plot(as.numeric(mainData[mainData$Title == title,2:(TOTALDAYS+1)]), type="o")
  axis(1, at=1:TOTALDAYS,labels=colnames(mainData[2:(TOTALDAYS+1)]), las=2)
}

plotSong("Cake By The Ocean")
