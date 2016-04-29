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

#creates CSV files for new dates
pullNewData <- function() {
  #setwd("radio/radio-data")
  date <- Sys.Date()
  while(TRUE) {
    if(file.exists(paste(date,".csv",sep=""))) break;
    
    write.csv(pullDayData(date), file=paste(date,".csv",sep=""), row.names=FALSE)
    print(paste(date,"added"))
    date <- date-1
  }
  print("done")
}

loadData <- function(days) {
  #setwd("radio-data")
  date <- Sys.Date()
  main <- read.csv(paste(date,".csv",sep="",select=cats))
  main <- main["Title"]
  colnames(main)[2] <- as.character.Date(date)
  
  for(i in 2:days) {
    date <- date-1
    temp <- read.csv(paste(date,".csv",sep=""))
    main <- merge(main, temp[c("Title","Spins")], by="Title")
    colnames(main)[ncol(mainData)] <- as.character.Date(date)
  }
  
  return(main)
}

plotSong <- function(title, days) {
  mainData <- loadData(days)
  mainData <- mainData[,c("Title",names(mainData[(days+1):2]))] #reverses date-col order
  plot(as.numeric(mainData[mainData$Title == title,2:(days+1)]), type="l", axes=FALSE,xaxt="n", yaxt="n",xlab="Date", ylab="Spins (Thousands)", main=title)
  xaxis <- colnames(mainData[2:(days+1)])
  for(i in 1:length(xaxis)) {
    xaxis[i] <- as.character(format(as.Date(xaxis[i]), format="%b-%d-%y"))
  }
  axis(1, at=c(1,seq(5,days,5)),labels=c(xaxis[1],xaxis[seq(5,length(xaxis),5)]), las=2)
  axis(2, at=seq(0,16000,1000), labels=seq(0,16,1),las=2)
}
plotSong("I Took A Pill In Ibiza",50)

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
