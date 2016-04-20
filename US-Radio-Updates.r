mainData <- NULL

library("rvest")
#gets data.frame for date
pullDayDataFrame <- function(date) {
  dateString <- gsub("-","",as.character.Date(date))
  url <- paste("http://kworb.net/radio/pop/archives/",dateString,".html", sep="")
  dayDataTable <- url %>% read_html() %>% html_nodes(xpath="/html/body/table") %>% html_table()
  dayDataTable <- dayDataTable[[1]]
  return(dayDataTable)
}

#creates CSV files for new dates
pullNewData <- function() {
  #setwd("radio/radio-data")
  date <- Sys.Date()
  while(TRUE) {
    if(file.exists(paste(date,".csv",sep=""))) break;
    
    write.csv(pullDayDataFrame(date), file=paste(date,".csv",sep=""), row.names=FALSE)
    date <- date-1
  }
}

plotSong <- function(title, days) {
  date <- Sys.Date()
  mainData <- read.csv(paste(date,".csv",sep=""))
  mainData <- mainData[c("Title","Spins")]
  colnames(mainData)[2] <- as.character.Date(date)
  #setwd("radio/radio-data") #must be in directory with CSV files
  for(i in 2:days) {
    date <- date-1
    temp <- read.csv(paste(date,".csv",sep=""))
    mainData <- merge(mainData, temp[c("Title","Spins")], by="Title")
    colnames(mainData)[ncol(mainData)] <- as.character.Date(date)
  }
  mainData <- mainData[,c("Title",names(mainData[(days+1):2]))]
  plot(as.numeric(mainData[mainData$Title == title,2:(days+1)]), type="l")
  #axis(1, at=1:TOTALDAYS,labels=colnames(mainData[2:(days+1)]), las=2)
}
plotSong("Pillowtalk",50)

####################################
library("rvest")
TOTALDAYS <- 30

date <- Sys.Date()
mainData <- getDayDataTable(date)
mainData <- mainData[c("Title","Spins")]
colnames(mainData)[2] <- format(date, format="%m-%d")

for(i in 2:TOTALDAYS) {
  date <- date-1
  tempData <- getDayDataTable(date)
  tempData <- tempData[c("Title","Spins")]
  colnames(tempData)[2] <- format(date, format="%m-%d")
  
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