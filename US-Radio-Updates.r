TOTALDAYS <- 30

library("rvest")
getDayDataTable <- function(date) {
  dateString <- gsub("-","",as.character.Date(date))
  url <- paste("http://kworb.net/radio/pop/archives/",dateString,".html", sep="")
  dayDataTable <- url %>% read_html() %>% html_nodes(xpath="/html/body/table") %>% html_table()
  dayDataTable <- dayDataTable[[1]]
  return(dayDataTable)
}

dataToCSV <- function(date) {
  temp <- getDayDataTable(date)
  write.csv(temp, file=paste(date,".csv",sep=""))
}

updateDataCSV <- function() {
  date <- Sys.Date()
  while(TRUE) {
    if(file.exists(paste(date,".csv",sep=""))) {
      break
    }
    else {
      dataToCSV(date)
    }
    date <- date-1
  }
}

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