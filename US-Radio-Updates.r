library("rvest")
TOTALDAYS <- 3

getDayDataTable <- function(date) {
  dateString <- gsub("-","",as.character.Date(date))
  url <- paste("http://kworb.net/radio/pop/archives/",dateString,".html", sep="")
  dayDataTable <- url %>% read_html() %>% html_nodes(xpath="/html/body/table") %>% html_table()
  dayDataTable <- dayDataTable[[1]]
  return(dayDataTable)
}

date <- Sys.Date()
mainData <- getDayDataTable(date)
keeps <- c("Title", "Spins")
mainData <- mainData[keeps]
colnames(mainData)[2] <- as.character.Date(date)

for(i in 2:TOTALDAYS) {
  date <- date-1
  tempData <- getDayDataTable(date)
  tempData <- tempData[c("Title","Spins")]
  colnames(tempData)[2] <- as.character.Date(date)
  
  mainData <- merge(mainData, tempData, by="Title")
}

date <- Sys.Date()
dateString <- as.character.Date(date)

mainData <- mainData[order(mainData[[dateString]], decreasing = TRUE),]
mainDataMatrix <- data.matrix(mainData[-1])
persp(x = 1:nrow(mainData), y = 1:TOTALDAYS, z = mainDataMatrix, phi=40, theta=10)

library("plot3D")
persp3D(x = 1:nrow(mainData), y = 1:TOTALDAYS, z = mainDataMatrix)
#size of matrix must be equal to x-by-y in persp function

library("rgl")s
persp3d(x = 1:nrow(mainData), y = 1:TOTALDAYS, z = mainDataMatrix, col=rainbow(1000), xlab="Song Position", ylab="Time Ago in Days", zlab="Spins")
#browseURL(paste("file://", writeWebGL(dir=file.path("radio", "webGL"), width=700), sep=""))
#creates html file holding interactable 3d-plot

barplot(mainData[[dateString]], col=rainbow(50))
