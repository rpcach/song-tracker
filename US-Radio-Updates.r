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

#barplot(spinData[[1]])

for(i in 2:TOTALDAYS) {
  date <- date-1
  yesterday <- getDayDataTable(date)
  yesterday <- yesterday[c("Title","Spins")]
  colnames(yesterday)[2] <- as.character.Date(date)
  
  mainData <- merge(mainData, yesterday, by="Title")
}

mainData <- mainData[order(mainData$Spins, decreasing = TRUE),]
xmatrix <- data.matrix(mainData[-1])
persp(x = 1:nrow(mainData), y = 1:TOTALDAYS, z = xmatrix, phi=40, theta=10)
library("plot3D")
persp3D(x = 1:nrow(mainData), y = 1:TOTALDAYS, z = xmatrix)
#size of matrix must be equal to x-by-y in persp function

library("rgl")
persp3d(x = 1:nrow(mainData), y = 1:TOTALDAYS, z = xmatrix, col=rainbow(1000), xlab="Song Position", ylab="Time Ago in Days", zlab="Spins")
#browseURL(paste("file://", writeWebGL(dir=file.path("radio", "webGL"), width=700), sep=""))
#creates html file holding interactable 3d-plot

#barplot(current$Spins, names.arg = current$Pos, col=rainbow(50))
#barplot(yesterday$Spins, names.arg = yesterday$Pos, col=rainbow(50))

