library("rvest")

date <- Sys.Date()-2
dateString <- gsub("-","",as.character.Date(date))
url <- paste("http://kworb.net/radio/pop/archives/",dateString,".html", sep="")
thedata <- url %>% read_html() %>% html_nodes(xpath="/html/body/table") %>% html_table()
thedata <- thedata[[1]]
keeps <- c("Pos", "Title", "Spins")
thedata <- thedata[keeps]

##spinData <- as.data.frame(cbind(current$Spins, yesterday$Spins))
#barplot(spinData[[1]])

TOTALDAYS <- 30

for(i in 2:TOTALDAYS) {
  date <- date - 1
  dateString <- gsub("-","",as.character.Date(date))
  url <- paste("http://kworb.net/radio/pop/archives/",dateString,".html", sep="")
  yesterday <- url %>% read_html() %>% html_nodes(xpath="/html/body/table") %>% html_table()
  yesterday <- yesterday[[1]]
  yesterday <- yesterday[c("Title","Spins")]
  colnames(yesterday)[2] <- dateString
  
  thedata <- merge(thedata, yesterday, by="Title")
}

thedata <- thedata[order(thedata$Pos),]
spinData <- as.data.frame(thedata[3:32])
xmatrix <- data.matrix(spinData)
persp(x = 1:nrow(thedata), y = 1:TOTALDAYS, z = xmatrix, phi=40, theta=10)
library("plot3D")
persp3D(x = 1:nrow(thedata), y = 1:TOTALDAYS, z = xmatrix)
#size of matrix must be equal to x-by-y in persp function
library("rgl")
persp3d(x = 1:nrow(thedata), y = 1:TOTALDAYS, z = xmatrix, col=rainbow(1000))
#browseURL(paste("file://", writeWebGL(dir=file.path("desktop", "webGL"), width=700), sep=""))
#creates html file holding interactable 3d-plot

#barplot(current$Spins, names.arg = current$Pos, col=rainbow(50))
#barplot(yesterday$Spins, names.arg = yesterday$Pos, col=rainbow(50))
