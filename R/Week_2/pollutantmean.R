pollutantmean <- function(directory,pollutant,id=1:332) {
  directory <- paste("./",directory,"/",sep='')
  fileList<-list.files(directory)
  dataVector <- c()
  for(i in id) {
    filePath <- paste(directory,fileList[i],sep='')
    currentFile <- read.csv(filePath, header=T, sep=",")
    cleanData <- currentFile[!is.na(currentFile[, pollutant]), pollutant]
    dataVector <- c(dataVector, cleanData)
  }
  result <- mean(dataVector)
  return(result)
 
}