complete <- function(directory,id=1:332) {
  directory <- paste("./",directory,"/",sep='')
  fileList<-list.files(directory)
  dataVector <-  rep(0, length(id))
  j<-1
  for(i in id) {
    filePath <- paste(directory,fileList[i],sep='')
    currentFile <- read.csv(filePath, header=T, sep=",")
    dataVector[j] <- sum(complete.cases(currentFile))
    j<-j+1
  }
  result <- data.frame(id = id, nobs = dataVector)
  return(result)
}