corr <- function(directory,threshold=0) {
  directory <- paste("./",directory,"/",sep='')
  fileList<-list.files(directory)
  id<-1:length(fileList)
  completeTable <- complete("specdata", 1:length(fileList))
  nobs <- completeTable$nobs
  ids <- completeTable$id[nobs > threshold]
  # get the length of ids vector
  idLength <- length(ids)
  dataVector <- rep(0, idLength)
  j<-1
  for(i in ids) {
    filePath <- paste(directory,fileList[i],sep='')
    currentFile <- read.csv(filePath, header=T, sep=",")
   dataVector[j] <-  cor(currentFile$sulfate, currentFile$nitrate, use="complete.obs")
      j<-j+1
  }
  result <- dataVector
  return(result)
}

