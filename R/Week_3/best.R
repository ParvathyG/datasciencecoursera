best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data   <- as.data.frame(cbind(outcomes[, 2],   # hospital
                                outcomes[, 7],   # state
                                outcomes[, 11],  # heart attack
                                outcomes[, 17],  # heart failure
                                outcomes[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  
  ## Check that state and outcome are valid
  if(!state %in% data[, "state"]) {stop("invalid state")}
  
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop("invalid outcome")}
  else{
  
   
  stateIdx <- which(data[, "state"] == state)
  stateData <- data[stateIdx, ]    # extracting data for the called state
  values <- as.numeric(stateData[, eval(outcome)])
  min_val <- min(values, na.rm = TRUE)
  result  <- stateData[, "hospital"][which(values == min_val)]
  output  <- result[order(result)]
  }
  return (output)
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}