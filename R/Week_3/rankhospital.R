rankhospital <- function(state, outcome,rank="best") {
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
  else if(is.numeric(rank)){
    
    
    stateIdx <- which(data[, "state"] == state)
    stateData <- data[stateIdx, ]    # extracting data for the called state
    values <- as.numeric(stateData[, eval(outcome)])
    stateData[, eval(outcome)] <- as.numeric(stateData[, eval(outcome)])
    stateData <- stateData[order(stateData[, eval(outcome)], stateData[, "hospital"]), ]
    output <- stateData[, "hospital"][rank]
  }else if(!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      stateIdx <- which(data[, "state"] == state)
      stateData <- data[stateIdx, ]    
      stateData[, eval(outcome)] <- as.numeric(stateData[, eval(outcome)])
      stateData <- stateData[order(stateData[, eval(outcome)], stateData[, "hospital"], decreasing = TRUE), ]
      output <- stateData[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
    
  }
  return (output)
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}