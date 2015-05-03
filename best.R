best <- function(state, outcome) {
  ## Read outcome data
  rawData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  stName <- rawData[,7]
  outcome_2_check <- c("heart attack","heart failure","pneumonia")
  ## Check if valid state Name.. Should be American state with two character.
  if((state %in% stName)==FALSE) {
    stop(print("ERROR!!! invalid state"))
  }
  ## Check if valid outcome selected
  if((outcome %in% outcome_2_check)==FALSE) {
    stop(print("ERROR!!! invalid outcome selected"))
  }
  ## Get data
  if(outcome %in% outcome_2_check[1]==TRUE){
    col <- 11
  }
  else if(outcome %in% outcome_2_check[2]==TRUE){
    col <- 17  
  }
  else if(outcome %in% outcome_2_check[3]==TRUE){
    col <- 23
  }
  ## Subset of data
  new.data <- rawData[rawData$State==state,]
  ## outcome column data
  nData <- as.numeric(new.data[,col])
  missing <- is.na(nData)
  wData <- new.data[!missing, ]
  # to get minimum values
  pdata <- as.numeric(wData[, col])
  pos <- which.min(pdata)
  HName <- wData[pos,"Hospital.Name"]
  # Sort in case of tie
  SortName <- sort(HName, decreasing = FALSE)
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  SortName[1]
}