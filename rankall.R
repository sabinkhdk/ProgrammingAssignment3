rankall <- function(outcome, num = "best") {
        ## Read outcome data
        rawData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        stName <- unique(rawData[,7])
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
        state <- levels(factor(rawData[,7]))  # Get names of state from dataset
        hospital <- vector(mode="character") # intialize vector
        ## Run rankhospital function by state
        for (i in seq(state)) {
                hospital[i] <- rankhospital(state[i], outcome, num)
        }
        data.frame(hospital, state)
}
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name