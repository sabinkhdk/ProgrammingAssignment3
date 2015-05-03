rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        rawData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
        ## Check number given vs number of hospitals
        if (is.numeric(num) == TRUE) {
                if (length(rawData[,2]) < num) {
                        return(NA)
                }
        }
        ## Subset of data
        new.data <- rawData[rawData$State==state,]
        ## outcome column data
        nData <- as.numeric(new.data[,col])
        missing <- is.na(nData)
        wData <- new.data[!missing, ]
        ## Sorting 
        OColName <- names(wData)[col] # output col name
        HColName <- names(wData)[2] # Hospital col Name
        index <- with(wData, order(wData[OColName], wData[HColName]))
        sort_data <- wData[index, ]        
        # Recode "best"==1 and "worst" == last amongst all
        if (is.character(num) == TRUE) {
                if (num == "best") {num = 1}
                else if (num == "worst") {
                        num = length(sort_data[, col])
                }
        }
        #return the hospital name with the outcome ranking of num
        sort_data[num, 2]  
}
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
