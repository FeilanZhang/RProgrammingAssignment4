rankhospital <- function(state, outcome, num = "best"){
    #read outcome data
    readout <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #check state are valid
    readout[,"State"] <- as.factor(readout[,"State"])
    statenames <- levels(readout[,"State"])
    if(any(state == statenames) == FALSE){
        stop("invalid state")
    }
    newdata <- readout[readout$State == state,]
    newdata[,11] <- as.numeric(newdata[,11])
    newdata[,17] <- as.numeric(newdata[,17])
    newdata[,23] <- as.numeric(newdata[,23])
    #check outcome are valid and order the new data frame
    if(outcome == "heart attack"){
        finalout <- newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, newdata$Hospital.Name, na.last = NA),]
    }
    else if(outcome == "heart failure"){
        finalout <- newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, newdata$Hospital.Name, na.last = NA),]
    }
    else if(outcome == "pneumonia"){
        finalout <- newdata[order(newdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, newdata$Hospital.Name, na.last = NA),]
    }
    else{
        stop("invalid outcome") 
    }
    #check num and return the hospital name
    last <- tail(finalout,1)
    nrows <- nrow(finalout)
    if(num == "best"){
        finalout[1,2]
    }
    else if(num == "worst"){
        last[1,2]
    }
    else if(num > nrows){
        NA
    }
    else{
        finalout[num,2]
    }
}
