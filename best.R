best <- function(state, outcome) {
    ## Read outcome data
    data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    filter<- c('heart attack','heart failure','pneumonia')
    
    if(!state %in% unique(data[,7])) stop("invalid state")
    if(!outcome %in% filter) stop("invalid outcome")  
    
    ## Return hospital name in that state with lowest 30-day death rate
    #init column variable to get appropriate column depending outcome
    column<- (switch(outcome , "heart attack" = 11 , "heart failure" = 17 , "pneumonia" = 23))
    #First filitration by state and outcome
    filteredData<- subset(data, State==state ,select=c(2,column))
    #giving column names to filtered data
    names(filteredData)<- c("Hospital","Outcome")
    #clearing non existent data and converting outcome to numeric
    filteredData<- subset(filteredData, Outcome !="Not Available")
    filteredData[, 2] <- as.numeric(filteredData[, 2])
    #finding the minimum value
    minimum<-min(filteredData[,2])
    #searching data for multiple observations having the same minimum
    filteredData<- subset(filteredData, Outcome==minimum)
    #sorting the candidate hospitals alphabetically having the same minimum outcome value
    filteredData<-filteredData[order(filteredData$Hospital),]
    #returning first observation with highest sorting rank and having the minimum outcome value
    filteredData[1,1]
}