rankhospital <- function(state, outcome, num= "best") {
## Read outcome data
    data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    filter<- c('heart attack','heart failure','pneumonia')
    
    if(!state %in% unique(data[,7])) stop("invalid state")
    if(!outcome %in% filter) stop("invalid outcome")  

    
## Return hospital name in that state with the given rank
    
    #init column variable to get appropriate column requested by outcome
    column<- (switch(outcome , "heart attack" = 11 , "heart failure" = 17 , "pneumonia" = 23))
    
    #First filitration by state and outcome
    filteredData<- subset(data, State==state ,select=c(2,column))
    
    #giving column names to filtered data
    names(filteredData)<- c("Hospital.Name","Rate")
    
    #clearing non existent data and converting outcome to numeric
    filteredData<- subset(filteredData, Rate !="Not Available")
    filteredData[, 2] <- as.numeric(filteredData[, 2])
    
    #sorting the hospitals by rate, hospital name and attaching rank column to final list
    filteredData<-filteredData[order(filteredData$Rate,filteredData$Hospital.Name),]
    maxrank<-nrow(filteredData)
    ranked<- cbind(filteredData,Rank=1:maxrank)
    
    #resolving passed rank vaiable and attaching it to num numeric variable
    if(!is.numeric(num)) {
        selection<- switch(num, "best" = 1 , "worst" = maxrank)} 
    else selection<-num
    
    #returning the requested rank observable, returns NA if rank is not available
    if (selection > maxrank) return(NA)
    else {
        result<- subset(ranked, Rank==selection,1)
    }
    result[1,1]    
    
}