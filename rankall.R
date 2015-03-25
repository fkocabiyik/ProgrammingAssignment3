rankall <- function(outcome, num= "best") {
    ## Read outcome data
    data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states<- unique(data[, 7])
    states<- states[order(states)] #final output should be ordered alphabetically
    
    ## Check that state and outcome are valid
    filter<- c('heart attack','heart failure','pneumonia')
    if(!outcome %in% filter) stop("invalid outcome")  
    
    
    ## Return hospital name in that state with the given rank
    
    #init column variable to get appropriate column requested by outcome
    column<- (switch(outcome , "heart attack" = 11 , "heart failure" = 17 , "pneumonia" = 23))
    
    #First filitration by outcome type
    filteredData<- subset(data, select=c(7,2,column))
    #giving column names to filtered data
    names(filteredData)<- c("State","Hospital","Rate")
    #cleaning data, and char to numeric transformation for rate column
    filteredData<- subset(filteredData, Rate !="Not Available")
    filteredData[, 3] <- as.numeric(filteredData[, 3])
    #ordering data by state, rate, hospital name
    filteredData<-filteredData[order(filteredData$State,filteredData$Rate,filteredData$Hospital),]
    
    #defining a function to be called for each state, function simply returns the hospital name
    #which has the rank equal the passed parameter and hospitals state
    staterank<-function(state) {
        stdata<-filteredData[filteredData$State==state,]
        #measuring subset size and attaching rank variable, since data is already sorted,
        #sequantial attaching gives the appropriate rank
        maxrank<-nrow(stdata)
        ranked<- cbind(stdata,Rank=1:maxrank)
        
        #resolving passed rank vaiable and attaching it to num numeric variable "selection"
        if(!is.numeric(num)) {
            selection<- switch(num, "best" = 1 , "worst" = maxrank)} 
        else selection<-num
        
        #populating the result variable either by NA or actual rank from the State subsetted data
        if (selection > maxrank) result<-NA
        else {
            result<- subset(ranked, Rank==selection,2)
        }
        #returning the function by a vector (rank,state)
        c(result,state)  
        
    }
    
    #lapply calls staterank function for each states member, and do call appy rbind on itself
    output = do.call(rbind, lapply(states, staterank))
    #populating dataframe
    
    #formating data frame
    rownames(output) = output[, 2]
    colnames(output) = c("hospital", "state")
    
    data.frame(output)
    
    
    
    
    
    
    
    
    #returning the requested rank observable, returns NA if rank is not available
    
    
}