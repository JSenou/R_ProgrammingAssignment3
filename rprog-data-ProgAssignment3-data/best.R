best <- function(state, outcome){
        ## This functions purpose is to figure out the best hospitla in a state
        ## Input: 2 letter state abreviation, and
        ## outcome = one of ("heart attack", "heart failure", "pneumonia")
        ## Output: returns the hospital with the lowest 30 day mortality for outcome
        
        ## reading in the data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # reading the file

        ## Checking the validity of the input arguments
        library(datasets)
        states <- c(state.abb, "DC")
        outcomes <-c("heart attack", "heart failure", "pneumonia")
        if(outcome %in% outcomes == FALSE){ stop("invalid outcome") }
        if(state %in% states == FALSE){ stop("invalid state") }
        
        ## Filter data + simplify the column names
        data <- data[c(2, 7, 11, 17, 23)]
        names(data)[1] <- "hospital.name"
        names(data)[2] <- "state"
        names(data)[3] <- "heart attack"
        names(data)[4] <- "heart failure"
        names(data)[5] <- "pneumonia"

        ## Gathering only the rows for the relevant state
        subset_state <- data[data$state == state,] 

        ## Finding the min value in the outcome col/relevant col
        minimum <- min( as.numeric( subset_state[ ,outcome] ), na.rm = TRUE ) 

        best_hospitals <- character() # this vector will hold the names of the best hospitals
        
        ## Searching and capturing hopitals with best rankings
        for(i in 1:nrow(subset_state)){
                if(is.na(as.numeric( subset_state[i ,outcome] ))){
                        next
                }
                else if(as.numeric( subset_state[i ,outcome] ) == minimum ){
                        best_hospitals[i] <- subset_state[i, "hospital.name"]
                }
        }    
        best_hospitals <- sort(best_hospitals) # abc order sorting
        best_hospitals[1] # picking the first onea
}