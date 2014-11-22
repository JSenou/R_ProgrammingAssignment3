source("best.R")
rankhospital <- function(state, outcome, num){
        ## The purpose of this function is to find hospitals of a certain ranking = num
        ## Input: 2 letter state abbreviation, outcome = one of ("heart attack", "heart failure", "pneumonia"),
        ## num which is an integer ranking or "worst" or "best"
        ## Output: Char vector of first hospital in that rank category
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # reading the file
        subset_state <- data[which(data$State == state), ] # contains all data but only for "state"
        row_num <- nrow(subset_state)
        values <- numeric() # this vector will hold all values from state and outcome
        outcome_col <- numeric() # this will hold the col number assciated to outcome
        if(outcome == "heart attack"){
                outcome_col <- 11
                for(i in 1:row_num){
                        values[i] <- subset_state[i, outcome_col] # collecting all values for outcome
                        values <- unique(sort(as.numeric(values))) # ascending sort, removal of NAs and nique values of values vec
                }
                
        }else if(outcome == "heart failure"){
                outcome_col <- 17
        }else if(outcome ==  "pneumonia"){
                outcome_col <- 23
        }
        print(values)
}