source("best.R")
rankhospital <- function(state, outcome, num){
        ## The purpose of this function is to find hospitals of a certain ranking = num
        ## Input: 2 letter state abbreviation, outcome = one of ("heart attack", "heart failure", "pneumonia"),
        ## num which is an integer ranking or "worst" or "best"
        ## Output: Char vector of first hospital in that rank category
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # reading the file
        subset_state <- data[which(data$State == state), ] # contains all data but only for "state"
        outcome_col <- numeric() # this will hold the col number assciated to outcome
        values <- numeric() # this vector will hold all values from state and outcome
        if(outcome == "heart attack"){
                outcome_col <- 11
                name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
                all_hospitals <- subset_state[ , c("Hospital.Name", name)]
        }else if(outcome == "heart failure"){
                outcome_col <- 17
                name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  
                all_hospitals <- subset_state[ , c("Hospital.Name", name)]
        }else if(outcome ==  "pneumonia"){
                outcome_col <- 23
                name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
                all_hospitals <- subset_state[ , c("Hospital.Name", name)]
        }
        for(i in 1:nrow(subset_state)){
                values[i] <- subset_state[i, outcome_col] # collecting all values for outcome
                values <- unique(sort(as.numeric(values))) # ascending sort, removal of NAs and nique values of values vec
        }
        rank_value <- values[num] # the value of outcome for the rank specified by num
        rank_hospitals <- character()  # this is where the relavant hospitals will be stored
        for(i in 1:nrow(all_hospitals)){
                if(all_hospitals[i,2] == rank_value){
                        rank_hospitals[i] <- all_hospitals[i,"Hospital.Name"]
                }
        }
#        hospital <- sort(hospital[,"Hospital.Name"])
#        rank_value <- values[num] # the value of outcome for the rank specified by num
#        hospital <- subset(subset_state, name == rank_value) #character() # vector will hold the names of the hospitals of the specified ranking
#         for(i in 1:nrow(subset_state)){
#                 if(subset_state[i, outcome] == rank_value){
#                         hospital[i] <- subset_state[i, "Hospital.Name"]
#                 }
#         }
        print(values)
        print(length(values))
        rank_hospitals <-sort(rank_hospitals)
        rank_hospitals
}