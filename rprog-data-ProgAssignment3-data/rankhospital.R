rankhospital <- function(state, outcome, num){
        
        ## The purpose of this function is to find hospitals of a certain ranking = num
        ## Input: 2 letter state abbreviation, outcome = one of ("heart attack", "heart failure", "pneumonia"),
        ## num which is an integer ranking or "worst" or "best"
        ## Output: Char vector of first hospital in that rank category
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # reading the file
        len <- nrow(data)
        state_found = FALSE
        for(i in 1:len){  # checking that the state argument is valid
                if(data[i, "State"] == state){
                        state_found = TRUE
                        return
                }
        }
        if(!state_found){
                stop("invalid state")
        }
        subset_state <- data[which(data$State == state), ] # contains all data but only for "state"
        values <- numeric() # this vector will hold all values from state and outcome
        if(outcome == "heart attack"){
                name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
                all_hospitals <- subset_state[ , c("Hospital.Name", name)]
        }else if(outcome == "heart failure"){
                name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  
                all_hospitals <- subset_state[ , c("Hospital.Name", name)]
        }else if(outcome ==  "pneumonia"){
                name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
                all_hospitals <- subset_state[ , c("Hospital.Name", name)]
        }else{
                stop("invalid outcome")
        }
        all_hospitals[ , 2] <- as.numeric(all_hospitals[ , 2])
        for(i in 1:nrow(all_hospitals)){
                if(is.na(all_hospitals[i,2])){
                        next
                }else{
                        values[i] <- all_hospitals[i, 2] # collecting all values (including repetitions) for outcome
                        values <- sort(as.numeric(values)) # ascending sort, removal of NAs
                }
               
        } 
        if(num == "best"){
                num <- 1
        }else if(num == "worst"){
                num <- length(values)
        }
        if(num > length(values)){ # stopping if rank choice is too high
               return(NA)
               stop()
        }   
        rank_value <- values[num] # the value of outcome for the rank specified by num
        hospitals <- character()  # this is where the relavant hospitals will be stored
        for(i in 1:nrow(all_hospitals)){
                if(is.na(all_hospitals[i,2])){
                        all_hospitals[i,2] <- rank_value - 1 # just making != to rank_value so next loop is false
                }
                if(all_hospitals[i,2] == rank_value){
                        hospitals[i] <- all_hospitals[i,"Hospital.Name"]
                 }
         }
        hospitals <- sort(hospitals)
        source("index_hospital.R")
        index <- index_hospital(values, num)
        hospitals[index]
}