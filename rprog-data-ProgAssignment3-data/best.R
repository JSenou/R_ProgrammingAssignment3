best <- function(state, outcome){
        ## This functions purpose is to figure out the best hospitla in a state
        ## Input: 2 letter state abreviation, and
        ## outcome = one of ("heart attack", "heart failure", "pneumonia")
        ## Output: returns the hospital with the lowest 30 day mortality for outcome
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # reading the file
        len <- nrow(data)
        state_found = FALSE
        for(i in 1:len){
                if(data[i, "State"] == state){
                        state_found = TRUE
                        return
                }
        }
        if(!state_found){
                stop("invalid state")
        }
        subset_state <- data[which(data$State == state), ] # contains all data but only for "state"
        row_num <- nrow(subset_state)
        minimum <- character() # holds the best mortality rate
        hospital <- character() # this vector will hold the names of the best hospitals
        # the loops below calculate the min outcome, finds all hospitals with the min
        if(outcome == "heart attack"){
                minimum <- min(as.numeric(subset_state[ ,11]), na.rm = TRUE) 
                for(i in 1:row_num){
                       if(subset_state[i ,11] == as.character(minimum)){
                                hospital[i] <- subset_state[i, "Hospital.Name"]
                        }
                }
        }else if(outcome == "heart failure"){
                minimum <- min(as.numeric(subset_state[ ,17]), na.rm = TRUE)
                for(i in 1:row_num){
                        if(subset_state[i ,17] == as.character(minimum)){
                                hospital[i] <- subset_state[i, "Hospital.Name"]
                        }
                }
        }else if(outcome ==  "pneumonia"){
                minimum <- min(as.numeric(subset_state[ ,23]), na.rm = TRUE)
                for(i in 1:row_num){
                        if(subset_state[i ,23] == as.character(minimum)){
                                hospital[i] <- subset_state[i, "Hospital.Name"]
                        }
                }
        }else{
                stop("invalid outcome")
        }     
        hospital <- sort(hospital) # abc order sorting
        hospital[1]
}