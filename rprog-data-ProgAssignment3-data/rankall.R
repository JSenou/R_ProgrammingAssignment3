rankall <- function(outcome, num){
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") # reading in the data
        if(outcome == "heart attack"){
                name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
        }else if(outcome == "heart failure"){
                name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  
        }else if(outcome ==  "pneumonia"){
                name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
        }else{
                stop("invalid outcome")
        }
        data <- data[ , c("Hospital.Name", "State", name) ] # subsetting the data
        data[,3] <- as.numeric(data[,3])
        library(datasets) # use the dataset state.abb which contains all 50 state abbreviations
        hospitals <- character()
        for(i in 1:50){
                state_info <- data[which(data$State == state.abb[i]), ] # contains all data but only for "state"
                values <- state_info[,c(name)]
                values <- sort(values)
                rank_value <- values[num]
                temp <- character()
                for(j in 1:nrow(state_info)){
                        if(is.na(state_info[j,3])){
                                next #state_info[j,3] <- rank_value - 1 # just making != to rank_value so next loop is false
                        }
                        if(state_info[j,3] == rank_value){
                                temp[j] <- state_info[j, "Hospital.Name"]
                        }
                }
                temp <- sort(temp)
                source("index_hospital.R")
                index <- index_hospital(values, num)
                hospitals[i] <- temp[index]
        }
        rankings <- data.frame(hospitals,state.abb) # this will hold all the relevant rankings to be returned
        print(rankings)
}