index_hospital <- function(ranks, num){
        ## ranks are integers 1,2,... where lower numbers are better
        ## rank values are the data values associated to a rank
        ## For example the if the best value taken is 8.1 then that is the rank value and its rank is 1
        uniq_ranks <- unique(ranks)
        rank_value <- ranks[num]
        #num_preceed <- which(uniq_ranks == rank_value) # the number of rank values less than or equal to rank_value
        count <- 0 
        for(i in 1:num){ # counting the number of hopitals with strictly better rank values than rank_value
                if(ranks[i] < rank_value){
                        count <- count + 1
                }
        }
        order <- num - count # index of the wanted hospital among all the hospitals with rank value = rank_value
        order
}