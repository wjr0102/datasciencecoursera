rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that outcome is valid
    outcome_valid <- outcome %in% c("heart attack","heart failure","pneumonia")
    if (outcome_valid){
        # Outcome col
        num_o <- 0
        if (outcome == "heart attack"){
            num_o <- 11
        }
        if (outcome == "heart failure"){
            num_o <- 17
        }
        if (outcome == "pneumonia"){
            num_o <- 23
        }
        # Change "best" "worst" to interge
        if (num == "best"){
            num <- 1
        }
        
        data_out <- data[c(2,7,num_o)]
        
        ## For each state, find the hospital of the given rank
        state <- unique(data$State)
        state <- state[order(state)]
        hospital <- NULL
        nums <- NULL
        for (s in state){

            # Get state
            data_state <- data_out[data_out$State == s,]
            data_state[[3]] <- as.numeric(data_state[[3]])
            data_sort <- data_state[order(data_state[[3]], data_state$Hospital.Name,na.last = TRUE),]
            len <- nrow(data_sort)
            while (is.na(data_sort[len,3])){
                len = len - 1;
            }
            if (num == "worst"){
                hospital <- c(hospital,data_sort[len,1])
                #nums <- c(nums,len)
            }
            if (num < len){
                hospital <- c(hospital,data_sort[num,1])
                #nums <- c(nums,num)
            }
            else if (num !="worst"){
                hospital <- c(hospital,NA)
                #nums <- c(nums,NA)
            }
        }
    }
    else{
        stop("invalid outcome")
    }
   
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data.frame(hospital,state,row.names = state)
}
