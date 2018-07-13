rankhospital <- function(state, outcome, num = "best") {
    result <- NA
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    state_valid <- state %in% data$State
    outcome_valid <- outcome %in% c("heart attack","heart failure","pneumonia")
    if (state_valid & outcome_valid){
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
        # Get three col
        data_state <- data[c(2,7,num_o)]
        # Get the right state
        data_state <- data_state[data_state$State == state,]
        data_state[[3]] <- as.numeric(data_state[[3]])
        # Rank
        data_sort <- sort(data_state[[3]])
        if (num == "best"){
            num <- 1
        }
        else if (num == "worst"){
            num <- length(data_sort)
        }
        if (num <= length(data_sort)){
            value <- data_sort[num]
            result <- data_state$Hospital.Name[which(data_state[[3]]==value)]
            # Get the specific one
            index <- 1
            while ((num - 1 > 0) && data_sort[num-1]==value){
                index <- index + 1
                num <- num - 1
            }
            result <- result[order(result)]
            result <- result[index]
        }
    }
    else if (!state_valid){
        stop("invalid state")
    }
    else{
        stop("invalid outcome")
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    result
}
