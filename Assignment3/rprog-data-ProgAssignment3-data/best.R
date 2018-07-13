# File note:
#   2.  Hosipital name
#   7.  State
#   11. heart attact
#   17. heart failure
#   23. pneumonia

best <- function(state, outcome) {
    ## Initialization
    result <- NULL
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    state_valid <- state %in% data$State
    outcome_valid <- outcome %in% c("heart attack","heart failure","pneumonia")
    if (state_valid & outcome_valid){
        num <- 0
        if (outcome == "heart attack"){
            num <- 11
        }
        if (outcome == "heart failure"){
            num <- 17
        }
        if (outcome == "pneumonia"){
            num <- 23
        }
        # Get three col
        data_state <- data[c(2,7,num)]
        # Get the right state
        data_state <- data_state[data_state$State == state,]
        data_state[[3]] <- as.numeric(data_state[[3]])
        # Drop off NA
        bad <- is.na(data_state[[3]])
        data_valid <- data_state[!bad,]
        # Get the min
        min_value <- min(data_valid[[3]])
        result <- data_valid$Hospital.Name[which(data_valid[[3]] == min_value)]
        result <- result[order(result)]
    }
    else if (!state_valid){
        stop("invalid state")
    }
    else{
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    result[1]
}
