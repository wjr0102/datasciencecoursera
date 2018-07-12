drop_NA <- function(file,pollutant = NULL){
    # Drop NA
    data <- NULL
    if (is.null(pollutant)){
        bad <- is.na(file[["nitrate"]]) | is.na(file[["sulfate"]])
        data <- file[[1]][!bad]
    }
    else{
        bad <- is.na(file[[pollutant]])
        data <- file[[pollutant]][!bad]
    }
    data
}