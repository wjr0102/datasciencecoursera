drop_NA <- function(file,pollutant){
    # Drop NA
    bad <- is.na(file[[pollutant]])
    data <- file[[pollutant]][!bad]
    data
}