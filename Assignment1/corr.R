corr <- function(directory, threshold = 0){
    old_dir = getwd()
    # Set the directory
    setwd(file.path(old_dir,directory))
    
    # Initialization
    result <- c()
    index <- 1
    for (i in 1:332){
        # Read the file
        file <- read_csv(i)
        # Get the complete number
        comp = complete(id = c(i))[1,"nobs"]
        if (comp > threshold){
            # Drop the missing data
            bad <- is.na(file[["nitrate"]]) | is.na(file[["sulfate"]])
            data_n <- file[["nitrate"]][!bad]
            data_s <- file[["sulfate"]][!bad]
            result[index] <- cor(data_n,data_s)
            index = index + 1
        }
        
    }
    setwd(file.path(old_dir))
    result
}