pollutantmean <- function(directory, pollutant, id = 1:332){
    old_dir = getwd()
    # Set the directory
    setwd(file.path(old_dir,directory))
    
    sum_number <- 0
    count <- 0
    for (i in id){
        # Read the file
        file <- read_csv(i)
        # Drop the missing data
        data <- drop_NA(file,pollutant)
        # Calculate
        count <- count + length(data)
        sum_number <- sum_number + sum(data)
    }
    setwd(file.path(old_dir))
    sum_number / count
}