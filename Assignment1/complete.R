complete <- function(directory = NULL, id = 1:332){
    if (!is.null(directory)){
        old_dir = getwd()
        # Set the directory
        setwd(file.path(old_dir,directory))
    }
    
    nobs = c(1:length(id))
    index = 1
    for (i in id){
        # Read the file
        file <- read_csv(i)
        # Drop the missing data
        data <- drop_NA(file)
        # Record nobs
        nobs[index] = length(data)
        index = index + 1
    }
    if (!is.null(directory)){
        setwd(file.path(old_dir))
    }
    data.frame(id = id, nobs = nobs)
    
}