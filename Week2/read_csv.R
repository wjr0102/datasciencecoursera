read_csv <- function(i){
    # Change i to the fomart "%03d"
    idx <- sprintf("%03d",i)
    # Modify the file name
    file_name <- paste(idx,".csv",sep = "")
    # Read .csv file
    file <- read.csv(file_name)
    file
}