pollutantmean <- function(directory, pollutant, id=1:332) {
    data <- data.frame()
    for (i in id) {
        longer_id = sprintf("%03d", i)
        path <- paste(directory, "/", longer_id, ".csv", sep="")
        #print(path)
        temp <- read.csv(path)
        data <- rbind(data, temp)
    }
    
    data <- data[pollutant]
    is_na = is.na(data)
    data <- data[!is_na, ]
    mean(data)
    
    # very inefficient code!
    # rbind copies entire table every time.
}