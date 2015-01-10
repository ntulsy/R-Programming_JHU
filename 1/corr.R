corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    files <- list.files(, path = directory, pattern="*.csv")
    data <- numeric()
    for (file in files) {
        path <- paste(directory, "/", file , sep="")
        temp <- read.csv(path)
        is.complete = complete.cases(temp)
        temp <- temp[is.complete, ]
        if (nrow(temp) > threshold){
            cor.value <- cor(x = temp$sulfate, y = temp$nitrate)
            data <- c(data, cor.value)
        }
    }
    data
}