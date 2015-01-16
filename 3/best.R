best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    valid.outcomes = c("heart attack", "heart failure", "pneumonia")
    ## Check that state and outcome are valid
    if (!state %in% data$State) 
        stop ("invalid state")
    if (!outcome %in% valid.outcomes)
        stop ("invalid outcome")
    
    data <- split(data, data$State)
    
    ## subsetting to a region
    column.number <- 23
    if (outcome == "heart attack")
        column.number <- 11
    else if (outcome == "heart failure")
        column.number <- 17
    data <- data[[state]]
    data <- data[order(data$Hospital.Name), ]
    temp <- data[column.number]
    min <- 100
    min.index <- 1
    for (i in 1:nrow(temp))
        if (!is.na(as.numeric(temp[i, ])) && as.numeric(temp[i, ]) < min)
        {
            min <- as.numeric(temp[i, ])
            min.index <- i
        }
    data[min.index, 2]
    
}