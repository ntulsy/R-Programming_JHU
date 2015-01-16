rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    valid.outcomes = c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (!state %in% data$State) 
        stop ("invalid state")
    if (!outcome %in% valid.outcomes)
        stop ("invalid outcome")
    
    column.number <- 23
    if (outcome == "heart attack")
        column.number <- 11
    else if (outcome == "heart failure")
        column.number <- 17
    
    data <- split(data, data$State)
    data <- data[[state]]
    data[[column.number]] <- as.numeric(data[[column.number]])
    data <- data[order(data[, column.number], data$Hospital.Name), ]
    is_na <- is.na(data[, column.number])
    data <- data[!is_na, ]
    
    
    if (num == "best")
        data[1, 2]
    else if (num == "worst")
        data[nrow(data), 2]
    else 
        data[as.numeric(num), 2]
}