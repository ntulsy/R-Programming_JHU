rankall <- function(outcome, num = "best") {
    rv <- data.frame()
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    valid.outcomes = c("heart attack", "heart failure", "pneumonia")
    
    if (!outcome %in% valid.outcomes)
        stop ("invalid outcome")
    
    column.number <- 23
    if (outcome == "heart attack")
        column.number <- 11
    else if (outcome == "heart failure")
        column.number <- 17
    
    data <- split(data, data$State)
    for (j in names(data)){
        i <- data[[j]]
        i[[column.number]] <- as.numeric(i[[column.number]])
        i <- i[order(i[, column.number], i$Hospital.Name), ]
        is_na <- is.na(i[, column.number])
        i <- i[!is_na, ]
        if (num == "best")
            t <- i[1, 2]
        else if (num == "worst")
            t <- i[nrow(i), 2]
        else 
            t <- i[as.numeric(num), 2]
        rv <- rbind(rv, data.frame(x = t, y = j))
        
    }
    colnames(rv) <- c("hospital", "state")
    rv
}