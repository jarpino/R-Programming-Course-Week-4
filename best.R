best <- function(state, outcome) {
  ## get data by the outcome, will test for valid outcome
  
  dt <- getDataByOutcome(outcome)
  
  names(dt)[3] <- "rate"
  
  ## get the data for the state that you want
  ## function will retun ordered dataset
  ## and stop on an invalid state
  dt <- getDataByState(dt, state)
  
  topValue <- dt[[1,3]]
  
  dt <- subset(dt, dt[,3] == topValue)
  
  dt <- dt[order(dt[,2]), ]
  
  dt
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  as.character(as.vector(dt[1,2]))
  
}

getDataByOutcome <- function(outcome) {
  ## Read data from csv file
  dataTable <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character");
  
  # - hospital name column
  hospitalColName <- colnames(dataTable[2])
  
  # - state column (useful for rankall function)
  stateColName <- colnames(dataTable[7])
  
  # - death rate column based on outcome
  
  if (outcome == 'heart attack'){
    dataColName <- colnames(dataTable[11])
  }
  else if (outcome == 'heart failure') {
    dataColName <- colnames(dataTable[17])
  }
  else if (outcome == 'pneumonia') {
    dataColName <- colnames(dataTable[23])
  }
  else {
    stop("invalid outcome")
  }
  
  # filter data containing 3 columns:
  columns <- c(stateColName,hospitalColName,dataColName)
  
  # re-assign data frame with only 3 columns
  reducedTable <- dataTable[columns]
  
  # convert death rate (column 3) to numeric
  
  reducedTable[, 3] <- suppressWarnings(as.numeric(as.vector(reducedTable[, 3])))
  
  # remove rows with NAs in column 2
  reducedTable <- reducedTable[complete.cases(reducedTable),]
  
  # stop on invalid state - no rows in data
  
  if (nrow(reducedTable) < 1) {
    stop("no rows returned")
  }

  # return data
  reducedTable
  
}

getDataByState <- function(data, state) {
  # filter by state
  data <- subset(data, State == state)
  
  # remove rows with NAs
  data <- data[complete.cases(data),]
  
  # stop on invalid state - no rows in data
  if (nrow(data) < 1) {
    stop("invalid state")
  }
  
  # sort on column 3 (death rate) to have lowest value on top
  data <- data[order(data[,3]), ]
  
  ## Return all data based on state and outcome
  data
  
}
