best <- function(state, outcome) {
  ## get data by the outcome, will test for valid outcome
  
  dt <- getDataByOutcome(outcome)
  
  names(dt)[3] <- "rate"
  
  ## get the data for the state that you want
  ## function will retun ordered dataset
  ## and stop on an invalid state
  dt <- getDataByState(dt, state)
  
  # sort by hospital value (column 2) and hospital names (column 3) in alphabetical order
  dt <- dt[order(dt[,3], dt[,2]), ]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  return(as.character(as.vector(dt[1,2])))
  
}

worst <- function(state, outcome) {
  ## get data by the outcome, will test for valid outcome
  
  dt <- getDataByOutcome(outcome)
  
  names(dt)[3] <- "rate"
  
  ## get the data for the state that you want
  ## function will retun ordered dataset
  ## and stop on an invalid state
  dt <- getDataByState(dt, state)
  
  # sort by hospital value (column 2) and hospital names (column 3) in alphabetical order
  dt <- dt[order(-dt[,3], -dt[,2]), ]
  
  ## Return hospital name in that state with highest 30-day death
  ## rate
  
  return(as.character(as.vector(dt[1,2])))
  
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

rankhospital <- function(state, outcome, num = "best") {
  
  # if num is numeric then get the data value
  if (typeof(num) == 'integer'| typeof(num) == 'double'){
    # if num within data rows...
    # Read data and filter by outcome
    data <- getDataByOutcome(outcome)
    
    # filter again by state
    data <- getDataByState(data, state)
    
    # filter finally based on ranking/num
    hospital <- getRanking(data, num)
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    return(hospital)
  } 
  # if num is worst then...
  else if (num == "worst"){
    return(worst(state, outcome))
    
  }
  # if num is 'best' then get data from first row
  else if (num == "best") {
    
    return(best(state, outcome))
    
  }
  # else stop with message 'invalid num'
  else {
    stop("invalid num")
  }
  
}

getRanking <- function(data, num) {
  
  rows <- nrow(data)
  
  # in row=??? and column=2 (death rate)
  if (num <= rows){
    
    # sort by hospital value (column 2) and hospital names (column 3) in alphabetical order
    data <- data[order(data[,3], data[,2]), ]
    
    ## Return hospital name in that state
    
    return(as.character(as.vector(data[num,2])))
    
  }
  else {
    # else num is greater/outside data rows
    return(NA)
  }
  
}


rankall <- function(outcome, num = "best") {
  # Read data and filter by outcome
  data <- getDataByOutcome(outcome)
  
  # extract state column and remove deplicates
  states <- data$State
  
  states <- unique(states)
  
  # order alphabetically
  states <- states[order(states)]
  
  # create an empty data frame for returning values
  dfReturn <- data.frame(hospital = character(length(states))
                         , state = character(length(states))
                         , stringsAsFactors = FALSE) 
  
  ## For each state, find the hospital of the given rank
  for (i in 1:length(states)) {
  
    # if num is numeric then get the data value
    if (typeof(num) == 'integer'| typeof(num) == 'double'){
      # if num within data rows...
      # filter again by state
      statedata <- getDataByState(data, states[i])
      
      # filter finally based on ranking/num
      hospital <- getRanking(statedata, num)
      
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
    } 
    # if num is worst then...
    else if (num == "worst"){
      hospital <- worst(states[i], outcome)
      
    }
    # if num is 'best' then get data from first row
    else if (num == "best") {
      
      hospital <- best(states[i], outcome)
      
    }
    # else stop with message 'invalid num'
    else {
      stop("invalid num")
    }
    
    dfReturn$hospital[i] <- hospital
    dfReturn$state[i] <- states[i]
  
    }
  
  #return dataframe containing hospitals and states
  dfReturn
  
}