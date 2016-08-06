## Function that returns a character vector with the name of the hospital that has the best 30-day mortality
## for the specified outcome in that state.
## Based on dataset that contains information about 30-day mortality and readmission rates for heart attacks, 
## heart failure, and pneumonia for over 4,000 hospitals avaiable at Hospital Compare web site (http://hospitalcompare.hhs.gov)

best <- function(state, outcome) {
  
  ## Read outcome data
  fileData <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  if(!state %in% unique(fileData$State)){
    stop("invalid state")
  }
  
  ## Defines a vector to test argument and after select outcome columns
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  if (!outcome %in% names(outcomes)){
    stop("invalid outcome")
  }  
  
  ## Reducing data that will be manipulated (columns Hospital Name = 2, State = 7, Outcome - function argument)
  outcomeStateData <- fileData[, c(2, 7, outcomes[outcome])]
  names(outcomeStateData) <- c("hospital", "state", "outcome")
  
  ## Discards incomplete cases. So hospitals that do not have data on a particular outcome will be
  ## excluded from the set of hospitals when deciding the rankings.
  outcomeStateData <- na.omit(outcomeStateData)
  
  ## Get the subset data for the specified state
  outcomeStateData <- outcomeStateData[outcomeStateData$state == state,]
  
  ## Sort it in alphabetical order by Hospital Name. So if there is a tie for the best hospital for a given outcome,
  ## then it will get the first one with wich.min as defined on programming assignment description
  outcomeStateData <- outcomeStateData[order(outcomeStateData$hospital),]

  ## Uses wich.min to get the first hospital (alphabetically ordered above) on the specified state that matches
  ## the minimun value for the outcome passed as an argument ("heart attack", "heart failure" or "pneumonia")  
  bestHospitalonState <- outcomeStateData$hospital[which.min(outcomeStateData$outcome)]
  
  ## Return hospital name in that state with lowest 30-day death
  bestHospitalonState
  
}
