## Function that returns a character vector with the name of the hospital that has the best 30-day mortality
## for the specified outcome in that state.
## Based on dataset that contains information about 30-day mortality and readmission rates for heart attacks, 
## heart failure, and pneumonia for over 4,000 hospitals avaiable at Hospital Compare web site (http://hospitalcompare.hhs.gov)

best <- function(state, outcome) {
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  if (is.na(match(state, unique(outcomeData$State)))){
    stop("invalid state")
  }
  
  if (is.na(match(outcome, c("heart attack", "heart failure", "pneumonia")))){
    stop("invalid outcome")
  }  
  
  ## Get the subset data for the specified state and sort it in alphabetical order by Hospital Name
  ## so if there is a tie for the best hospital for a given outcome, then it will get the first one with wich.min
  ## as defined on programming assignment description
  outcomeDataState <- outcomeData[outcomeData$State == state,]
  outcomeDataState <- outcomeDataState[order(outcomeDataState$Hospital.Name),]
  
  ## Uses wich.min to get the first hospital (alphabetically ordered above) on the specified state that matches
  ## the minimun value for the outcome passed as an argument ("heart attack", "heart failure" or "pneumonia")
  ## wich.min always discards NA values. So hospitals that do not have data on a particular outcome will be
  ## excluded from the set of hospitals when deciding the rankings.
  
  if (outcome == "heart attack") {
    
    bestHospitalonState <- outcomeDataState$Hospital.Name[which.min(outcomeDataState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]
  
  } else if (outcome == "heart failure") {
      
             bestHospitalonState <- outcomeDataState$Hospital.Name[which.min(outcomeDataState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]    
         
         } else {
           
               bestHospitalonState <- outcomeDataState$Hospital.Name[which.min(outcomeDataState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)]    
           }
  
  ## Return hospital name in that state with lowest 30-day death
  bestHospitalonState
  
}
