## Returns a character vector with the name of the hospital that has the ranking specified by the num argument for the State

rankhospital <- function(state, outcome, num = "best") {
  
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
  
  ## If the number given by num is larger than the number of hospitals in that state, then the function should return NA
  if (is.numeric(num) & num > length(unique(fileData$State))){
    return(NA)
  }
  
  if (!is.numeric(num) & !num %in% c("best", "worst")){
    stop("invalid num argument")
  }  
  
  ## Reducing data that will be manipulated (columns Hospital Name = 2, State = 7, Outcome - function argument)
  outcomeStateData <- fileData[, c(2, 7, outcomes[outcome])]
  names(outcomeStateData) <- c("hospital", "state", "outcome")
  
  ## Discards incomplete cases. So hospitals that do not have data on a particular outcome will be
  ## excluded from the set of hospitals when deciding the rankings.
  outcomeStateData <- na.omit(outcomeStateData)
  
  ## Get the subset data for the specified state
  outcomeStateData <- outcomeStateData[outcomeStateData$state == state,]
  
  ## Sort it first by outcome rate. The lowest rate will be on the top of the list, so it will be ranked.
  ## After, sorts by alphabetical order by Hospital Name. So  ties should be broken by using the hospital name.
  outcomeStateData <- outcomeStateData[order(outcomeStateData$outcome, outcomeStateData$hospital),]
  
  if (num == "best"){
    rankingResult <- head(outcomeStateData$hospital, 1)
  } else if (num == "worst"){
            rankingResult <- tail(outcomeStateData$hospital, 1)
          } else {
             rankingResult <- outcomeStateData$hospital[num]
          }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  rankingResult
}