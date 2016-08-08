rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  fileData <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  
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
  
  ## Sort it first by state, then outcome rate. The lowest rate will be on the top of the list, so it will be ranked.
  ## After, sorts by alphabetical order by Hospital Name. So  ties should be broken by using the hospital name.
  outcomeStateData <- outcomeStateData[order(outcomeStateData$state, outcomeStateData$outcome, outcomeStateData$hospital),]
  
  ## Split the data into a list of data frames separated by state
  splitDataByState <- split(outcomeStateData, outcomeStateData$state)
  
  rankByState <- function(data, ranking = num){
      if (ranking == "best"){
        rankingResult <- head(data$hospital, 1)
      } else if (ranking == "worst"){
        rankingResult <- tail(data$hospital, 1)
      } else {
        rankingResult <- data$hospital[ranking]
      }    
  }
  
  ## For each state, find the hospital of the given rank
  rankingListByState <- sapply(splitDataByState, rankByState)

  ## Return a data frame with the hospital names and the (abbreviated) state name  
  data.frame(hospital=rankingListByState, state=names(rankingListByState), row.names=names(rankingListByState))

}