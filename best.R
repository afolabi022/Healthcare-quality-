best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("C:/Program Files/RStudio/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_states <- unique(data$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% valid_states)) {
    stop("Invalid state")
  }
  
  if (!(outcome %in% valid_outcomes)) {
    stop("Invalid outcome")
  }
  
  ## Convert outcome to the correct column name
  if (outcome == "heart attack") {
    outcome_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    outcome_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    outcome_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  ## Filter data for the given state
  state_data <- subset(data, State == state)
  
  ## Convert the mortality rate to numeric (ensure it is numeric and clean)
  state_data[, outcome_column] <- as.numeric(state_data[, outcome_column])
  
  ## Remove rows with NA mortality rates
  state_data <- state_data[!is.na(state_data[, outcome_column]), ]
  
  ## Find the hospital with the lowest mortality rate
  min_mortality <- min(state_data[, outcome_column], na.rm = TRUE)
  
  ## Find the hospitals with the lowest mortality rate
  best_hospitals <- state_data[state_data[, outcome_column] == min_mortality, "Hospital.Name"]
  
  ## If there is a tie, choose the first hospital alphabetically
  best_hospital <- sort(best_hospitals)[1]
  
  ## Return the best hospital name
  return(best_hospital)
}
source("best.R")

