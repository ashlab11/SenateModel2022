#import libraries
library(googledrive)
library(googlesheets4)
library(tidyverse)

senate_sheets <- drive_get("Senate Data")

priors <- senate_sheets %>%
  range_read(sheet = 2)
states <- senate_sheets %>%
  range_read(sheet = 1)
genballot <- senate_sheets %>%
  range_read(sheet = 11)
overperformance <- senate_sheets %>%
  range_read(sheet = 13)


#modify the previous priors to be relative to genballot

priors[, 2] <- priors[, 2] - genballot[1, 2][[1]]
priors[, 3] <- priors[, 3] - genballot[2, 2][[1]]
priors[, 4] <- priors[, 4] - genballot[3, 2][[1]]

# function to find the covariance between polls/priors in a state
totalvar <- function(InputState) {
    #get priors and polls for the state
  if (InputState %in% c("Alaska", "Utah")){
    var_state_priors <- c(0, 0, 0)
    var_polls_weights <- c(0.25, 0.25, 0.5)
  }else{
    var_state_priors <- as.numeric(priors[priors$State == InputState, 2:4])
    var_polls_weights <- 0.7 * c(0.25, 0.25, 0.5)
  }
  if (InputState %in% currentpolls$state) {
    var_state_polls <- as.numeric(poll_error[poll_error$State == InputState, 2:4])
    var_priors_weights <- 0.3 * c(0.3, 0.1, 0.6)
    
  }else{
    var_state_polls <- c(0,0,0)
    var_priors_weights <- c(0.3, 0.1, 0.6)
  }

    #getting variance
    combined_observations <- rbind(var_state_priors, var_state_polls)
    combined_weights <- rbind(var_priors_weights, var_polls_weights)
    weighted_means <- rbind(weighted.mean(combined_observations[1, ], combined_weights[1, ]), weighted.mean(combined_observations[2, ], combined_weights[2, ]))
    diff <- sweep(combined_observations, 1, weighted_means, '-')
    
    #square rooting the variance to make sure that weights are only counted once
    varweighted <- diff * sqrt(combined_weights)
    varmatrix <- (as.matrix(varweighted) %*% t(varweighted)) / (as.matrix(sqrt(combined_weights)) %*% t(sqrt(combined_weights)))
    #not square rooting the covar, to make sure the weights are counted once
    covarweighted <- diff * (combined_weights)
    covmatrix <- (as.matrix(covarweighted) %*% t(covarweighted)) / (as.matrix(combined_weights) %*% t(combined_weights))
    polls_priors_var <- varmatrix[1,1] + varmatrix[2, 2] + 2*covmatrix[1,2]
    
    var <- ifelse(InputState %in% current_experts$State, polls_priors_var*0.9 + varexpert*0.1, polls_priors_var)
    return(sqrt(var))

} 


#function to get the meanresult for a state
meanresult <- function(InputState) {
  if (!(InputState %in% c("Alaska", "Utah"))){
    #PRIORS FIRST
    #get priors and polls for the state
    state_priors <- priors[priors$State == InputState, 2:4]
    #giving weights to priors: if polls exist, then they're weighted at 0.3, and if they don't, weighted at 1
    priors_weights <- 
      if (InputState %in% currentpolls$state) {
        0.3 * c(0.3, 0.1, 0.6)
      } else{
        c(0.3, 0.1, 0.6)
      }
    #sum of the past years added to the general ballot * the previously determined weights
    priors_prediction <- sum((state_priors + genballotresult) * priors_weights) 
    
    #THEN POLLS
    #polls prediction: 0 if the polls don't exist, and the current polls plus the previous polling error's weighted average if they do
    polls_prediction <- 
      if (InputState %in% currentpolls$state) {
        (currentpolls[currentpolls$state == InputState, 2][[1]] + sum(poll_error[poll_error$State == InputState, 2:4] * c(0.25, 0.5, 0.25))) * 0.7
      } else{
        0
      }
    
    #summing together the polling predictions
    priors_polls_prediction <- priors_prediction + polls_prediction
  } else{
    #what to do if the state IS Alaska or Utah
    priors_polls_prediction <- currentpolls[currentpolls$state == InputState, 2][[1]]
  }


    
    #combining the previous predictions with experts and overperformance
    if (InputState %in% current_experts$State) {
      if (InputState %in% overperformance$State) {
        total_prediction <- priors_polls_prediction * 0.8 + current_experts[current_experts$State == InputState, 2][[1]] * 0.1 + 
          overperformance[overperformance$State == InputState, 4][[1]]*0.1
      } else{
          total_prediction <- priors_polls_prediction * 0.9 + current_experts[current_experts$State == InputState, 2][[1]] * 0.1
        }
    } else {
      if (InputState %in% overperformance$State) {
        total_prediction <- priors_polls_prediction * 0.9 + overperformance[overperformance$State == InputState, 4][[1]]*0.1
      } else{
        total_prediction <- priors_polls_prediction
        }
    }
    
    return(total_prediction)
}


chanceofwinning <- function(InputState) {
  #if a state is 15 points in one favor, it's not going to the other side -- so the chance of winning is 100% for whichever side
  if (states[states$State == InputState, 2][[1]][[1]] > 15){
    return(1)
  }
  else if(states[states$State == InputState, 2][[1]][[1]] < -15){
    return(0)
  }
  else{
    meanresult <- states[states$State == InputState, 2][[1]][[1]]
    sd <- states[states$State == InputState, 3][[1]][[1]]
    return(1 - pnorm(0, mean=meanresult, sd=sd))
  }
  
}

states$meanresult <- lapply(states$State, meanresult)
states$sd <- lapply(states$State, totalvar)
states$winchance <- lapply(states$State, chanceofwinning)
write_sheet(states, ss = as_sheets_id("https://docs.google.com/spreadsheets/d/1gXGn5vKjhYyhi2-38qiH4Ps5qMvppXg3JFSVYijZLFU/edit#gid=800772858"), sheet = 17)

