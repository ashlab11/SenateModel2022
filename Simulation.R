#import libraries
library(googledrive)
library(googlesheets4)
library(tidyverse)

senate_sheets <- drive_get("Senate Data")

#Pull the state column from the state matrix
stateslist <- senate_sheets %>%
  range_read(sheet = 1) %>%
  slice(-c(27)) %>%
  filter(!(State %in% c("Alaska", "Louisiana", "Utah"))) %>%
  pull(State)

#Get demographics
demographics <- senate_sheets %>%
  range_read(sheet = 6) %>%
  slice(-c(1, 2, 4, 19, 29, 37:50)) %>%
  select(c(2:10))
  
  
#scaling the demographics
demographics_scaled <- as.data.frame(scale(demographics)) %>%
  add_column(state = stateslist, .before = "%Black")

#function to get the Pearson r-value between two states
r_value <- function(state1, state2) {
  state1dems <- as.numeric(unlist(demographics_scaled[demographics_scaled$state == state1, 2:10]))
  state2dems <- as.numeric(unlist(demographics_scaled[demographics_scaled$state == state2, 2:10]))
  return(cor(state1dems, state2dems))
}

#creates matrix with every combination of two states
combination <- expand.grid(stateslist, stateslist) %>%
  rename(state1 = Var1, state2 = Var2) %>%
  mutate(state1 = as.character(state1), state2 = as.character(state2)) %>%
  rowwise() %>%
  mutate(correlation = r_value(state1, state2))


simulationstates <- states %>%
  filter(!(State %in% c("Alaska", "Utah", "Louisiana", "Oklahoma (special)")))

#creates a random list of 31 numbers, then dot products that with every state to get the simulated result of each election
numseats <- function() {
  new_z_score <- function(InputState) {
    return(pull(combination[combination$state1 == InputState, 3]) %*% randomlist)
  }
  randomlist <- rnorm(31, mean = 0, sd = 1)
  numseatsstates <- simulationstates %>%
    rowwise() %>%
    mutate(augmented_z_score = new_z_score(State)) %>%
    mutate(newresult = meanresult + augmented_z_score * sd)
  
  return(numseatsstates$newresult)

}

#how many times to run the simulation
numofsimulations <- 10000
simulation <- data.frame(replicate(numofsimulations, numseats()))
statessimulationresult <- simulation %>%
  rowwise() %>%
  #get average result for every state
  mutate(avgresult = mean(c_across(cols = everything()))) %>% 
  #get chance of (democrat) winning for every state
  mutate(chanceofwinning = case_when(
    avgresult > 15 ~ 1,
    avgresult < -15 ~ 0,
    TRUE ~ (sum(c_across(cols = everything()) > 0) / numofsimulations))) %>%
  add_column(state = stateslist, .before = "avgresult") %>%
  select(-c(1:numofsimulations)) %>% 
  add_row(state = states[states$State == "Oklahoma (special)", 1][[1]][[1]], avgresult = states[states$State == "Oklahoma (special)", 2][[1]][[1]], chanceofwinning = states[states$State == "Oklahoma (special)", 4][[1]][[1]]) %>%
  add_row(state = states[states$State == "Utah", 1][[1]][[1]], avgresult = states[states$State == "Utah", 2][[1]][[1]], chanceofwinning = states[states$State == "Utah", 4][[1]][[1]]) %>%
  add_row(state = states[states$State == "Alaska", 1][[1]][[1]], avgresult = states[states$State == "Alaska", 2][[1]][[1]], chanceofwinning = states[states$State == "Alaska", 4][[1]][[1]]) %>%
  add_row(state = states[states$State == "Louisiana", 1][[1]][[1]], avgresult = states[states$State == "Louisiana", 2][[1]][[1]], chanceofwinning = states[states$State == "Louisiana", 4][[1]][[1]])
  
  
#get vector with the number of seats dems get every runthrough
listofresults <- simulation %>%
  apply(2,function(x) sum(x > 0) + 36)



write_sheet(as.data.frame(listofresults), ss = as_sheets_id("https://docs.google.com/spreadsheets/d/1gXGn5vKjhYyhi2-38qiH4Ps5qMvppXg3JFSVYijZLFU/edit#gid=800772858"), sheet = 18)

