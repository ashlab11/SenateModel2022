library(googledrive)
library(googlesheets4)
library(tidyverse)

senate_sheets <- drive_get("Senate Data")
states <- senate_sheets %>%
  range_read(sheet = 1)
priors <- senate_sheets %>%
  range_read(sheet = 2)
results2018 <- senate_sheets %>%
  range_read(sheet = 3)
houseresults2018 <- senate_sheets %>%
  range_read(sheet = 4)
days_away <- 60

#IMPORTING IN PAST RATINGS
ratings2014 <- read_tsv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/2014/pollster-ratings.tsv")) %>%
  select(c(2, 12))
colnames(ratings2014)[2] <- "Mean.Reverted.Bias"

ratings2016 <- read.csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/2016/pollster-ratings.csv")) %>%
  select(c(2, 14))

ratings2018 <- read.csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/2018/pollster-ratings.csv")) %>%
  separate(Mean.Reverted.Bias, sep = " +", into = c("party", "Mean.Reverted.Bias")) %>%
  mutate(Mean.Reverted.Bias = case_when(
    party == "D" ~ as.double(Mean.Reverted.Bias),
    party == "R" ~ -1 * as.double(Mean.Reverted.Bias)
  )) %>%
  select(c(1, "Mean.Reverted.Bias"))

ratings2020 <- read.csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/2020/pollster-ratings.csv")) %>%
  select(c(1, 10)) %>%
  separate(Mean.Reverted.Bias, sep = " +", into = c("party", "Mean.Reverted.Bias")) %>%
  mutate(Mean.Reverted.Bias = case_when(
    party == "D" ~ as.double(Mean.Reverted.Bias),
    party == "R" ~ -1 * as.double(Mean.Reverted.Bias)
  )) %>%
  select(c(1, "Mean.Reverted.Bias"))

masterratings <- read.csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/pollster-ratings.csv")) %>%
  select(c(2, 9))


#IMPORTING IN DATA
prespolls2020 <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/data/president_polls_historical.csv"))
prespolls2016 <- read.csv(url("http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv"))
historicalsenatepolls <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/data/senate_polls_historical.csv"))
govpolls2018 <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/data/governor_polls_historical.csv"))
housepolls2018 <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/data/house_polls_historical.csv"))
currentsenatepolls <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv"))
genballotpolls <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/data/generic_ballot_polls.csv"))
prespolls2020 

#function to weigh polls based on how far away they were created
pollweights <- function(faraway) {
  return(1 / (1 + exp(faraway - 30)))
}

#AVERAGE OF CURRENT SENATE ELECTIONS, WITHOUT OKLAHOMA SPECIAL
currentsenatepollsnooklahoma <- currentsenatepolls %>%
  #only takes polls within 30 days of the election, with grades of C and above
  filter((Sys.Date() - as.Date(end_date, "%m/%d/%y")) < days_away & !(fte_grade %in% c("C/D", "D"))) %>%
  #weighing every poll according to how far away it is
  mutate(weights = pollweights(as.numeric(Sys.Date() - as.Date(end_date, "%m/%d/%y")))) %>%
  #taking out Oklahoma special
  filter(!(candidate_name %in% c("Kendra Horn", "Markwayne Mullin", "Robert Murphy", "Ray Woods"))) %>%
  #only taking partisan polls if we have an error for them
  filter(!(partisan %in% c("DEM", "REP") & !(pollster_rating_name %in% masterratings$Pollster))) %>%
  #getting the sum of percents for each party for every race
  group_by(poll_id, pollster_rating_name, state, population, party, end_date, weights) %>%
  summarize(pct = sum(pct)) %>%
  pivot_wider(names_from = party, values_from = pct) %>%
  #getting margin
  mutate(margin = case_when(
    state == "Utah" ~ IND - REP,
    TRUE ~ DEM - REP))  %>%
  select("poll_id", "pollster_rating_name", "state", "end_date", "population", "margin", "weights") %>%
  # #getting the best group of voters for every poll
  mutate(population = case_when(
    population == "lv" ~ 4,
    population == "rv" ~ 3,
    population == "v" ~ 2,
    population == "a" ~ 1
  )) %>%
  group_by(poll_id) %>%
  filter(population == max(population)) %>%
  #adding in the bias for every poll
  left_join(masterratings, by = c("pollster_rating_name" = "Pollster")) %>%
  replace_na(list(Mean.Reverted.Bias = 0)) %>%
  #getting margin - bias for each state
  mutate(result = margin - Mean.Reverted.Bias) %>%
  #getting polling average for each state
  group_by(state) %>%
  summarize(margin = weighted.mean(result, weights))

#AVERAGE OF CURRENT OKLAHOMA SPECIAL ELECTIONS
okspecial <- currentsenatepolls %>%
  #only takes polls within 30 days of the election, with grades of C and above
  filter((Sys.Date() - as.Date(end_date, "%m/%d/%y")) < days_away & !(fte_grade %in% c("C/D", "D"))) %>%
  #taking out Oklahoma special
  filter(candidate_name %in% c("Kendra Horn", "Markwayne Mullin", "Robert Murphy", "Ray Woods")) %>%
  #weighing every poll according to how far away it is
  mutate(weights = pollweights(as.numeric(Sys.Date() - as.Date(end_date, "%m/%d/%y")))) %>%
  group_by(poll_id, pollster_rating_name, state, population, party, end_date, weights) %>%
  summarize(pct = sum(pct)) %>%
  pivot_wider(names_from = party, values_from = pct) %>%
  #getting margin
  mutate(margin = DEM - REP) %>%
  select("poll_id", "pollster_rating_name", "state", "end_date", "population", "margin", "weights") %>%
  #getting the best group of voters for every poll
  mutate(population = case_when(
    population == "lv" ~ 4,
    population == "rv" ~ 3,
    population == "v" ~ 2,
    population == "a" ~ 1
  )) %>%
  group_by(poll_id, pollster_rating_name, state, end_date) %>%
  filter(population == max(population)) %>%
  group_by(state) %>%
  summarize(margin = weighted.mean(margin, weights)) %>%
  mutate(state = "Oklahoma (special)")

#ALASKA POLLING
alaskapolls <- currentsenatepolls %>%
  #only takes polls within 40 days of the election, with grades of C and above, from the state Alaska
  filter(state == "Alaska" & (Sys.Date() - as.Date(end_date, "%m/%d/%y")) < days_away & !(fte_grade %in% c("C/D", "D"))) %>%
  #weighing every poll according to how far away it is
  mutate(weights = pollweights(as.numeric(Sys.Date() - as.Date(end_date, "%m/%d/%y")))) %>%
  group_by(poll_id, pollster_rating_name, state, population, candidate_name, end_date, weights) %>%
  summarize(pct = max(pct)) %>%
  pivot_wider(names_from = candidate_name, values_from = pct) %>%
  #getting margin
  mutate(margin = `Lisa Murkowski` - `Kelly C. Tshibaka`) %>%
  select("poll_id", "pollster_rating_name", "state", "end_date", "population", "margin", "weights") %>%
  #getting the best group of voters for every poll
  mutate(population = case_when(
    population == "lv" ~ 4,
    population == "rv" ~ 3,
    population == "v" ~ 2,
    population == "a" ~ 1
  )) %>%
  group_by(poll_id, pollster_rating_name, state, end_date) %>%
  filter(population == max(population)) %>%
  group_by(state) %>%
  summarize(margin = weighted.mean(margin, weights)) %>%
  mutate(state = "Alaska") 

#Utah current polls:

#CURRENT SENATE POLLS
currentpolls <- bind_rows(currentsenatepollsnooklahoma, okspecial) %>%
  filter(!(state == "Alaska")) %>%
  bind_rows(alaskapolls)

#GEN BALLOT POLLS
genballotpolls <- genballotpolls %>%
  #only takes polls within 30 days of the election, with grades of C and above
  filter((Sys.Date() - as.Date(end_date, "%m/%d/%y")) < days_away & !(fte_grade %in% c("C/D", "D", "D-", "D+", "D/F", "F"))) %>%
  #weighing every poll according to how far away it is
  mutate(weights = pollweights(as.numeric(Sys.Date() - as.Date(end_date, "%m/%d/%y")))) %>%
  #getting margin
  mutate(margin = dem - rep)  %>%
  select("poll_id", "pollster_rating_name", "end_date", "population", "margin", "weights") %>%
  # #getting the best group of voters for every poll
  mutate(population = case_when(
    population == "lv" ~ 4,
    population == "rv" ~ 3,
    population == "v" ~ 2,
    population == "a" ~ 1
  )) %>%
  group_by(poll_id) %>%
  filter(population == max(population)) %>%
  #adding in the bias for every poll
  left_join(masterratings, by = c("pollster_rating_name" = "Pollster")) %>%
  replace_na(list(Mean.Reverted.Bias = 0)) %>%
  #getting margin - bias for each state
  mutate(result = margin - Mean.Reverted.Bias)

genballotresult <- weighted.mean(genballotpolls$result, genballotpolls$weights)

#AVERAGE OF 2020 ELECTIONS
pres2020 <- prespolls2020 %>%
  #only takes polls within 30 days of the election, with grades of C and above
  filter((as.Date(election_date, "%m/%d/%y") - as.Date(end_date, "%m/%d/%y")) < days_away & !(fte_grade %in% c("C/D", "D", "D-", "D+", "D/F", "F"))) %>%
  filter(!(state %in% c("", "District of Columbia", "Maine CD-1", "Maine CD-2", "Nebraska CD-2"))) %>%
  #weighing every poll according to how far away it is
  mutate(weights = pollweights(as.numeric(as.Date(election_date, "%m/%d/%y") - as.Date(end_date, "%m/%d/%y")))) %>%
  #getting the sum of percents for each party for every race
  group_by(poll_id, pollster_rating_name, state, population, party, end_date, weights) %>%
  summarize(pct = sum(pct)) %>%
  pivot_wider(names_from = party, values_from = pct) %>%
  #getting margin
  mutate(margin = DEM - REP) %>%
  select("poll_id", "pollster_rating_name", "state", "end_date", "population", "margin", "weights") %>%
  #getting the best group of voters for every poll
  mutate(population = case_when(
    population == "lv" ~ 4,
    population == "rv" ~ 3,
    population == "v" ~ 2,
    population == "a" ~ 1
  )) %>%
  group_by(poll_id) %>%
  filter(population == max(population)) %>%
  #adding in the bias for every poll
  group_by(poll_id, pollster_rating_name, state, end_date, population, margin) %>%
  left_join(ratings2018, by = c("pollster_rating_name" = "Pollster")) %>%
  replace_na(list(Mean.Reverted.Bias = 0)) %>%
  #getting margin - bias for each state
  mutate(result = margin - Mean.Reverted.Bias) %>%
  #getting polling average for each state
  group_by(state) %>%
  summarize(polls2020 = weighted.mean(result, weights))
  

#AVERAGE OF 2018 ELECTIONS
senate2018 <- historicalsenatepolls %>%
  filter(abs(as.Date("11/6/18", "%m/%d/%y") - as.Date(end_date, "%m/%d/%y")) < days_away & !(fte_grade %in% c("C/D", "D", "D-", "D+", "D/F", "F"))) %>%
  #weighing every poll according to how far away it is
  mutate(weights = pollweights(as.numeric(as.Date("11/6/18", "%m/%d/%y") - as.Date(end_date, "%m/%d/%y")))) %>%
  #getting the sum of percents for each party for every race
  filter(state %in% states$State) %>%
  group_by(poll_id, pollster_rating_name, state, population, party, end_date, weights) %>%
  summarize(pct = sum(pct)) %>%
  pivot_wider(names_from = party, values_from = pct) %>%
  #getting margin
  mutate(margin = case_when(
    state == "Vermont" ~ IND - REP,
    TRUE ~ DEM - REP
  ))  %>%
  select("poll_id", "pollster_rating_name", "state", "end_date", "population", "margin", "weights") %>%
  #getting the best group of voters for every poll
  mutate(population = case_when(
    population == "lv" ~ 4,
    population == "rv" ~ 3,
    population == "v" ~ 2,
    population == "a" ~ 1
  )) %>%
  group_by(poll_id) %>%
  filter(population == max(population)) %>%
  #adding in the bias for every poll
  left_join(ratings2016, by = c("pollster_rating_name" = "Pollster")) %>%
  replace_na(list(Mean.Reverted.Bias = 0)) %>%
  #getting margin - bias for each state
  mutate(result = margin - Mean.Reverted.Bias) %>%
  #getting polling average for each state
  group_by(state) %>%
  summarize(polls2018 = weighted.mean(result, weights)) %>%
  filter(!(state == "California"))


#GOVERNOR RACES 2018
gov2018 <- govpolls2018 %>%
  filter((abs(as.Date("11/6/18", "%m/%d/%y") - as.Date(end_date, "%m/%d/%y")) < days_away | state == "Alabama") & !(fte_grade %in% c("C/D", "D", "D-", "D+", "D/F", "F"))) %>%
  filter(state %in% c("Alabama", "Alaska", "Arkansas", "California", "Colorado", "Georgia", "Idaho", "Illinois", "Iowa", "Kansas", "New Hampshire", 
                      "Oregon", "South Carolina", "South Dakota")) %>%
  #getting the sum of percents for each party for every race
  mutate(weights = pollweights(as.numeric(as.Date("11/6/18", "%m/%d/%y") - as.Date(end_date, "%m/%d/%y")))) %>%
  group_by(poll_id, pollster_rating_name, state, population, party, end_date, weights) %>%
  summarize(pct = sum(pct)) %>%
  pivot_wider(names_from = party, values_from = pct) %>%
  #getting margin
  mutate(margin = DEM - REP)  %>%
  select("poll_id", "pollster_rating_name", "state", "end_date", "population", "margin") %>%
  #getting the best group of voters for every poll
  mutate(population = case_when(
    population == "lv" ~ 4,
    population == "rv" ~ 3,
    population == "v" ~ 2,
    population == "a" ~ 1
  )) %>%
  group_by(poll_id) %>%
  filter(population == max(population)) %>%
  #adding in the bias for every poll
  group_by(poll_id, pollster_rating_name, state, end_date, population, margin) %>%
  left_join(ratings2016, by = c("pollster_rating_name" = "Pollster")) %>%
  replace_na(list(Mean.Reverted.Bias = 0)) %>%
  #getting margin - bias for each state
  mutate(result = margin - Mean.Reverted.Bias) %>%
  #getting polling average for each state
  group_by(state) %>%
  summarize(polls2018 = mean(result)) %>%
  filter(state %in% c("Alabama", "Alaska", "Arkansas", "California", "Colorado", "Georgia", "Idaho", "Illinois", "Iowa", "Kansas", "New Hampshire",
                      "Oregon", "South Carolina", "South Dakota", "Wisconsin"))

govsenate2018 <- bind_rows(senate2018, gov2018)
#AVERAGE OF THE HOUSE RACE POLLS IN EACH OF THE THREE STATES
house2018 <- housepolls2018 %>%
  filter(cycle == 2018 & state %in% c("Kentucky", "Louisiana", "North Carolina")) %>%
  filter(abs(as.Date("11/6/18", "%m/%d/%y") - as.Date(end_date, "%m/%d/%y")) < days_away & !(fte_grade %in% c("C/D", "D", "D-", "D+", "D/F", "F"))) %>%
  mutate(weights = pollweights(as.numeric(as.Date("11/6/18", "%m/%d/%y") - as.Date(end_date, "%m/%d/%y")))) %>%
  select("poll_id", "pollster_rating_name", "state", "population", "seat_number", "party", "pct", "weights") %>%
  group_by(poll_id, pollster_rating_name, state, population, party, seat_number, weights) %>%
  summarize(pct = sum(pct)) %>%
  pivot_wider(names_from = party, values_from = pct) %>%
  mutate(margin = DEM - REP) %>%
  mutate(population = case_when(
    population == "lv" ~ 4,
    population == "rv" ~ 3,
    population == "v" ~ 2,
    population == "a" ~ 1
  )) %>%
  group_by(poll_id) %>%
  filter(population == max(population)) %>%
  left_join(ratings2016, by = c("pollster_rating_name" = "Pollster")) %>%
  replace_na(list(Mean.Reverted.Bias = 0)) %>%
  #getting margin - bias for each state
  mutate(result = margin - Mean.Reverted.Bias) %>%
  group_by(state, seat_number) %>%
  summarize(polls2018 = weighted.mean(result, weights))
  

#AVERAGE OF 2016 ELECTIONS 
pres2016 <- prespolls2016 %>%
  #only takes polls within 30 days of the election, with grades of C and above
  filter((as.Date("11/8/16", "%m/%d/%y") - as.Date(enddate, "%m/%d/%Y")) < days_away & !(grade %in% c("C/D", "D", "D-", "D+", "D/F", "F"))
         & type == "polls-only") %>%
  mutate(margin = rawpoll_clinton - rawpoll_trump) %>%
  mutate(weights = pollweights(as.numeric(as.Date("11/8/16", "%m/%d/%y") - as.Date(enddate, "%m/%d/%Y")))) %>%
  select("poll_id", "pollster", "state", "enddate", "population", "margin", "weights") %>%
  # #getting the best group of voters for every poll
  mutate(population = case_when(
    population == "lv" ~ 4,
    population == "rv" ~ 3,
    population == "v" ~ 2,
    population == "a" ~ 1
  )) %>%
  group_by(poll_id) %>%
  filter(population == max(population)) %>%
  #adding in the bias for every poll
  left_join(ratings2014, by = c("pollster" = "Pollster")) %>%
  replace_na(list(Mean.Reverted.Bias = 0)) %>%
  #getting margin - bias for each state
  mutate(result = margin - Mean.Reverted.Bias) %>%
  #getting polling average for each state
  group_by(state) %>%
  summarize(polls2016 = weighted.mean(result, weights)) %>%
  filter(!(state %in% c("", "U.S.", "District of Columbia", "Maine CD-1", "Maine CD-2", "Nebraska CD-2")))



#GETTING POLLING ERROR


#Note for error: if DEM wins by 1 point, but polls showed an even race, error will be 1
#So, to calculate for polling error, ADD the past polling error to current races

error2016 <- pres2016 %>%
  inner_join(priors, by = c("state" = "State"), copy = TRUE) %>%
  select(-c("result2018", "result2020")) %>%
  mutate(error2016 = result2016 - polls2016, .keep = "unused")

error2020 <- pres2020 %>%
  inner_join(priors, by = c("state" = "State"), copy = TRUE) %>%
  select(-c("result2016", "result2018")) %>%
  mutate(error2020 = result2020 - polls2020, .keep = "unused")

error2018govsenate <- govsenate2018 %>%
  inner_join(results2018, by = c("state" = "State")) %>%
  mutate(error2018 = result2018 - polls2018, .keep = "unused")

error2018house <- house2018 %>%
  inner_join(houseresults2018, by = c("state" = "State", "seat_number" = "District")) %>%
  mutate(error2018 = result2018 - polls2018, .keep = "unused") %>%
  group_by(state) %>%
  summarize(error2018 = mean(error2018))

error2018 <- bind_rows(error2018govsenate, error2018house)

poll_error <- states %>%
  inner_join(error2016, by = c("State" = "state")) %>%
  left_join(error2018, by = c("State" = "state")) %>%
  inner_join(error2020, by = c("State" = "state"))


#if a state doesn't have polling error for 2018, make it the average of 2016 and 2020 -- only happens for HI, LA, OK
poll_error[is.na(poll_error$error2018), 3] <- (poll_error[is.na(poll_error$error2018), 2] + poll_error[is.na(poll_error$error2018), 4])/2

#adding a row for Oklahoma (special) that is the same as Oklahoma
poll_error <- poll_error %>%
  add_row(State = "Oklahoma (special)", error2016 = -13.3297360, error2018 = -10.3271441, error2020 = -7.32455232)
  

