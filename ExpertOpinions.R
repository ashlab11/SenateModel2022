library(googledrive)
library(googlesheets4)
library(tidyverse)

senate_sheets <- drive_get("Senate Data")

past_experts <- senate_sheets %>%
  range_read(sheet = 8) 
current_experts <- senate_sheets %>%
  range_read(sheet = 9, skip = 1) 

#creating a linreg based on past expert ratings, using Lean D, Lean R, Likely D, and Likely R
# tilt D counts as 0.5 Lean D, and same for Tilt R
past_experts <- past_experts %>%
  #only doing the linreg on states that had margins of <8 points
  filter(abs(Margin) < 8) 
expertlm <- lm(Margin ~ `Lean D (no tilt)` + `Lean R (no tilt)` + `Likely D` + `Likely R` + `Year` + 0, data = past_experts)
expertcoefficients <- summary(expertlm)$coefficients[1:4, 1]

#sum the cov of all the coefficients
varexpert <- sum(vcov(expertlm))

#applying the linreg to this year's expert ratings
current_experts <- current_experts %>%
  rowwise() %>%
  mutate(margin = sum(c_across(15:18) * expertcoefficients)) %>%
  inner_join(priors, by = c("State" = "State")) %>%
  filter(abs(result2016 * 0.3 + result2018 * 0.1 + result2020 * 0.6) < 8) %>%
  select("State", "margin")
