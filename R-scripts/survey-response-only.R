# Input: see files below
# Output: data-csv/survey-response-only.csv

library(tidyverse)
source("R/reads.R")
source("R/tidy.R")

# survey response only ---------------------------------------------------------
# 2022-12-04: Only the actual survey responses
files <-
  c("data-raw/Denmark_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
    "data-raw/Finland_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
    "data-raw/Iceland_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
    "data-raw/Norway_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
    "data-raw/Sweden_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx")

# read in all countries raw data in one go
raw <- 
  map_df(files, read_data) |> 
  # remove some strange variables
  select(-c(cf_fi, cf_income_level)) |> 
  filter(!is.na(.rid))
# only the survey response variables
sur <- 
  raw |> 
  select(.rid:comment, .cntr) |> 
  # a peculiar errors, mostly likely some excel slips
  mutate(diet_meat = ifelse(diet_meat == "none", "None", diet_meat)) 

sur |> write_csv("data-csv/survey-response-only.csv")
