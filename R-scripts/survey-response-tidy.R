# Input:  data-csv/survey-response-only.csv
# Output: data-csv/survey-response-tidy.csv

library(tidyverse)
source("R/tidy.R")

sur <- 
  read_csv("data-csv/survey-response-only.csv",
           guess_max = 1e6)

# fix "less", "more", "older" --------------------------------------------------
sur <- 
  sur |> 
  # replace "none" and "5ormore" in some of the variables
  mutate(bq_adult   = td_none_and_5ormore(bq_adult),
         corr = ifelse(bq_adult == 0, "bq_adult_0->1", NA),
         # Assume there is always one adult
         bq_adult   = ifelse(bq_adult == 0, 1, bq_adult),
         bq_child   = td_none_and_5ormore(bq_child),
         bq_infant  = td_none_and_5ormore(bq_infant),
         pets_cats  = td_none_and_5ormore(pets_cats),
         pets_dogs  = td_none_and_5ormore(as.character(pets_dogs)),
         pets_other = td_none_and_5ormore(pets_other)
  ) |> 
  # fix age
  mutate(bq_age = case_when(bq_age == "15 or less" ~ "15",
                            bq_age == "80 or more" ~ "80",
                            TRUE ~ bq_age),
         bq_age = as.integer(bq_age))
# we can check the "correction-log" by:
sur |> count(corr)

# calculate "decile" income (acturally 11th-tiles) -----------------------------
#  a little helper function to remove text from bq_p_gross and bq_h_gross
#   used in the next two "chunks"
td_strip_currency <- function(x) {
  x <-
    x |> 
    str_replace(" DKK/month", "") |> 
    str_replace(" €/month", "") |> 
    str_replace(" ISK/month", "") |> 
    str_replace(" NOK/month", "") |> 
    str_replace(" SEK/month", "") |> 
    str_replace("Less than ", "") |> 
    str_replace("More than ", "") |>  
    str_replace_all("\\.", "") |> 
    str_replace("–", " –") |> 
    str_squish()
  return(x)
}

## personal gross --------------------------------------------------------------
# create a lookup table containing the 11 incomes levels per country, the 
#  low, mid and upper range of the values as well as rank (1-11)
lookup_bq_p_gross <- 
  sur |> 
  mutate(x = td_strip_currency(bq_p_gross)) |> 
  separate(x, into = c("lo", "hi"), sep = " – ") |> 
  mutate(hi = str_replace_all(hi, " ", ""),
         lo = str_replace_all(lo, " ", ""),
         hi = as.integer(hi),
         lo = as.integer(lo)) |> 
  mutate(mid = (lo + hi) / 2,
         mid = ifelse(is.na(mid), lo, mid)) |> 
  rename(bq_p_gross_hi = hi,
         bq_p_gross_lo = lo,
         bq_p_gross_mi = mid) |> 
  select(.cntr, bq_p_gross, bq_p_gross_lo, bq_p_gross_mi, bq_p_gross_hi) |> 
  distinct() |> 
  arrange(.cntr, bq_p_gross_mi) |> 
  group_by(.cntr) |> 
  mutate(bq_p_gross_11 = 1:n()) |> 
  ungroup() |> 
  mutate(bq_p_gross_hi = ifelse(bq_p_gross_11 ==  1, bq_p_gross_mi, bq_p_gross_hi),
         bq_p_gross_lo = ifelse(bq_p_gross_11 ==  1,          -Inf, bq_p_gross_lo),
         bq_p_gross_hi = ifelse(bq_p_gross_11 == 11,           Inf, bq_p_gross_hi),
         bq_p_gross_lo = ifelse(bq_p_gross_11 == 11, bq_p_gross_mi, bq_p_gross_lo))
# lookup_bq_p_gross |> write_csv("data-csv/lookup_bq_p_gross.csv")

## household gross -------------------------------------------------------------
# same as above except now for bq_h_gross
lookup_bq_h_gross <- 
  sur |> 
  filter(!str_starts(bq_h_gross, "Same")) |> 
  mutate(x = td_strip_currency(bq_h_gross)) |>
  select(.cntr, bq_h_gross, x) |> 
  separate(x, into = c("lo", "hi"), sep = " – ") |> 
  mutate(hi = str_replace_all(hi, " ", ""),
         lo = str_replace_all(lo, " ", ""),
         hi = as.integer(hi),
         lo = as.integer(lo)) |> 
  mutate(mid = (lo + hi) / 2,
         mid = ifelse(is.na(mid), lo, mid)) |> 
  rename(bq_h_gross_hi = hi,
         bq_h_gross_lo = lo,
         bq_h_gross_mi = mid) |> 
  select(.cntr, bq_h_gross, bq_h_gross_lo, bq_h_gross_mi, bq_h_gross_hi) |> 
  distinct() |> 
  arrange(.cntr, bq_h_gross_mi) |> 
  group_by(.cntr) |> 
  mutate(bq_h_gross_11 = 1:n()) |> 
  ungroup() |> 
  mutate(bq_h_gross_hi = ifelse(bq_h_gross_11 ==  1, bq_h_gross_mi, bq_h_gross_hi),
         bq_h_gross_lo = ifelse(bq_h_gross_11 ==  1,          -Inf, bq_h_gross_lo),
         bq_h_gross_hi = ifelse(bq_h_gross_11 == 11,           Inf, bq_h_gross_hi),
         bq_h_gross_lo = ifelse(bq_h_gross_11 == 11, bq_h_gross_mi, bq_h_gross_lo))
# lookup_bq_h_gross |> write_csv("data-csv/lookup_bq_h_gross.csv")

# Correcting bq_adult ----------------------------------------------------------
# From the excel log-file
# -Corrected for "None" answers for question "Number of adults (over 18 years old) 
#   living in your household*? (Including yourself!)" updated to 1
# -Updated to match the respondent vs. household income. If household income is 
#  reported as higher than the personal income and the personal and household 
#  income deciles did not overlap, updated to 2 if the original response was 1. 
# -Updated the number of adults with the above two corrections (adults_corrected)
#
# NOTE: the overlapping is understood to means the lower_upper ranges of the 
#  two income ranges. like
#  bq_p_gross: ------lower______uppper----------------- ....
#  bq_h_gross: -------------------------lower_____upper
#
# this takes a while ...
bq_adult_corrections <-
  sur |> 
  filter(!str_starts(bq_h_gross, "Same"), bq_adult == 1) |> 
  select(.rid, .cntr, bq_adult, bq_p_gross, bq_h_gross) |> 
  left_join(lookup_bq_p_gross) |> 
  left_join(lookup_bq_h_gross |> select(.cntr, bq_h_gross, lo = bq_h_gross_lo, hi = bq_h_gross_hi)) |> 
  rowwise() |>
  # need to check overlaps twice
  mutate(overlaps = case_when(between(lo, bq_p_gross_lo, bq_p_gross_hi) | 
                                between(hi, bq_p_gross_lo, bq_p_gross_hi)  ~ "overlap",
                              between( bq_p_gross_lo, lo, hi) | 
                                between( bq_p_gross_hi, lo, hi)  ~ "overlap2",
                              TRUE ~ "none")) |> 
  ungroup()
bq_adult_corrections |> count(overlaps)
# so we are finding 341 responses where the number of adults is reported as 1
#  but where there is no overlap in the gross personal vs gross household income
# one can glimpse them via:
bq_adult_corrections |> 
  filter(overlaps == "none") |> 
  glimpse()
# add these records to the survey
#  NOTE: Here bq_adult is overwritten, but the a new variable "correction" keeps
#        account of what was done
sur <- 
  sur |> 
  left_join(bq_adult_corrections |> 
              filter(overlaps == "none") |> 
              select(.rid, .cntr) |> 
              mutate(corrections = "bq_adult_1->2")) |> 
  mutate(bq_adult = case_when(corrections == "bq_adult_1->2" ~ bq_adult + 1,
                              TRUE ~ bq_adult),
         corr = case_when(!is.na(corr) & !is.na(corrections) ~ paste0(corr, "_", corrections),
                          TRUE ~ corr)) |> 
  select(-corrections)
# we can check the "correction-log" by:
sur |> count(corr)
                          

# Add the personal gross income decile -----------------------------------------
sur <- 
  sur |> 
  left_join(lookup_bq_p_gross |> select(.cntr, bq_p_gross, bq_p_gross_11))

# Add the household gross income decile ----------------------------------------
#  Here we need to take into account the "Same as personal gross income"
#  First to the simple one (where we do not have the "Same as ...")
sur <- 
  sur |> 
  left_join(lookup_bq_h_gross |> select(.cntr, bq_h_gross, bq_h_gross_11))
# so how many are still missing
sur |> 
  count(bq_h_gross_11)
# thats 1326 respondees. These should all be with bq_h_gross as "Same as ....":
sur |> 
  filter(is.na(bq_h_gross_11)) |> 
  pull(bq_h_gross) |> 
  unique()

## Dealing with houshold gross the same as personal gross ----------------------
# Here we need to find the household decile (11th-ile) for where bq_h_gross is 
# "Same as personal gross income"
# Ideally one should replace "Same as ..." with the corresponding bq_h_gross.
#  At the moment that is not done below, only the bg_h_gross_11 is dealt with
# A bit of a "Krísuvíkurleið" done here in the coding but things seems to work.
# This takes quite a while, need to find a smarter way
#  Only seems to work on the developmental version of dplyr
bq_h_gross_same_as_personal_gross_income <- 
  sur |> 
  filter(bq_h_gross == "Same as personal gross income") |> 
  select(.rid, .cntr, bq_p_gross, bq_h_gross) |> 
  left_join(lookup_bq_p_gross) |> 
  # distinct() |> 
  arrange(.cntr, bq_p_gross_11) |> 
  #filter(.cntr == "DK") |> 
  left_join(lookup_bq_h_gross |> select(.cntr, lo = bq_h_gross_lo, hi = bq_h_gross_hi, bhg_11 = bq_h_gross_11)) |> 
  rowwise() |>
  mutate(.match = case_when(between(bq_p_gross_lo, lo, hi) & between(bq_p_gross_hi, lo, hi) ~ bhg_11,
                            between(bq_p_gross_mi, lo, hi) ~ bhg_11,
                            TRUE ~ -9)) |> 
  ungroup() |> 
  filter(.match > 0)

bq_h_gross_same_as_personal_gross_income |> 
  glimpse()

# things seem to be ok, the bq_h_gross == "Same as personal gross income" is
#   1326 rows, and the output of bq_h_gross_same_as_personal_gross_income is 
#   the same number of rows
# Again though, a nicer algorithm would be kosher

sur <- 
  sur |> 
  left_join(bq_h_gross_same_as_personal_gross_income |> 
              select(.rid, .cntr, bhg_11)) |> 
  mutate(bq_h_gross_11 = ifelse(is.na(bq_h_gross_11) & !is.na(bhg_11),
                                bhg_11,
                                bq_h_gross_11)) |> 
  select(-bhg_11)

# MORE CODE MOST LIKELY TO BE ADDED --------------------------------------------

sur |> write_csv("data-csv/survey-response-tidy.csv")
