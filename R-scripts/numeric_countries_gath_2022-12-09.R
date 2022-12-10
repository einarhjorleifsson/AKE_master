# 2022-12-09: "hh_type" occurs in two columns in excel. the first one is in 
#             column 2 and deals with house type. the second one, wich is 
#             sometimes labelled hh_type_column-number or hh_type_c stands hfor 
#             a family type class. Here the second column is returned as bq_class
library(tidyverse)
library(readxl)
library(janitor)
library(corrplot)
library(sf)
source("R/reads.R")

# import -----------------------------------------------------------------------
files <- 
  c("data-raw/Denmark_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
    "data-raw/Finland_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
    "data-raw/Iceland_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
    "data-raw/Norway_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
    "data-raw/Sweden_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx")


num <-
  map_df(files, read_numeric) |> 
  mutate(bq_age = as.numeric (bq_age))


# add dgurba level 2
dg2 <- 
  read_sf("data/spatial/DGURBA2.gpkg") |> 
  st_transform(crs = 4326) |> 
  mutate(DEGURBA2 = as.integer(DEGURBA2))

num <- 
  num |> 
  separate(col = bq_geoloc, into = c("lat", "lon"), sep = ";", convert = TRUE) |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326,
           remove = FALSE) |> 
  st_join(dg2) |> 
  st_drop_geometry()

num |> glimpse()


#removing outliers

num |> 
  select(.rid, .cntr, cf_footprint_ex_pm) |> 
  group_by(.cntr) |> 
  summarise(min = min(cf_footprint_ex_pm),
            q25 = quantile(cf_footprint_ex_pm, 0.25),
            q75 = quantile(cf_footprint_ex_pm, 0.75),
            max = max(cf_footprint_ex_pm),
            .groups = "drop") |>
  mutate(lo = q25 - ((q75 - q25) * 2.2),
         hi = q25 + ((q75 - q25) * 2.2))


sum.num <- 
  num |> 
  #select(.rid, .cntr, cf_footprint_ex_pm) |> 
  group_by(.cntr) |> 
  summarise(min = min(cf_footprint_ex_pm),
            q25 = quantile(cf_footprint_ex_pm, 0.25),
            q75 = quantile(cf_footprint_ex_pm, 0.75),
            min = min(cf_footprint_ex_pm),
            max = max(cf_footprint_ex_pm),
            lo = q25 - ((q75 - q25) * 2.2),
            hi = q75 + ((q75 - q25) * 2.2),
            n = n(),
            n.above = sum(cf_footprint_ex_pm > hi),
            percent.above = round(n.above / n * 100, 2),
            .groups = "drop")

find_fravillingar <- function(x, multiplier = 2.2) {
  q25 <- quantile(x, 0.25)
  q75 <- quantile(x, 0.75)
  lo <- q25 - ((q75 - q25) * multiplier)
  hi <- q75 + ((q75 - q25) * multiplier)
  fravillingur <- x > hi
  return(fravillingur)
}
num <- 
  num |> 
  group_by(.cntr) |> 
  mutate(cf_footprint_ex_pm_fravillingur = find_fravillingar(x = sqrt(cf_footprint_ex_pm), multiplier = 2.2),
         cu_cf_footprint_ex_pm_fravillingur = find_fravillingar(cu_cf_footprint_ex_pm)) |> 
  ungroup()



# flokka saman aldursbil mutate/cut

num <-
  num |> 
  mutate (bq_age_gath = case_when(bq_age %in% 0:36 ~ 1,
                                  bq_age %in% 37:50 ~ 2,
                                  bq_age %in% 51:60 ~ 3,
                                  bq_age %in% 61:80 ~ 4))
  
num |> 
  count(bq_age_gath, .cntr) |> 
  spread(bq_age_gath, n)

num <-
  num |> 
  mutate (bq_edu_gath = case_when(bq_educati %in% 1:3 ~ 1,
                                  bq_educati %in% 4 ~ 2,
                                  bq_educati %in% 5:6 ~ 3))

num <-
  num |> 
  mutate(bq_gender_gath = case_when(bq_gender == 2~2,
                                    bq_gender == 3~1,
                                    bq_gender %in% c(1,4)~3))
num |> 
  count(bq_gender, bq_gender_gath)

num |> write_csv("data-csv/numeric-merged-countries_2022-12-09.csv")

