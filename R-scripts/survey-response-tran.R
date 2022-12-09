# Input:  data-csv/survey-response-tidy.csv
# Output: data-csv/survey-response-tran.csv

# NOTE: all names of transformed variable start (for now) with "."

library(sf)
library(tidyverse)
source("R/carbon-footprint.R")

# INPUT ------------------------------------------------------------------------
sur <- read_csv("data-csv/survey-response-tidy.csv", guess_max = 1e6)

# household --------------------------------------------------------------------
# household unit
sur <- 
  sur |> 
  mutate(bq_hu = bq_adult + bq_child,
         hh_size_hu = hh_size / bq_hu) |> 
  # consumption unit
  mutate(bq_cu = 1 + 0.7 * (bq_adult - 1) + 0.5 * (bq_child + bq_infant),
         hh_size_cu = hh_size / bq_cu)

# Carbon footprint -------------------------------------------------------------

## Diet ------------------------------------------------------------------------
# Note that the diet coefficients may not be correctly derived, see:
#  rm/carbon-footprint-derivations
sur <- 
  sur |> 
  mutate(.cf_diet = calc_cf_diet(diet, diet_meat))
  
## Pets ------------------------------------------------------------------------
sur <- 
  sur |> 
  mutate(.cf_pets = calc_cf_pets(pets_dogs, pets_cats))

## Local travel ----------------------------------------------------------------

sur <- 
  sur |> 
  mutate(.cf_public_transportation = cf_public_transportation(lt_pt_km))

## Leisure travel --------------------------------------------------------------
sur <- 
  sur |> 
  calc_cf_leasure_travel()

## Goods and services ----------------------------------------------------------
sur <-
  sur |> 
  calc_cf_goods_and_services_method1()

## Housing ---------------------------------------------------------------------

## Vechicle possession ---------------------------------------------------------

## Second home -----------------------------------------------------------------

# Geospatial stuff -------------------------------------------------------------

## DEGURBA level 1 --------------------------------------------------------------
#  need to allocate position to the nearest dgurba-shape
sur <- 
  sur |> 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326,
           remove = FALSE)
dgu <- 
  read_sf("data/spatial/DGURBA_2018_01M.gpkg") |>
  janitor::clean_names() |> 
  select(nsi_code, lat_nat, .degurba1 = dgurba) %>% 
  mutate(is.valid = st_is_valid(.)) |> 
  # 1 shape not valid
  filter(is.valid) |> 
  select(-is.valid)
CRS <- st_crs(dgu)
sur <- 
  sur |> 
  st_transform(crs = CRS) |> 
  st_join(dgu) |> 
  st_transform(crs = 4326)

## DGURBA level 2 --------------------------------------------------------------
dgu <- 
  read_sf("data/spatial/DGURBA2.gpkg") |>
  janitor::clean_names() %>%
  rename(.degurba2 = degurba2)
CRS <- st_crs(dgu)
sur <- 
  sur |> 
  st_transform(crs = CRS) |> 
  st_join(dgu) |> 
  st_transform(crs = 4326)

## Population density ----------------------------------------------------------
pop <- 
  read_sf("data/spatial/JRC_GRID_2018.gpkg") |> 
  janitor::clean_names() |> 
  select(.tot_p_2018 = tot_p_2018)
CRS <- st_crs(pop)
sur <- 
  sur |> 
  st_transform(crs = CRS) |> 
  st_join(pop) |> 
  st_transform(crs = 4326)
sur <-
  sur |> 
  st_drop_geometry()

# OUTPUT -----------------------------------------------------------------------

sur |> write_csv("data-csv/survey-response-tran.csv")
