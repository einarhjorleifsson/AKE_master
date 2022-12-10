library(tidyverse)
library(readxl)
library(janitor)

# import -----------------------------------------------------------------------
files <- 
  c("data-raw/z_older/DENMARK-DATA-ADULTS-CORRECTED-w-NEW-VARIABLES.xlsx",
    "data-raw/z_older/FINLAND-DATA-ADULTS-CORRECTED-w-NEW-VARIABLES.xlsx",
    "data-raw/z_older/ICELAND-DATA-ADULTS-CORRECTED-w-NEW-VARIABLES.xlsx",
    "data-raw/z_older/NORWAY-DATA-ADULTS-CORRECTED-w-NEW-VARIABLES.xlsx",
    "data-raw/z_older/SWEDEN-DATA-ADULTS-CORRECTED-w-NEW-VARIABLES.xlsx")


dk <- read_excel(files[1], sheet = 3, guess_max = 1e6, col_types = "text") |> rename(.rid = 1) |> mutate(.cntr = "DK")
fi <- read_excel(files[2], sheet = 4, guess_max = 1e6, col_types = "text") |> rename(.rid = 1) |> mutate(.cntr = "FI")
is <- read_excel(files[3], sheet = 3, guess_max = 1e6, col_types = "text") |> rename(.rid = 1) |> mutate(.cntr = "IS")
no <- read_excel(files[4], sheet = 3, guess_max = 1e6, col_types = "text") |> rename(.rid = 1) |> mutate(.cntr = "NO") |> 
  rename(cf_goods_and_services = goods_and_services,
         cf_vehicle_possession = vehicle_possession)
se <- read_excel(files[5], sheet = 3, guess_max = 1e6, col_types = "text") |> rename(.rid = 1) |> mutate(.cntr = "SE")

bind_rows(dk, fi, is, no, se) |> 
  janitor::clean_names() |> 
  rename(.rid = 1,
         .cntr = cntr) |> 
  mutate(.rid = as.integer(.rid),
         cf_vehicle_possession = as.numeric(cf_vehicle_possession),
         cf_goods_and_services = as.numeric(cf_goods_and_services)) |> 
  filter(!is.na(.rid)) |> 
  write_csv("data-csv/numeric-merged-countries_oldest-excel-files.csv")
  