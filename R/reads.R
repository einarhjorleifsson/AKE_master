# 2022-12-04: Version that works on the following files:
# files <- 
#   c("data-raw/Denmark_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
#     "data-raw/Finland_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
#     "data-raw/Iceland_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
#     "data-raw/Norway_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx",
#     "data-raw/Sweden_data_adults corrected_new variables_exiobase_vehicle prod maint_consumption_units.xlsx")



# importing data ---------------------------------------------------------------
read_raw <- function(fil) {
  suppressMessages(
    readxl::read_excel(fil,
                       guess_max = 1e4,  # no guessing
                       sheet = 1, 
                       na = c("N/A", "NULL"))) |> 
    rename(.rid = 1) |> 
    # keep only the first response id column
    select(-starts_with("Response ID")) |> 
    # have to have a response_id
    filter(!is.na(.rid))
}
read_key <- function(fil) {
  readxl::read_excel(fil,
                     col_types = "text",
                     sheet = "Key",
                     skip = 4) |> 
    dplyr::select(alias = 1,
                  name = 2) |> 
    dplyr::mutate(alias = ifelse(alias == "hh_heat2?", "hh_heat2_yesno", alias))
}
read_numeric <- function(fil) {
  
  ret <- 
    readxl::read_excel(fil, 
                       guess_max = 1e5,
                       sheet = 3) |> 
    janitor::clean_names() |> 
    rename(.rid = 1) |> 
    # have to have a response_id
    filter(!is.na(.rid))
  
  if(str_detect(tolower(fil), "norway")) {
    ret <-
      ret |> 
      rename(cf_unique_id = cf_response_id,
             cf_income_level = income_level,
             cf_age_group = age_group,
             cf_household = household) |> 
      mutate(cf_unique_id = as.character(cf_unique_id)) |> 
      select(-adult_correction)
  }

  if(str_detect(tolower(fil), "sweden")) {
    ret <- 
      ret |> 
      rename(hh_type = hh_type_12,
             pq_p_gross = bq_p_gross_151,
             bq_h_gross = bq_h_gross_152,
             bq_hhsize_corrected = hh_size_corrected) |> 
      select(-c(bq_p_gross_184,
                bq_h_gross_186,
                hh_type_196,
                adults_corrected_for_0,
                bq_adult_corrected_zero))
  }
  
  
  ret <- 
    ret |> 
    mutate(lau_id = as.character(lau_id),
           pets_cats = as.character(pets_cats),
           pets_dogs = as.character(pets_dogs),
           pets_other = as.character(pets_other),
           bq_age = as.character(bq_age),
           cf_unique_id = as.numeric(cf_unique_id)) |> 
    mutate(.cntr = case_when(str_detect(tolower(fil), "denmark") ~ "DK",
                             str_detect(tolower(fil), "finland") ~ "FI",
                             str_detect(tolower(fil), "iceland") ~ "IS",
                             str_detect(tolower(fil), "norway") ~ "NO",
                             str_detect(tolower(fil), "sweden") ~ "SE"))
  return(ret)
}


read_data <- function(fil) {
  raw <- read_raw(fil)
  key <- read_key(fil)
  num <- read_numeric(fil)
  
  # only take the first 166 columns - the rest are not part of the surevey
  #  but externally derived variables (mutated values)
  raw_survey <- raw |> select(1:166)
  colnames(raw_survey) <- key$alias[1:ncol(raw_survey)]
  raw_survey <-
    raw_survey |> 
    rename(.rid = response_id) |> 
    mutate(pets_dogs = as.character(pets_dogs),
           pets_cats = as.character(pets_cats)) |> 
    separate(bq_geoloc, into = c("lat", "lon"), sep = ";", convert = TRUE)
  
  raw_footprint <- 
    raw |> 
    select(ends_with("footprint")) |> 
    rename(cf_diet = "Diet footprint",
           cf_housing = "Housing footprint",
           cf_vehicle_possession = "Vehicle possession footprint",
           cf_local_travel = "Local travel footprint",
           cf_leisure_travel = "Leisure travel footprint",
           cf_goods_and_services = "Goods and services footprint",
           cf_pets = "Pets footprint",
           cf_summer_cottage = "Summer house footprint",
           cf_footprint = "Total footprint")
  
  raw_rest <- 
    raw |> 
    # Note: Not using standard "project" names here
    select(bq_decile = `Income Level decile`,
           bq_age_class = `Age group`,
           bq_n = `Number of persons in household`)
  
  
  bind_cols(raw_survey, raw_rest, raw_footprint) |> 
    mutate(.cntr = case_when(str_detect(tolower(fil), "denmark") ~ "DK",
                             str_detect(tolower(fil), "finland") ~ "FI",
                             str_detect(tolower(fil), "iceland") ~ "IS",
                             str_detect(tolower(fil), "norway") ~ "NO",
                             str_detect(tolower(fil), "sweden") ~ "SE"))
}

