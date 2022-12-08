td_none_and_5ormore <- function(x) {
  x |> 
    stringr::str_squish() |> 
    stringr::str_replace("None", "0") |> 
    stringr::str_replace("5 or more", "5") |> 
    as.numeric()
}

td_raw_data <- function(d) {
  
  d |> 
    # replace "none" and "5ormore" in some of the variables
    mutate(bq_adult   = td_none_and_5ormore(bq_adult),
           # Assume there is always one adult - but note we will use adult_corrected below
           bq_adult   = ifelse(bq_adult == 0, 1, bq_adult),
           bq_child   = td_none_and_5ormore(bq_child),
           bq_infant  = td_none_and_5ormore(bq_infant),
           pets_cats  = td_none_and_5ormore(pets_cats),
           pets_dogs  = td_none_and_5ormore(as.character(pets_dogs)),
           pets_other = td_none_and_5ormore(pets_other)) |> 
    mutate(.bqy = bq_child + bq_infant,  # temporary variable, will get rid of it downstream
           bq_n = bq_adult_corrected + bq_child + bq_infant,
           # note this will replace hh_type_c
           bq_type = case_when(bq_adult_corrected == 1 & .bqy == 0 ~ "Single",         # 1
                               bq_adult_corrected >  1 & .bqy == 0 ~ "Couple",         # 2
                               bq_adult_corrected == 1 & .bqy >  0 ~ "Single parent",  # 3
                               bq_adult_corrected >  1 & .bqy >  0 ~ "Family"),        # 4
           bq_type = factor(bq_type, levels = c("Single", "Couple", "Single parent", "Family"))) |> 
    select(-.bqy) |> 
    # "Align" the income decile
    mutate(bq_decile = paste0("A", str_pad(str_sub(bq_decile, 2), width = 2, pad = "0"))) |> 
    # DROP DATA
    filter(hh_type != "Other") |> 
    # Lump hh_types
    mutate(hh_type = ifelse(hh_type == "Semi-detached house",
                            "Row house",
                            hh_type),
           hh_type = factor(hh_type, levels = c("Apartment", "Row house", "Detached house"))) |> 
    # fix error in entry of "diet_meat"
    mutate(diet_meat = ifelse(diet_meat == "none", "None", diet_meat))
  
}

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
