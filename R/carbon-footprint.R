#' Calculate diet carbon footprint
#'
#' @param diet The survey response value for diet type
#' @param meat The survey response value for meat consumption
#'
#' @return A vector containing diet carbon ...
#' @export
#'
calc_cf_diet <- function(diet, meat) {
  dplyr::case_when(diet %in% c("Vegetarian diet", "Vegan diet") & meat == "None" ~ 1132,
                   diet == "Pescatarian diet" & meat == "None" ~ 1278,
                   diet == "Omnivore diet" & meat == "Maximum once or twice a week" ~ 1533,
                   diet == "Omnivore diet" & meat == "Two to three times a week" ~ 1679,
                   diet == "Omnivore diet" & meat == "Almost everyday" ~ 2519,
                   diet == "Omnivore diet" & meat == "At least once or twice a day" ~ 3213,
                   TRUE ~ 0)
}

#' Footprint of pets (dogs and cats)
#'
#' @param dogs number of dogs
#' @param cats number of cats
#'
#' @return a vector
#' @export
#'
calc_cf_pets <- function(dogs, cats) {
  dogs * 630 + cats * 630/2
}

#' Public transportation
#' 
#' To calculate the emissions of public transportation, an average value of 
#' direct greenhouse gas emissions of natural gas bus, commuter train, tram and 
#' metro were utilized based on the data from VTT Technical Research Centre of 
#' Finland (2021). The indirect greenhouse gas emissions from vehicles, 
#' infrastructure, fuel production and supply chain (Chester & Horvath 2009) 
#' were added to all transport modes before calculating the average public 
#' transportation coefficient, 0.12 kg CO2e/PKT, used in the calculator.

#' In the survey, participants were asked to estimate how many kilometers did 
#' they travel per week by public transportation, and this was multiplied by 
#' 0.12 kg CO2e to get the GHG emission of local travel.
#'
#' @param km_per_week 
#'
#' @return A vector
#' @export
#'
cf_public_transportation <- function(km_per_week) {
  km_per_week * 0.12 * 52
}


#' Leisure travel footprint
#' 
#' Production emissions of vehicles, infrastructure, fuel production, and supply 
#' chain emissions are added to all transport modes included in leisure travel 
#' section (airplane, ferry, train, bus) according to Chester & Horvath (2009). 
#' Direct emissions are calculated based on the travel distance and transport 
#' mode. Exact values used to calculate the travel emissions are
#' ...
#' 
#' For short distant leisure travel, boat trips are calculated as 
#' 250km x 2 (return). Airplane, train and bus are calculated as 
#' 500km x 2 (return).
#' 
#' For medium distance leisure travel, boat trips are calculated as 
#' 1140km x 2 (e.g. Helsinki to Travemunde and back). Airplane, train 
#' and bus are calculated as 2000km * 2 (return).
#' 
#' For long distance leisure travel, boat trips are calculated as 
#' 6000km x 2 (e.g. Southampton to New York and back). Airplane, train and bus 
#' are calculated as 8000km x 2 (return).
#'
#' @param d A data frame containing ....
#'
#' @return A data frame with additional variable .cf_leisure_travel
#' @export
#'
calc_cf_leasure_travel <- function(d) {
  d2 <- 
    d |> 
    select(.cntr, .rid, starts_with("ld_"))  |> 
    select(-ld_trips) |>
    select(!starts_with("ld_car")) |> 
    pivot_longer(
      cols = ld_ferry_s:ld_bus_l,
      names_to = c("pfix", "t", "d"),
      names_sep = "_",
      values_to = "n") |> 
    select(-pfix)
  d3 <- 
    d2 |> 
    mutate(cf = case_when(t == "ferry" & d == "s" ~ n * 2 *  250 * 0.36,
                          t == "ferry" & d == "m" ~ n * 2 * 1140 * 0.36,
                          t == "ferry" & d == "l" ~ n * 2 * 6000 * 0.36,
                          t == "air"   & d == "s" ~ n * 2 *  500 * 0.34,
                          t == "air"   & d == "m" ~ n * 2 * 2000 * 0.28,
                          t == "air"   & d == "l" ~ n * 2 * 8000 * 0.28,
                          t == "train" & d == "s" ~ n * 2 *  500 * 0.08,
                          t == "train" & d == "m" ~ n * 2 * 2000 * 0.08,
                          t == "train" & d == "l" ~ n * 2 * 8000 * 0.08,
                          t == "bus"   & d == "s" ~ n * 2 *  500 * 0.15,
                          t == "bus"   & d == "m" ~ n * 2 * 2000 * 0.15,
                          t == "bus"   & d == "l" ~ n * 2 * 8000 * 0.15,
                          t == "car"   & d == "s" ~ 0,
                          t == "car"   & d == "m" ~ 0,
                          t == "car"   & d == "l" ~ 0,
                          TRUE ~ 0)
    )
  d4 <- d3 |> group_by(.cntr, .rid) |> summarise(.cf_leisure_travel = sum(cf), .groups = "drop")
  d |> 
    left_join(d4, by = c(".rid", ".cntr"))
}

#' Goods and services footprint
#' 
#' The greenhouse gas emissions resulting from the consumption of goods and 
#' services are defined by utilizing ENVIMAT -model (Alhola et al. 2019). 
#' Classification of Individual Consumption According to Purpose (COICOP) 
#' consumption categories are utilized both in the survey and in ENVIMAT-model 
#' (United Nations 2018). Inflation corrections are added to ENVIMAT 2016 
#' intensities according to Statistic Finland (2020), updating the intensities 
#' to 2020. Currency exchange rates (EUR/EUR=1) are from the same year, 2020, 
#' as follows:
#' 
#' SEK/EUR=10.4865;
#' NOK/EUR=10.7238;
#' ISK/EUR=154.59;
#' DKK/EUR=7.4543 
#' (European Central Bank, 2021).
#' 
#' Personal emissions are calculated by multiplying the used amount of money 
#' by the category coefficient. Coefficients for different categories are 
#' available within the function.
#'
#' @param d A data frame containing ....
#' @param summarized boolean (default TRUE) indicating it summary or itemized
#' carbon footprint is returned.
#'
#' @return A data frane with additional variable .cf_goods_and_services
#' @export
#'
#' @examples
calc_cf_goods_and_services_envimat <- function(d, summarized = TRUE) {
  exchange <- 
    tribble(~.cntr,      ~xch,
            "DK",   7.4543,
            "FI",   1.0000,
            "IS", 154.5900, 
            "NO",   10.7238,
            "SE",   10.4865)
  gs_co2e <- 
    tribble(~variable, ~co2e_eur,
            "gs_alc_cig", 0.17, 
            "gs_clothes", 0.32,
            "gs_design",  0.47,
            "gs_health",  0.16,
            "gs_sport",   0.36,
            "gs_hospo",   0.30,
            "gs_hotels",  0.40,
            "gs_electr",  0.60,
            "gs_other",   0.19)
  d2 <- 
    d |> 
    select(.rid, .cntr, starts_with("gs_")) |> 
    select(-gs_total) |> 
    gather(variable, val, -c(.cntr, .rid)) |> 
    left_join(exchange, by = ".cntr") |> 
    mutate(eur = val / xch) |> 
    left_join(gs_co2e, by = "variable") |> 
    # multiply by 12 to get the annual esimates
    mutate(.cf_goods_and_services = eur * co2e_eur * 12)
  
  if(summarized) {
    d2 <- 
      d2 |> 
      group_by(.rid, .cntr) |> 
      summarise(.cf_goods_and_services = sum(.cf_goods_and_services),
                .groups = "drop")
  } else {
    d2 <- 
      d2 |> 
      select(.rid, .cntr, variable, val = .cf_goods_and_services) |> 
      mutate(variable = paste0(".cf_", variable)) |> 
      spread(variable, val)
  }
  d |> 
    left_join(d2,
              by = c(".rid", ".cntr"))
}


#' Annual energy consumption (MWh??) per m2
#' 
#' Energy consumption per m2 (MWh/k-m2/year) as a function of house type and
#' decade of construction.
#'
#' @param type A character string, one of "Apartment", "Row house" and "Detached house"
#' @param decade An integer indicating the start of the decade (1950, 1960, ... 2000)
#'
#' @return A numeric vector containing annual energy consumption (MWh??) per square-meter
#' @export
#'
calc_house_ec_m2_year <- function(type, decade) {
  
  if(class(decade) == "character") {
    decade <- 
      as.integer(stringr::str_sub(decade, 1, 4))
  }
  
  type <- ifelse(type == "Semi-detached house", "Detached house", type)
  type <- ifelse(type == "Other", "Apartment", type)
  gge <- 
    dplyr::case_when(decade == 1950 & type == "Apartment" ~ 0.167,
                     decade == 1960 & type == "Apartment" ~ 0.196,
                     decade == 1970 & type == "Apartment" ~ 0.178,
                     decade == 1980 & type == "Apartment" ~ 0.148,
                     decade == 1990 & type == "Apartment" ~ 0.158,
                     decade == 2000 & type == "Apartment" ~ 0.132,
                     decade == 1950 & type == "Detached house" ~ 0.180,
                     decade == 1960 & type == "Detached house" ~ 0.203,
                     decade == 1970 & type == "Detached house" ~ 0.180,
                     decade == 1980 & type == "Detached house" ~ 0.152,
                     decade == 1990 & type == "Detached house" ~ 0.153,
                     decade == 2000 & type == "Detached house" ~ 0.133,
                     decade == 1950 & type == "Row house" ~ 0.182,
                     decade == 1960 & type == "Row house" ~ 0.200,
                     decade == 1970 & type == "Row house" ~ 0.181,
                     decade == 1980 & type == "Row house" ~ 0.151,
                     decade == 1990 & type == "Row house" ~ 0.151,
                     decade == 2000 & type == "Row house" ~ 0.134,
                     TRUE ~ NA_real_)
  return(gge)
}