
get_trial_parameter <- function(ffy) {
  #/*=================================================*/
  #' # Extract input information from field data
  #/*=================================================*/
  #--- field data ---#
  field_data <- jsonlite::fromJSON(
    file.path(
      here("Data", "CommonData"),
      "metadata.json"
    ),
    flatten = TRUE
  ) %>%
  data.table() %>%
  .[, farm_field := paste(farm, field, sep = "_")] %>% 
  .[, field_year := paste(farm_field, year, sep = "_")] 

  #--- get field parameters for the field-year ---#
  w_field_data <- field_data[field_year == ffy, ]

  w_farm_field <- w_field_data$farm_field
  w_year <- w_field_data$year

  #--- get input data ---#
  input_data <- 
    dplyr::select(
      w_field_data, 
      starts_with("input")
    ) %>%  
    map(1) %>% 
    rbindlist(fill = TRUE)

  #/*----------------------------------*/
  #' ## Crop information
  #/*----------------------------------*/
  crop <- w_field_data[, crop] 
  crop_unit <- w_field_data[, crop_unit] 
  # crop_price <- as.numeric(w_field_data[, crop_price]) 
  # if(is.na(crop_price) == TRUE) {
  #   print("Crop price is either missing in the metadata or the value provided is not valid. Using the default value.")
  #   crop_price <- case_when(
  #     crop == "soy" ~ 14, # $/bu
  #     crop == "corn" ~ 5.5, # $/bu
  #     crop == "cotton" ~ 0.93 * 480, # $/bales
  #   )
  # }

  land_unit <- w_field_data[, land_unit] 
  reporting_unit <- w_field_data[, reporting_unit] 
  harvester_width <- w_field_data[, h_width][[1]]

  #/*=================================================*/
  #' # Input information
  #/*=================================================*/
  n_var_ls <- c(
    "NH3", "UREA46", "UAN32", "UAN28", "1_2_1(36)", "LAN(26)", "MAP", 
    "1_0_0", "1_0_1", "2_3_2(22)", "15_10_6", "3_0_1", "2_3_4(32)", 
    "4_3_4(33)", "5_1_5", "Sp"
  )

  input_data_trial <- 
    input_data[
      strategy == "trial", 
      .(form, use_target_rate_instead, machine_width, unit, data)
    ] %>% 
    .[, input_type := NA] %>% 
    .[, input_type := ifelse(form == "seed", "S", input_type)] %>% 
    .[, input_type := ifelse(form %in% n_var_ls, "N", input_type)] %>% 
    .[, reporting_unit := field_data$reporting_unit]

  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Base nitrogen 
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  is_base_N <- "base" %in% input_data[, strategy]

  if (is_base_N) {

    n_base_rate <- 
      input_data[strategy == "base", ] %>% 
      rowwise() %>% 
      mutate(
        n_equiv_rate = convert_N_unit(
          form = form, 
          unit = unit, 
          rate = rate, 
          reporting_unit = reporting_unit
        ) 
      ) %>% 
      data.table() %>% 
      .[, sum(n_equiv_rate)]

  } else {

    n_base_rate <- 0  

  }

  return(list(
    n_base_rate = n_base_rate,
    input_data_trial = input_data_trial
  ))
}



