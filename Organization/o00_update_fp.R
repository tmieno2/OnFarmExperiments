#/*=================================================*/
#' # Update field parameter to the latest format
#/*=================================================*/

field_data <- jsonlite::fromJSON(
  file.path(
    here("Data", "CommonData"),
    "field_parameter.json"
  ),
  flatten = TRUE
) %>%
data.table() %>%
.[, field_year := paste(farm, field, year, sep = "_")] 

field_data <- relocate(field_data, trial_supervisor, researcher, farm, field, year, crop) %>% 
  .[order(year),]

#/*----------------------------------*/
#' ## Insert missing variable
#/*----------------------------------*/
update_fp <- function(i) {

  print(i)

  #--- get field parameters for the field-year ---#
  w_field_data <- field_data[i, ]

  #--- get input data ---#
  input_data <- dplyr::select(w_field_data, starts_with(
    "input")) 

  num_empty_inputs <- nrow(input_data %>% map(1) %>% rbindlist(fill = TRUE))

  for (j in seq_len(num_empty_inputs)) {

    print(j)

    temp_input_data <- input_data[[j]][[1]] %>% data.table()

    if (!"unit" %in% names(temp_input_data)) {
      temp_input_data$unit <- "gallons, lbs, Mg, kg, bales"
    } else if (is.na(temp_input_data$unit)) {
      temp_input_data$unit <- "gallons, lbs, Mg, kg, bales"
    }

    if (!"date" %in% names(temp_input_data)) {
      temp_input_data$date <- "mm/dd/yyyy"
    } else if (is.na(temp_input_data$date)) {
      temp_input_data$date <- "mm/dd/yyyy"
    }

    if (temp_input_data$strategy == "trial") {

      if (!"price" %in% names(temp_input_data)) {
        temp_input_data$price <- "numeric (no double quotes needed)"
      } else if (is.na(temp_input_data$price)) {
        temp_input_data$price <- "numeric (no double quotes needed)"
      }
      
      if (!"Rx_exists" %in% names(temp_input_data)) {
        temp_input_data$Rx_exists <- "not available, exists (not received), received"
      }

      if (!"machine_width" %in% names(temp_input_data)) {
        temp_input_data$machine_width <- "numeric (no double quotes needed)"
      }

      if (!"sq_rate" %in% names(temp_input_data)) {
        temp_input_data$sq_rate <- "numeric (no double quotes needed) or (Rx) prescription file name"
      }

      if (!"use_target_rate_instead" %in% names(temp_input_data)) {
        temp_input_data$use_target_rate_instead <- "true or false (no double quotes needed)"
      }

      input_data_to_assign <- temp_input_data[, .(
        form, strategy, unit, price, date, data, sq_rate, 
        Rx_exists, machine_width, use_target_rate_instead
      )]

    } else if (temp_input_data$strategy == "base") {

      if (!"rate" %in% names(temp_input_data)) {
        temp_input_data$rate <- "numeric (no double quotes needed)"
      } else if (is.na(temp_input_data$rate)) {
        temp_input_data$rate <- "na"
      } 

      if (!"data" %in% names(temp_input_data)) {
        temp_input_data$data <- "Rx file name, as-applied file name, or NA"
      } else if (is.na(temp_input_data$data)) {
        temp_input_data$data <- "na"
      } 

      input_data_to_assign <- temp_input_data[, 
        .(form, strategy, rate, unit, date, data
      )]
    }

    eval(parse(text=paste("field_data[i, input.", j , ":= list(input_data_to_assign)]", sep = "")))

  }

}

lapply(seq_len(nrow(field_data)), update_fp)

field_data[, field_year := NULL]

jsonlite::write_json(
    field_data, 
    file.path(
      here("Data", "CommonData"),
      "fp_DSB_updated.json"
    ),
    pretty = TRUE
  )





