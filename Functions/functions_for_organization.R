######################################
# Collection of functions for organizing data
######################################
# /*=================================================*/
#' # Make data availability check
# /*=================================================*/
make_data_report <- function() {
  temp <- read_rmd("DataProcessing/data_availability_check.Rmd")
  writeLines(temp, con = here("Reports/ProjectTeam/data_availability_check.Rmd"))
  render(here("Reports/ProjectTeam/data_availability_check.Rmd"))
}

# /*=================================================*/
#' # Create Grower Data Folders
# /*=================================================*/

make_grower_folders <- function(field_data) {

  #--- get the field-year combinations ---#
  field_year_ls <- field_data$field_year
  # ffy <- field_year_ls[[2]]

  make_indiv_growers_folder <- function(ffy) {
    root_dir <- paste0(here(), "/Data/Growers/", ffy)

    #--- Parent ---#
    dir.create(root_dir)

    #--- Data  ---#
    dir.create(paste0(root_dir, "/Raw"))
    dir.create(paste0(root_dir, "/OriginalRawData"))
    dir.create(paste0(root_dir, "/Intermediate"))
    dir.create(paste0(root_dir, "/TrialDesign"))
    dir.create(paste0(root_dir, "/Analysis-Ready"))
    dir.create(paste0(root_dir, "/DataProcessingReport"))
    dir.create(paste0(root_dir, "/Conversations"))
    file.create(file.path(root_dir, "notes.md"))
    file.create(file.path(root_dir, "meta_data_entry.R"))

    #--- Reports ---#
    dir.create(paste0(here(), "/Reports/Growers/", ffy))

    # === copy the word template for reports ===#
    # file.copy(
    #   here("Data/CommonData/word_template.docx"),
    #   paste0(here(), "/Reports/Growers/", ffy)
    # )
  }

  lapply(field_year_ls, make_indiv_growers_folder)
}

# /*=================================================*/
#' # Create DataRequest Folders
# /*=================================================*/

make_td_folders <- function(field_data) {
  w_year <- Sys.Date() %>% year()

  field_year_ls <- field_data[year == w_year, ]$field_year

  make_indiv_folders <- function(ffy) {
    root_dir <- paste0(here(), "/Data/DataRequest/", ffy)

    if (!file.exists(root_dir)) {

      #--- Parent ---#
      dir.create(root_dir)
    }
  }

  lapply(field_year_ls, make_indiv_folders)
}

# /*=================================================*/
#' # Create a new entry with input data
# /*=================================================*/

gen_fp_template <- function(farm, field, year, crop, input_ls, strategy_ls, json_file = NULL) {
  temp_data <- data.table(
    trial_supervisor = "supervisor name",
    researcher = "institution name (e.g., UIUC, UMT, LSU)",
    farm = farm,
    field = field,
    year = year,
    crop = crop,
    crop_price = "numeric (no double quotes needed)",
    crop_unit = "bu or kg",
    raw_yield = "not received, lost, received",
    yield_data = "yield monitor data file name here",
    tr_data = "trial design data file name here",
    land_unit = "ac or ha",
    reporting_unit = "imperial or metric",
    h_width = list("numeric (no double quotes needed)"),
    ec = "not available, exists (not received), received",
    soil_sampling = "not available, exists (not received), received",
    trial_notes = "true or false (no double quotes needed)"
  )

  # i <- 1
  for (i in seq_len(length(input_ls))) {
    if (strategy_ls[i] == "trial") {
      #--- if trial data ---#
      if (input_ls[i] == "seed") {
        #--- if seed ---#
        temp_input_data <- data.table(
          form = "seed",
          strategy = "trial",
          unit = "seeds, thou_seeds",
          price = "numeric (no double quotes needed)",
          date = "mm/dd/yyyy",
          data = "not received, lost, file_name in the Raw data folder if received",
          sq_rate = "numeric (no double quotes needed) or (Rx) prescription file name",
          min_rate = "numeric (no double quotes needed)",
          max_rate = "numeric (no double quotes needed)",
          Rx_data = "not available, exists (not received), received",
          machine_width = "numeric (no double quotes needed)",
          section_num = "numeric (no double quotes needed)",
          input_plot_width = "numeric (no double quotes needed)",
          use_target_rate_instead = "true or false (no double quotes needed)"
        )
      } else {
        #--- if not seed ---#
        temp_input_data <- data.table(
          form = input_ls[i],
          strategy = "trial",
          unit = "gallons, lbs, Mg, kg, bales",
          price = "numeric (no double quotes needed)",
          date = "mm/dd/yyyy",
          data = "not received, lost, file_name in the Raw data folder if received",
          sq_rate = "numeric (no double quotes needed) or (Rx) prescription file name",
          min_rate = "numeric (no double quotes needed)",
          max_rate = "numeric (no double quotes needed)",
          Rx_data = "file_name, none",
          machine_width = "numeric (no double quotes needed)",
          section_num = "numeric (no double quotes needed)",
          input_plot_width = "numeric (no double quotes needed)",
          use_target_rate_instead = "true or false (not double quotes needed)"
        )
      }
    } else {
      #--- if base ---#
      temp_input_data <- data.table(
        form = input_ls[i],
        strategy = "base",
        data = "Rx file name, as-applied file name, or NA",
        unit = "gallons, lbs, Mg, kg, bales",
        price = "numeric (no double quotes needed)",
        date = "mm/dd/yyyy",
        rate = "numeric (no double quotes needed)"
      )
    }

    eval(parse(text = paste("temp_data[, input.", i, ":= list(temp_input_data)]", sep = "")))
  }

  if (!is.null(json_file)) {
    existing_data <- jsonlite::fromJSON(
      file.path(
        here("Data", "CommonData"),
        json_file
      ),
      flatten = TRUE
    ) %>%
      data.table()
    temp_data <- rbind(existing_data, temp_data, fill = TRUE)
  } else {
    json_file <- "fp_template"
  }

  jsonlite::write_json(
    temp_data,
    file.path(
      here("Data", "CommonData"),
      json_file
    ),
    pretty = TRUE
  )
}


# /*----------------------------------*/
#' ## Example
# /*----------------------------------*/
# gen_fp_template(
#   farm = "Paul",
#   field = "UIUC",
#   year = 2021,
#   crop = "corn",
#   input_ls = c("seed", "urea", "N_equiv"),
#   strategy_ls = c("trial", "trial", "base"),
#   fp_file = "fp_new_DSB.json"
# )

# /*=================================================*/
#' # Initiate field parameter entries for a farm-field-year
# /*=================================================*/
# This function add new field parameter templates WITHOUT input data
# for a specified field

initiate_fp_entry <- function(farm, field, year, crop, json_file = NULL) {
  temp_data <- data.table(
    trial_supervisor = "supervisor name",
    researcher = "institution name (e.g., UIUC, UMT, LSU)",
    farm = farm,
    field = field,
    year = year,
    crop = crop,
    crop_price = "numeric (no double quotes needed)",
    crop_unit = "bu or kg",
    raw_yield = "not received, lost, received",
    land_unit = "ac or ha",
    reporting_unit = "imperial or metric",
    h_width = list("numeric (no double quotes needed)"),
    ec = "not available, exists (not received), received",
    soil_sampling = "not available, exists (not received), received",
    trial_notes = "true or false (no double quotes needed)"
  )

  if (!is.null(json_file)) {
    existing_data <- jsonlite::fromJSON(
      file.path(
        here("Data", "CommonData"),
        paste0(json_file, ".json")
      ),
      flatten = TRUE
    ) %>%
      data.table()
    temp_data <- rbind(existing_data, temp_data, fill = TRUE)
  } else {
    json_file <- "fp_template"
  }

  jsonlite::write_json(
    temp_data,
    file.path(
      here("Data", "CommonData"),
      paste0(json_file, ".json")
    ),
    pretty = TRUE
  )
}

# /*----------------------------------*/
#' ## Example
# /*----------------------------------*/
# initiate_fp_entry(
#   farm = "Paul",
#   field = "UIUC",
#   year = 2021,
#   json_file = "field_parameter_example.json"
# )

# /*=================================================*/
#' # Add inputs to the template
# /*=================================================*/
# Note: this code adds input data to an existing farm-field-year in
# an existing field parameter file
# json_file <- "metadata.json"
# json_file <- "metadata_new.json"

add_inputs <- function(json_file, farm, field, year, input_data) {
  ffy <- paste(farm, field, year, sep = "_")

  existing_data <-
    here("Data", "CommonData", json_file) %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    data.table() %>%
    .[, field_year := paste(farm, field, year, sep = "_")]

  w_data <- existing_data[field_year == ffy, ]

  if (nrow(w_data) != 1) {
    print(
      "No (or duplicate) records for the specified farm-field-year are found. Check if the specified parameters are correct."
    )
    break
  }

  ex_input_data_temp <- w_data$input_data[[1]]

  if (length(ex_input_data_temp) > 0) { # if there is at least one existing entry
    ex_input_data <-
      ex_input_data_temp %>%
      rowwise() %>%
      mutate(data = list(
        data %>%
          data.table() %>%
          .[, feature := paste(form, product, strategy, var_name_prefix, sep = "_")]
      ))

    all_features <-
      map(ex_input_data$data, "feature") %>%
      unlist()
  } else {
    ex_input_data <- NULL
    all_features <- NA
  }

  for (i in 1:nrow(input_data)) {
    temp_input_data <-
      input_data[i, ] %>%
      data.table()

    temp_feature <- temp_input_data[, paste(form, product, strategy, var_name_prefix, sep = "_")]

    which_row <- which(temp_feature == all_features)

    if (length(which_row) != 0) {
      print("There is an exsiting entry in the current input data that is identical. Overriding the entry.")
      ex_input_data$data[[which_row]] <- temp_input_data
    } else {
      print("There is no exsiting entry in the current input data. Adding the entry.")
      ex_input_data <- rbind(ex_input_data, tibble(data = list(temp_input_data)))
    }
  }

  w_data <-
    as_tibble(w_data) %>%
    rowwise() %>%
    mutate(input_data = list(
      as.data.frame(ex_input_data)
    ))

  out_data <-
    rbind(
      existing_data[field_year != ffy, ],
      w_data,
      fill = TRUE
    ) %>%
    .[order(field_year), ] %>%
    .[, field_year := NULL]

  jsonlite::write_json(
    out_data,
    file.path(
      here("Data", "CommonData"),
      json_file
    ),
    pretty = TRUE
  )
}

# /*----------------------------------*/
#' ## Example
# /*----------------------------------*/
# add_inputs(
#   json_file = "field_parameter_example.json",
#   farm = "DodsonAg",
#   field = "Windmill",
#   year = "2019",
#   input_ls = c("urea", "N_equiv"),
#   strategy_ls = c("trial", "base")
# )

# /*=================================================*/
#' # Add Rx (prescription from commercial software) data
# /*=================================================*/
# Rx_data <-
#   data.table(
#     form = c("MAP", "UREA32"),
#     model = c("granular", "AdaptN"),
#     file = c("Rx_granular.shp", "Rx_AdaptN.shp"),
#     date = c("04/01/2021", "04/02/2021")
#   )

add_Rx <- function(json_file, farm, field, year, Rx_data) {
  ffy <- paste(farm, field, year, sep = "_")

  existing_data <-
    here("Data", "CommonData", json_file) %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    data.table() %>%
    .[, field_year := paste(farm, field, year, sep = "_")]

  w_data <- existing_data[field_year == ffy, ]

  if (nrow(w_data) != 1) {
    print(
      "No (or duplicate) records for the specified farm-field-year are found. Check if the specified parameters are correct."
    )
    break
  }

  ex_Rx_data_temp <- w_data$Rx_data[[1]]

  if (length(ex_Rx_data_temp) > 0) { # if there is at least one existing entry
    ex_Rx_data <-
      ex_Rx_data_temp %>%
      rowwise() %>%
      mutate(data = list(
        data %>%
          data.table() %>%
          .[, feature := paste(form, model, file, sep = "_")]
      ))

    all_features <-
      map(ex_Rx_data$data, "feature") %>%
      unlist()
  } else {
    ex_Rx_data <- NULL
    all_features <- NA
  }

  for (i in 1:nrow(Rx_data)) {
    temp_Rx_data <-
      Rx_data[i, ] %>%
      data.table()

    temp_feature <- temp_Rx_data[, paste(form, model, file, sep = "_")]

    which_row <- which(temp_feature == all_features)

    if (length(which_row) != 0) {
      print("There is an exsiting entry in the current input data that is identical. Overriding the entry.")
      ex_Rx_data$data[[which_row]] <- temp_input_data
    } else {
      print("There is no exsiting entry in the current input data. Adding the entry.")
      ex_Rx_data <- rbind(ex_Rx_data, tibble(data = list(temp_Rx_data)))
    }
  }

  w_data <-
    as_tibble(w_data) %>%
    rowwise() %>%
    mutate(Rx_data = list(
      as.data.frame(ex_Rx_data)
    ))

  out_data <-
    rbind(
      existing_data[field_year != ffy, ],
      w_data,
      fill = TRUE
    ) %>%
    .[order(field_year), ] %>%
    .[, field_year := NULL]

  jsonlite::write_json(
    out_data,
    file.path(
      here("Data", "CommonData"),
      json_file
    ),
    pretty = TRUE
  )
}


# /*=================================================*/
#' # Add External Data
# /*=================================================*/

# json_file <- "metadata.json"

add_Ex <- function(json_file, farm, field, year, Ex_data) {
  ffy <- paste(farm, field, year, sep = "_")

  existing_data <-
    here("Data", "CommonData", json_file) %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    data.table() %>%
    .[, field_year := paste(farm, field, year, sep = "_")]

  w_data <- existing_data[field_year == ffy, ]

  if (nrow(w_data) != 1) {
    print(
      "No (or duplicate) records for the specified farm-field-year are found. Check if the specified parameters are correct."
    )
    break
  }

  ex_Ex_data_temp <- w_data$Ex_data[[1]]

  if (length(ex_Ex_data_temp) > 0) { # if there is at least one existing entry
    ex_Ex_data <-
      ex_Ex_data_temp %>%
      rowwise() %>%
      mutate(data = list(
        data %>%
          data.table() %>%
          .[, feature := paste(file, paste(vars[[1]], collapse = "_"), var_name_prefix, sep = "_")]
      ))

    all_features <-
      map(ex_Ex_data$data, "feature") %>%
      unlist()
  } else {
    ex_Ex_data <- NULL
    all_features <- NA
  }

  for (i in 1:nrow(Ex_data)) {
    temp_Ex_data <-
      Ex_data[i, ] %>%
      data.table()

    temp_feature <- temp_Ex_data[, paste(file, paste(vars[[1]], collapse = "_"), var_name_prefix, sep = "_")]

    which_row <- which(temp_feature == all_features)

    if (length(which_row) != 0) {
      print("There is an exsiting entry in the current input data that is identical. Overriding the entry.")
      ex_Ex_data$data[[which_row]] <- temp_input_data
    } else {
      print("There is no exsiting entry in the current input data. Adding the entry.")
      ex_Ex_data <- rbind(ex_Ex_data, tibble(data = list(temp_Ex_data)))
    }
  }

  w_data <-
    as_tibble(w_data) %>%
    rowwise() %>%
    mutate(Ex_data = list(
      as.data.frame(ex_Ex_data)
    ))

  out_data <-
    rbind(
      existing_data[field_year != ffy, ],
      w_data,
      fill = TRUE
    ) %>%
    .[order(field_year), ] %>%
    .[, field_year := NULL]

  jsonlite::write_json(
    out_data,
    file.path(
      here("Data", "CommonData"),
      json_file
    ),
    pretty = TRUE
  )
}

# /*=================================================*/
#' # Add a variable to a field parameter json file
# /*=================================================*/

add_var_to_fp <- function(file_name, var_name, var_value = NULL, overwrite = FALSE) {
  field_data <- jsonlite::fromJSON(
    file.path(
      here("Data", "CommonData"),
      file_name
    ),
    flatten = TRUE
  ) %>%
    data.table() %>%
    .[, field_year := paste(farm, field, year, sep = "_")]

  if (!var_name %in% names(field_data)) {
    if (!is.null(var_value)) {
      suppressWarnings(field_data[, (var_name) := var_value])
    } else {
      suppressWarnings(field_data[, (var_name) := "NA"])
    }
  } else {
    stop("A variable of the same name already exists. Aborting the requested operation.")
  }

  if (overwrite == TRUE) {
    jsonlite::write_json(
      field_data,
      file.path(
        here("Data", "CommonData"),
        file_name
      ),
      pretty = TRUE
    )
  } else {
    cat("A new variable was added. Confirm this is indeed what you want. If so, use overwrite = TRUE option to overwrite the file.")
    return(field_data)
  }
}

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Example
# /*~~~~~~~~~~~~~~~~~~~~~~*/
#--- check the data with a new variable ---#
# data_check <- add_var_to_fp(
#   file_name = "field_parameter_example.json",
#   var_name = "new",
#   var_value = "temp",
#   overwrite = FALSE
# )

#--- overwrite the file ---#
# add_var_to_fp(
#   file_name = "field_parameter_example.json",
#   var_name = "new",
#   var_value = "temp",
#   overwrite = TRUE
# )