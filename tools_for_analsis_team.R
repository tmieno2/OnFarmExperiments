# /*=================================================*/
#' # Preparation
# /*=================================================*/
#--- packages and functions ---#
source(
  "https://github.com/tmieno2/OnFarmExperiments/blob/master/Functions/prepare.R?raw=TRUE",
  local = TRUE
)

#--- field parameter data ---#
field_data <-
  jsonlite::fromJSON(
    file.path(
      here("Data", "CommonData"),
      "metadata.json"
    ),
    flatten = TRUE
  ) %>%
  data.table() %>%
  .[, field_year := paste(farm, field, year, sep = "_")]

#--- get the field-year combinations ---#
# field_year_ls <- field_data[year == 2020, ]$field_year
field_year_ls <- field_data$field_year

# /*=================================================*/
#' # Data Processing
# /*=================================================*/
# make_data_report()

ffy <- field_year_ls[2]

# /*----------------------------------*/
#' ## Non-experiment Data
# /*----------------------------------*/
#--- all fields ---#
# lapply(field_year_ls, non_exp_process_make_report)

#--- individually ---#
get_ne_data(
  ffy = ffy,
  rerun = TRUE,
  locally_run = FALSE
)

# /*----------------------------------*/
#' ## Experiment Data
# /*----------------------------------*/
#--- all fields ---#
# lapply(field_year_ls, exp_process_make_report, rerun = TRUE)

trial_pars <- get_trial_parameter(ffy)
trial_info <- trial_pars$input_data_trial

process_yield(
  ffy = ffy,
  ol_yield_sd_factor = 4
)

process_input(
  ffy = ffy,
  ol_sd_factor = 4,
  #* === define how many as-applied paths you have in a design plot ===*#
  #* this one is used only when there are missing data points due to the application of zero rates
  #* do not forget ""
  num_paths = "c(2, 1)"
)

# process_input(
#   ffy = ffy,
#   ol_sd_factor = 4
# )

#* Warning: the number of elements in max_dev_ls has to match with the number of inputs in trial_info

merge_yield_input(
  ffy,
  overlap_acceptance_pct = 0.1,
  max_dev_ls = "c(10, 10)",
  ignore_overlap_threshold = 0.05 # default 0.05
)


# /*----------------------------------*/
#' ## Final Data Processing (Putting all altogether)
# /*----------------------------------*/
#--- all fields ---#
# lapply(field_year_ls, f_process_make_report)

#--- individually ---#
f_process_make_report(ffy = ffy)

# locally_run = FALSE

# /*=================================================*/
#' # Run analysis and make report
# /*=================================================*/

# /*----------------------------------*/
#' ## Run analysis
# /*----------------------------------*/
#--- all fields ---#
# lapply(field_year_ls, run_analysis, rerun = TRUE)

#--- individually ---#
run_analysis(
  ffy = field_year_ls[7],
  rerun = TRUE,
  locally_run = FALSE
)

# /*----------------------------------*/
#' ## Make report
# /*----------------------------------*/
#--- all fields ---#
# lapply(field_year_ls, make_report, rerun = TRUE)

#--- individually ---#
make_grower_report(
  ffy = field_year_ls[6],
  rerun = TRUE
)

# render("/Users/tmieno2/Box/DIFM_Central/Reports/Growers/Scheider_Roby_2019/grower-report_DSB.Rmd")

here(
  "Reports/Growers", ffy,
  paste0("grower-report-s.Rmd")
) %>%
  render()
