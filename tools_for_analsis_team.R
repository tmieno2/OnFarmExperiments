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
make_data_report()

# ffy <- field_year_ls[8]

# /*----------------------------------*/
#' ## Non-experiment Data
# /*----------------------------------*/
#--- all fields ---#
# lapply(field_year_ls, non_exp_process_make_report)

#--- individually ---#
non_exp_process_make_report(field_year_ls[1], rerun = TRUE)

# /*----------------------------------*/
#' ## Experiment Data
# /*----------------------------------*/
#--- all fields ---#
# lapply(field_year_ls, exp_process_make_report, rerun = TRUE)

#--- individually ---#
exp_process_make_report(
  ffy = field_year_ls[1],
  rerun = TRUE,
  locally_run = TRUE
)

# /*----------------------------------*/
#' ## Final Data Processing (Putting all altogether)
# /*----------------------------------*/
#--- all fields ---#
# lapply(field_year_ls, f_process_make_report)

#--- individually ---#
f_process_make_report(
  ffy = field_year_ls[2],
  rerun = TRUE
)

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
  locally_run = TRUE
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

