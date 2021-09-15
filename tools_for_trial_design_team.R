######################################
# R codes to assist Trial Design Coordinators
######################################

# /*=================================================*/
#' # Preparation
# /*=================================================*/
#/*----------------------------------*/
#' ## Load packages and source functions
#/*----------------------------------*/
source(
  "https://github.com/tmieno2/DIFM/blob/master/Functions/prepare.R?raw=TRUE",
  local = TRUE
)

# /*----------------------------------*/
#' ## Load the field parameter data
# /*----------------------------------*/
field_data <- 
jsonlite::fromJSON(
  here("Data", "CommonData", "fp_2021_TD.json"),
  flatten = TRUE
) %>%
data.table() %>%
.[, field_year := paste(farm, field, year, sep = "_")] %>% 
.[year == 2021, ]

field_year_ls <- field_data$field_year

#/*=================================================*/
#' # Make trial designs
#/*=================================================*/

ffy <- field_year_ls[53]

get_td_parameters(ffy, json_file = "fp_2021_TD.json")

#--- force headland to be 100 feet ---#
make_trial_design(
  ffy = field_year_ls[53], 
  json_file = "fp_2021_TD.json", 
  head_dist = 120, # default uses 2 * the max of the input plot widths
  side_dist = 60, # default uses the max of the section width (minimum is 30)
  #=== angled harvesting ===#
  plot_heading = "ab-lines-cl",
  #=== this option is for designs where application and harvesting are perpendicular to each other ===#
  perpendicular = TRUE,
  min_plot_length = 200,
  max_plot_length = 200,
  ab_line_type = "lock", # pick from "non", "free", "lock"
  #=== design type ===#
  # jcl: jump-conscious latin
  # ejca: extra jump-conscious alternate strip
  # design_type = "ejca",
  #=== user-specified rates ===#
  # rates = list(c(40, 50, 60, 70, 80)),
  #=== number of levels ===#
  # used in "jcl", ignored in "ejca"
  # num_levels = c(6, 6), 
  #=== maximum jump allowed  ===#
  # used in "ejca", ignored in "jcl"
  # max_jumps = c(NA, NA),
  #=== file name ===#
  file_name_append = ""
)

#/*=================================================*/
#' # DevTeam
#/*=================================================*/
ffy <- field_year_ls[38]

get_td_parameters(ffy, "fp_2021_TD.json")

make_trial_design(
  ffy = field_year_ls[38], 
  json_file = "fp_2021_TD.json", 
  # head_dist = 120, # default uses 2 * the max of the input plot widths
  # side_dist = 30, # default uses the max of the section width (minimum is 30)
  boundary = "boundary-west",
  #=== angled harvesting ===#
  plot_heading = "ab-line-s-west",
  ab_line_type = "non", # pick from "non", "free", "lock"
  #=== design type ===#
  # jcl: jump-conscious latin
  # ejca: extra jump-conscious alternate strip
  # design_type = "ejca",
  #=== user-specified rates ===#
  # rates = list(c(40, 50, 60, 70, 80)),
  #=== number of levels ===#
  # used in "jcl", ignored in "ejca"
  # num_levels = c(6, 6), 
  #=== maximum jump allowed  ===#
  # used in "ejca", ignored in "jcl"
  # max_jumps = c(NA, NA),
  #=== file name ===#
  file_name_append = "",
  locally_run = TRUE
)




