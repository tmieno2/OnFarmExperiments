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
  "https://github.com/tmieno2/OnFarmExperiments/blob/master/Functions/prepare.R?raw=TRUE",
  local = TRUE
)

# /*----------------------------------*/
#' ## Load the field parameter data
# /*----------------------------------*/
field_data <- 
  jsonlite::fromJSON(
    here("Data", "CommonData", "metadata.json"),
    flatten = TRUE
  ) %>%
  data.table() %>%
  .[, field_year := paste(farm, field, year, sep = "_")]

#/*=================================================*/
#' # Create new field parameter entries  
#/*=================================================*/
#--- create an field parameter entry for a farm-field-year ---#
initiate_fp_entry(
  farm = "BROB",
  field = "1",
  year = 2021,
  crop  = "wheat",
  json_file = "metadata"
)

#--- create data request folders ---#
make_td_folders(field_data)

#--- create grower data folders ---#
# (final destination of the raw datasets collected from the participating
# farmers)  
make_grower_folders(field_data)

#--- add inputs data (as the details of the trial gets clear) ---#
add_inputs(
  json_file = "metadata.json",
  farm = "BROB",
  field = "1",
  year = "2021",
  input_ls = c("11520ZN", "N_equiv", "UREA46", "UAN32", "N_equiv"),
  product_ls = c("11520ZN", "120026", "UREA46", "UAN32", "KQXRN"),
  strategy_ls = c("trial", "base", "trial", "trial", "base")
)

#/*----------------------------------*/
#' ## Add Rx information
#/*----------------------------------*/
Rx_data <-
  data.table(
    form = c("MAP", "UREA32"),
    model = c("granular", "AdaptN"),
    file = c("Rx_granular.shp", "Rx_AdaptN.shp"),
    date = c("04/01/2021", "04/02/2021")
  )

add_Rx(
  json_file = "metadata.json",
  farm = "BROB",
  field = "1",
  year = "2021",
  Rx_data = Rx_data
)

#/*=================================================*/
#' # Add field parameter templates  
#/*=================================================*/
#--- add field parameter templates ---#

gen_fp_template(
  farm = "Bohnhoff",
  field = "Tims",
  year = "2021",
  crop = "corn",
  input_ls = c("seed", "urea"),
  strategy_ls = c("trial", "trial"),
  json_file = "metadata.json"
)

field_data <- jsonlite::fromJSON(
  here("Data", "CommonData", "fp_2021_TD.json"),
  flatten = TRUE
) %>%
data.table() %>%
.[, field_year := paste(farm, field, year, sep = "_")]

make_grower_folders(field_data)


