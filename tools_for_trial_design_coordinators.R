######################################
# R codes to assist Trial Design Coordinators
######################################

# /*=================================================*/
#' # Preparation
# /*=================================================*/
# /*----------------------------------*/
#' ## Load packages and source functions
# /*----------------------------------*/
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

# /*=================================================*/
#' # Create new field parameter entries
# /*=================================================*/
#--- create an field parameter entry for a farm-field-year ---#
initiate_fp_entry(
  farm = "MMUR",
  field = "1",
  year = 2021,
  crop = "wheat",
  json_file = "metadata"
)

# #--- create data request folders ---#
# make_td_folders(field_data)

#--- create grower data folders ---#
# (final destination of the raw datasets collected from the participating
# farmers)
make_grower_folders(field_data)

#--- add inputs data (as the details of the trial gets clear) ---#
add_inputs(
  json_file = "metadata.json",
  farm = "MMUR",
  field = "1",
  year = "2021",
  file_names_ls = c(NA, "21_MMUR_W_SN_FA1", "21_MMUR_W_SN_HF1")
  input_ls = c("N_equiv", "UAN32", "UAN32"),
  product_ls = c("11520", "UAN32", "UAN32"),
  strategy_ls = c("base", "trial", "trial"),
  var_name_prefix_ls = c(NA, "FA1", "HF1")
)

# /*----------------------------------*/
#' ## Add Rx information
# /*----------------------------------*/
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

# /*----------------------------------*/
#' ## Add Ex information
# /*----------------------------------*/
Ex_data <-
  data.table(
    data_type = c("NDRE"),
    file = c("21_BROB_W_SN_SRO.shp"),
    date = c("04/02/2021"),
    vars = list(c("NDVI", "Tgt_Rate_N"))
  )

add_Ex(
  json_file = "metadata.json",
  farm = "BROB",
  field = "1",
  year = "2021",
  Ex_data = Ex_data
)

# #/*=================================================*/
# #' # Add field parameter templates
# #/*=================================================*/
# #--- add field parameter templates ---#

# gen_fp_template(
#   farm = "Bohnhoff",
#   field = "Tims",
#   year = "2021",
#   crop = "corn",
#   input_ls = c("seed", "urea"),
#   strategy_ls = c("trial", "trial"),
#   json_file = "metadata.json"
# )

# field_data <- jsonlite::fromJSON(
#   here("Data", "CommonData", "fp_2021_TD.json"),
#   flatten = TRUE
# ) %>%
# data.table() %>%
# .[, field_year := paste(farm, field, year, sep = "_")]

# make_grower_folders(field_data)