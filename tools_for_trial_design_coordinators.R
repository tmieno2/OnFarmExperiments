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
  farm = "NTHO",
  field = "1",
  year = 2021,
  crop = "corn",
  json_file = "metadata"
)

# #--- create data request folders ---#
# make_td_folders(field_data)

#--- create grower data folders ---#
# (final destination of the raw datasets collected from the participating
# farmers)
make_grower_folders(field_data)

input_data <-
  data.table(
    form = c("N_equiv", "UAN32", "UAN32"),
    product = c("11520", "UAN32", "UAN32"),
    strategy = c("base", "trial", "trial"),
    file_name = c(NA, "21_MMUR_W_SN_FA1", "21_MMUR_W_SN_HF1"),
    unit = c("lbs", "gallons", "gallons"),
    date = c("mm/dd/yyyy", "mm/dd/yyyy", "mm/dd/yyyy"),
    Rx_data = c("none", "none", "none"),
    machine_width = c(NA, 90, 40),
    rate = c(32, NA, NA),
    var_name_prefix = c("none", "FA1", "HF1")
  )

#--- Laila working version ---#
input_data <-
  data.table(
    form = c("NH3", "N_equiv", "UAN32"),
    product = c("NH3", "MAP", "UAN32"),
    strategy = c("trial", "base", "trial"),
    file_name = c("21_NTHO_C_ML_FA1", NA, "21_NTHO_C_ML_FA2"),
    unit = c("lbs", "lbs", "gallons"),
    date = c("12/21/2020", "mm/dd/yyyy", "06/10/2021"),
    Rx_data = c("none", "none", "none"),
    machine_width = c(30, NA, 30),
    rate = c(NA, 11, NA),
    var_name_prefix = c("FA1", "none", "FA2")
  )


#--- add inputs data (as the details of the trial gets clear) ---#

add_inputs(
  json_file = "metadata.json",
  farm = "MMUR",
  field = "1",
  year = "2021",
  input_data = input_data
)

#--- Laila working version ---#
add_inputs(
  json_file = "metadata.json",
  farm = "NTHO",
  field = "1",
  year = "2021",
  input_data = input_data
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

#--- Laila working version ---#
Rx_data <-
  data.table(
    form = c("UAN32"),
    model = c("granular"),
    file = c("21_NTHO_C_ML_GRX.shp"),
    date = c("04/01/2021")
  )

add_Rx(
  json_file = "metadata.json",
  farm = "NTHO",
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
  farm = "DSTE",
  field = "1",
  year = "2021",
  Ex_data = Ex_data
)


Ex_data <-
  data.table(
    data_type = c("NDRE"),
    file = c("21_MMUR_W_SN_HF1.shp"),
    date = c("04/02/2021"),
    vars = list(c("NDRE", "Tgt_Rate_g"))
  )

add_Ex(
  json_file = "metadata.json",
  farm = "MMUR",
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