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
  farm = "DROL",
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
    form = c("UAN32"),
    product = c( "UAN32"),
    strategy = c("trial"),
    file_name = c("21_DROL_C_ML_FA1"),
    unit = c("gallons"),
    date = c("07/01/2021"),
    Rx_data = c("21_DROL_C_ML_RX2"),
    machine_width = c(60),
    rate = c(NA),
    var_name_prefix = c("FA1")
  )


#--- add inputs data (as the details of the trial gets clear) ---#

add_inputs(
  json_file = "metadata.json",
  farm = "KMED2",
  field = "1",
  year = "2021",
  input_data = input_data
)

#--- Laila working version ---#
add_inputs(
  json_file = "metadata.json",
  farm = "DROL",
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
    model = c("adaptn"),
    file = c("21_DROL_C_ML_ADX.shp"),
    date = c("07/01/2021")
  )

add_Rx(
  json_file = "metadata.json",
  farm = "DROL",
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

#--- Laila working version ---#
Ex_data <-
  data.table(
    data_type = c("NDRE"),
    file = c("21_DOBE_W_SN_HF1.shp"),
    date = c("04/05/2021"),
    vars = list(c("NDRE")),
    var_name_prefix = c("HF1")
  )


Ex_data <-
  data.table(
    data_type = c("MZ"),
    file = c("21_DROL_C_ML_MZ.shp"),
    date = c("07/01/2021"),
    vars = list(c("Id", "Zone", "Yield", "EY")),
    var_name_prefix = c("MZ")
  )


Ex_data <-
  data.table(
    data_type = c("TR"),
    file = c("21_DROL_C_ML_TR.shp"),
    date = c("07/01/2021"),
    vars = list(c("n_base_32")),
    var_name_prefix = c("TR")
  )


add_Ex(
  json_file = "metadata.json",
  farm = "DROL",
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