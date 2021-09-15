# Folder structure

+ Codes/
    * Analysis/
    * DataProcessing/
    * Functions/
    * Organization/
    * Report/
    * TrialDesignGeneration/
+ Data/
    * CommonData/
        - FieldParameter
    * Disposable/
        - any temporary datasets you may use
    * Growers/ (individual farm_field_year)
        - Raw/
            + boundary
            + raw-yield
            + as-planted
            + as-applied
        - Intermediate/
        - TrialDesign/
        - DataProcessingReport/
        - Conversations/
        - Analysis-Ready/ (all the files you need to create reports)
            + analysis data
            + processed yield
            + as-planted
            + as-applied
    * TrialDesignDataRequest/ (individual farm_field_year)
+ Reports/
    * Growers/ (reports for growers)
    * ProjectTeam/

# Naming convention

## Datasets stored in "Raw"

+ name raw yield shape file as **raw-yield.shp**
+ name as-planted seed rate shape file as **as-applied-s.shp**
+ name as-applied nitrogen rate shape file as **as-applied-n.shp**
+ name as-applied potassium rate shape file as **as-applied-k.shp**
+ name boundary shape file as **boundary.shp**
+ name trial design shape file as **trial-design.shp**
+ name soil sampling shape file as **soil-sampling.shp**
+ name EC shape (tif) file as **ec.shp (.tif)**
+ name ab-lines shape file as **ab-line.shp**

Do **NOT** put field or farm names, or year in any of these files.  

# How to use the codes

## Data processing

`A01_data_processing.R` downloads, processes, and prepares datasets ready for analysis and reports. It also creates data processing reports to help the data processor to gain insights into the performance of the data processing and potential problems.

For the Non-experiment files, you only need **boundary.shp** shape file. For the Experiment files, you need **raw-yield.shp** and **as-planted.shp** shape files.

+ `non_exp_process_make_report()` will run the following codes to obtain and process non-experiment data:
    * **ne01_topography.Rmd**
    * **ne02_ssurgo.Rmd**
    * **ne03_weather.Rmd**
    * **ne04_ec.Rmd**
    * **ne05_soil_sampling.Rmd** (has not been implemented yet)

+ `exp_process_make_report()` will run the following codes to process experiment data:
    * **e01_reduce_as_planted.Rmd**:
        - input: **Raw/as-planted.shp**
        - output: **Intermediate/as_planted_reduced.rds**
        - checks unit, variable names, etc
        - create yield polygons around each of the yield points
    * **e02_gen_yield_polygons.Rmd**:
        - input: **Raw/raw-yield.shp**
        - output: **Intermediate/yield_polygons.rds**
        - checks unit, variable names, etc
        - create yield polygons around each of the yield points
    * **e03_gen_as_planted_polygons.Rmd**:
        - input: **Intermediate/as_planted_reduced.rds**
        - output: **Intermediate/as_planted_polygons.rds**
        - create as-planted polygons around each of the as-planted points
    * **e04_yield_further_processing.Rmd**:
        - input: **Intermediate/as_planted_polygons.rds** and **Intermediate/yield_polygons.rds**
        - output: **Intermediate/yield_group_by_obs.rds**
        - combine yield and as-planted polygons data   
        - group observations to make up observation units

+ `f_process_make_report` will run the following code to combine all the datasets:
    * **f01_combine_all_datasets.Rmd**
    * output: **analysis_data.rds**

## Run analysis and create report

`A01_analysis_report.R` run analyses and create report. It runs the following functions:

+ `run_analysis()` will run the following code to run analyses
    * **a01_analysos_soy.Rmd**
+ `make_report()`: reports will be saved to **Reports/field-year/grower_report.html**
    * **r01_make_report_html.Rmd**

# Field Parameter

## Definition

Go to **Data/CommonData/FieldParameter_metadata.md**

## Data entry rules


# Trial Deign Workflow for Field Trial Supervisors

The Field Trial Supervisors are responsible for working with the members of the Trial Implementation Team to get the initial data and information from the farmers and into the data file system. These data are 

+ boundary file
+ ab-lines file
+ past yield files (optional)
+ past as-applied files (optional), 
+ Rx files (optional) 

Once participations from farmers are confirmed, use `tools_for_trial_design_coordinators.R` to do the following:

+ run `initiate_fp_entry()` to add an field parameter entry to the farmer to **field_parameter.json**  
+ run `make_td_folders()` to create folders to which you can ask farmers to upload their data files
+ run `make_grower_folders()` to create grower data folders, which is the final destination of the raw datasets collected from the participating farmers 
+ communicate with the farmers and agree on the specifics of their trial designs (**Important: all communication should go through the Field Trial Implementation part of the Trello Board**)
+ use `add_input()` to add input information to the existing field parameter entries for the farmers (which was created at the very first step) 
+ review the data files shared by the farmers and move the relevant files into **Raw/** folder for the farmers (**Important: follow the File Naming Convention section above**). 


# Misc Notes

+ Update the `FedData` package using `devtools::install_github("ropensci/FedData")`, otherwise, `get_ssurgo()` could fail.

