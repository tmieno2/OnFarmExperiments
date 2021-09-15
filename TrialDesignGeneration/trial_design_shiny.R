library(shiny)
library(sf)
library(data.table)
library(lwgeom)
library(tidyverse)
library(measurements)

source("https://github.com/tmieno2/DIFM/blob/master/Functions/functions_for_trial_design.R?raw=TRUE")

read_shp <- function(input_shp) {

  prev_wd <- getwd()
  upload_directory <- dirname(input_shp$datapath[1])
  setwd(upload_directory)
  for (i in 1:nrow(input_shp)){
    file.rename(input_shp$datapath[i], input_shp$name[i])
  }
  shp_name <- input_shp$name[
    grep(x = input_shp$name, pattern = "*.shp")
  ]
  shp_path <- paste(upload_directory, shp_name, sep = "/")
  setwd(prev_wd)

  return_sf <- st_read(shp_path)

  return(return_sf)

}

make_td_design <- function(input) {
  #/*----------------------------------*/
  #' ## field boundary
  #/*----------------------------------*/
  field <- read_shp(input$boundary_shp)

  if (is.na(st_crs(field))){
    field <- st_set_crs(field, 4326) 
  } 

  field <- field %>% 
    st_make_valid() %>%
    st_transform_utm()

  #/*----------------------------------*/
  #' ## ab_line
  #/*----------------------------------*/
  ab_line <- read_shp(input$abline_shp)

  if (is.na(st_crs(ab_line))){
    ab_line <- st_set_crs(ab_line, 4326) 
  } 

  ab_line <- ab_line %>% 
    st_make_valid() %>%
    st_transform_utm()

  #/*----------------------------------*/
  #' ## Other parameters
  #/*----------------------------------*/
  #=== headland length ===#
  headland_length <- input$headland_length %>% 
    conv_unit("ft", "m")

  #=== plot width ===#
  plot_width <- input$plot_width %>% 
    conv_unit("ft", "m")

  #=== grower-chosen rate ===#
  grower_chosen_rate <- input$grower_chosen_rate

  experiment_plots <- make_trial_grids(
    field = field, 
    #--- by default uses the first one ---#
    ab_line = ab_line[1, ], 
    plot_width = plot_width, 
    cell_height = conv_unit(10, "ft", "m"),
    headland_length = headland_length
  )

  #/*----------------------------------*/
  #' ## Create headland
  #/*----------------------------------*/
  experiment_plots_dissolved <- experiment_plots %>% 
    st_snap_to_grid(size = 0.0001) %>%
    st_make_valid() %>% 
    summarize(plot_id = min(plot_id))

  headland <- st_difference(field, experiment_plots_dissolved) %>%
    dplyr::select(geometry) %>% 
    #--- assign grower-chosen rate to the headland ---#
    mutate(rate = grower_chosen_rate)

  experiment_design <- assign_rates(
    data_sf = experiment_plots, 
    rates_ls = c(0.5, 0.65, 0.8, 1, 1.15, 1.3) * grower_chosen_rate,
    # pattern = "block_randomized"
    pattern = "fixed-latin-square"
    # pattern = "sequential"
  )  %>% 
  #--- keep cell_id here for orthogonality check later ---#
  dplyr::select(rate)

  trial_design <- rbind(
    experiment_design,
    headland
  )

  return(trial_design)
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Trial Design"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----

      fileInput(
        inputId = "abline_shp",
        label = "ab-line",
        multiple = TRUE
      ),

      fileInput(
        inputId = "boundary_shp",
        label = "Field Boundary",
        multiple = TRUE
      ),

      numericInput(
        inputId = "headland_length",
        label = "Headland Length (feet)",
        value = 90
      ),

      numericInput(
        inputId = "plot_width",
        label = "Plot Width (feet)",
        value = 30
      ),

      numericInput(
        inputId = "grower_chosen_rate",
        label = "What would have been your application rate?",
        value = 180
      ),

      actionButton(
        inputId = "make_td", 
        label = "Generate a Trial Design", 
        class = "btn-success"
      )

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      plotOutput(outputId = "trial_design_plot")

    )
  )
)

server <- function(input, output) {

  observeEvent(
    input$make_td,
    {
      output$trial_design_plot <- renderPlot({

        if (input$make_td == 0) {
          return()
        } else {
          isolate({
            ab_exists <- !is.null(input$abline_shp)
            boundary_exists <- !is.null(input$boundary_shp)
            hl_exists <- !is.null(input$headland_length)
            pw_exists <- !is.null(input$plot_width)
            gcr_exists <- !is.null(input$grower_chosen_rate)

            if (ab_exists & boundary_exists & hl_exists & pw_exists & gcr_exists) {
              

              trial_design <- make_td_design(input)

              tm_td <- tm_shape(trial_design) +
                tm_fill(
                  col = "rate", 
                  palette = "YlGn", 
                  style = "cat"
                ) + 
              tm_shape(headland) +
                tm_borders(
                  lwd = 2
                ) +
              tm_shape(ab_line[1, ]) +
                tm_lines(
                  col = "red",
                  lwd = 2
                ) +
              tm_layout_to_add

              return(tm_td)    
            }
          })
        }
      })
    }
  )

  

}

shinyApp(ui = ui, server = server)

