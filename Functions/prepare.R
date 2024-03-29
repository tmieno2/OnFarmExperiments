#--- load all the packages ---#
cat("Loading some generic packages.\n\n")

library(here)
library(rmarkdown)
library(sf)
library(data.table)
library(bookdown)
library(knitr)
library(parallel)
library(tmap)
library(stringr)
library(jsonlite)
library(measurements)
library(tidyverse)

#--- source functions from github accounts ---#
cat("Sourcing functions from github repositories.\n\n")

source("https://raw.githubusercontent.com/brittanikedge/DIFM/main/Functions.R")
source("https://github.com/tmieno2/OnFarmExperiments/blob/master/Functions/functions_for_analysis.R?raw=TRUE")
source("https://github.com/tmieno2/OnFarmExperiments/blob/master/Functions/functions_for_execution.R?raw=TRUE")
source("https://github.com/tmieno2/OnFarmExperiments/blob/master/Functions/functions_for_organization.R?raw=TRUE")
source("https://github.com/tmieno2/OnFarmExperiments/blob/master/Functions/functions_for_processing.R?raw=TRUE")
source("https://github.com/tmieno2/OnFarmExperiments/blob/master/Functions/functions_for_trial_design.R?raw=TRUE")

#/*=================================================*/
#' # Figure theme
#/*=================================================*/
fig_theme_nonsp <- theme(
  axis.title.x = element_text(
    size = 12,
    family = "Times"
  ),
  axis.title.y  =  element_text(
    size = 12,
    family = "Times"
  ),
  axis.text  =  element_text(
    size = 10,
    family = "Times"
  ),
  axis.ticks  =  element_line(
    size = 0.3, 
    linetype = "solid"
  ),
  axis.ticks.length  =  unit(.15, 'cm'),
  #--- legend ---#
  legend.text  =  element_text(
    size = 12,
    family = "Times"
  ),
  legend.title  =  element_text(
    size = 12,
    family = "Times"
  ),
  legend.key.size  =  unit(0.6, "cm"),
  #--- strip (for faceting) ---#
  strip.text  =  element_text(
    size  =  12, 
    family = "Times"
  ),
  #--- panel ---#
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_rect(fill=NA)
)
 
fig_theme_sp <- theme(
  axis.text.y = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.x = element_blank(),
  axis.line.y = element_blank(),
  #--- panel ---# 
  panel.border = element_blank(),
  panel.grid.major = element_line(color = "transparent"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.background = element_blank(),
  # panel.background = element_rect(fill = "transparent"),
  #--- plot ---#
  plot.title = element_text(
    family = "Times", 
    face = "bold", 
    size = 12
  ),
  plot.margin = margin(0, 0, 0, 0, "cm"),
  plot.background = element_rect(
    fill = "transparent", 
    color = "transparent"
  ),
  #--- legend ---#
  legend.text = element_text(
    size = 12,
    family = "Times"
  ),
  legend.title = element_text(
    size = 12,
    family = "Times"
  ),
  legend.key.size = unit(0.6, "cm"),
  #--- strip (for faceting) ---#
  strip.text = element_text(
    size = 12,
    family = "Times"
  )
)
 