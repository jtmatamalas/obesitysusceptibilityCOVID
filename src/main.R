### ----------------------------------------------------------------------------
### LOADING PACKAGES
### ----------------------------------------------------------------------------

## Data manipulation
library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)
library(plyr)

## Data visualization
library(ggplot2)
library(cowplot)
library(ggthemes)
library(ggsci)
library(scales)
library(rgdal)
library(sf)
library(sp)
library(patchwork)
library(plotly)
library(wesanderson)
library(paletteer)
library(ggrepel)

## Data tables
library(gt)

## Data analysis
library(nlme)

## Miscelaneous
library(ascii)
library(parallel)
library(pROC)

### ----------------------------------------------------------------------------
### LOADING MODULES
### ----------------------------------------------------------------------------

modules_path <- "./src/"

## Verbosity
verbose <- FALSE

## Loading Data
source(sprintf("%s/load_data.R", modules_path))

## Build cohort
source(sprintf("%s/cohort_building.R", modules_path))

## Make a comparison between MGB and mass
source(sprintf("%s/mgb_vs_mass.R", modules_path))

## Perform logistic regression analysis
source(sprintf("%s/logistic_analysis.R", modules_path))

## Generate summary table
source(sprintf("%s/summary_table.R", modules_path))

## Generate figures
source(sprintf("%s/figures.R", modules_path))
