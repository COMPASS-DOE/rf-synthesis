## This script creates creates
## 
## Peter Regier
## 2022-04-01

# 1. Setup ---------------------------------------------------------------------

# Clean workspace
#rm(list = ls())

# Load packages
require(pacman)
p_load(tidymodels, 
       tidyverse,
       lubridate,
       hydroGOF, 
       cowplot, 
       readr, 
       PNWColors)


# Variables treated as constants
source("R scripts/constants.R")





