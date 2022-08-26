## This script takes all the raw data files downloaded from NERR and combines them
## into two datasets, one for each NERR site, which are used for modeling. 
##
## General steps
## 1. read in data by site and type
## 2. process those data (clean up based on flags, bin to appropriate intervals)
## 3. merge together with emphasis on maintaining highest sample size
##
## QC is based on NERR's flagging approach:  https://cdmo.baruch.sc.edu/data/qaqc.cfm
##
## Data types
## nutrients/chlorophyll = nuts
## water quality = wq
## meteorology / climate = met
##
## Data are labeled as CBV (Chesapeake Bay Virginia) or OWC (Old Woman Creek)
##
## Authors: Peter Regier and Matt Duggan
##
## Date Created: 2021-05-27
## Laste Updated: 2022-08-22
##
## Email: peter.regier@pnnl.gov
##
## ---------------------------



# 1. Setup ----------------------------------------------------------------

  #load packages using 'pacman'
  require(pacman) # easy way to load packages
  pacman::p_load(hms, #extract time from datetime, 
                 lubridate, #work with dates
                 parsedate, #auto-parse dates without being told format
                 tidyverse, #data wrangling
                 tictoc, #timing things
                 furrr, #parallel for purrr
                 purrr) #tools to iterate rowwise through a dataframe (used for pmap)
  
  
  # Read in Constants
  source("R_scripts/constants.R")
  
  # Read in functions for preparing data
  source("R_scripts/functions_prep.R")

  # Set filepath for writing out data
  filepath_out = "data/created/"

# 2. read nutrients -------------------------------------------------------

  # Read in pre-processed nutrients
  cbv_nuts <- read_dir(paste0(cbv_directory, "nutrients")) %>% 
                process_nuts()
  
  owc_nuts <- read_dir(paste0(owc_directory, "nutrients")) %>% 
                process_nuts()


# 3. read water quality --------------------------------------------------------------

  cbv_wq <- read_dir(paste0(cbv_directory, "water_quality")) %>% 
              prep_wq() %>% 
              bin_wq()
  
  owc_wq <- read_dir(paste0(owc_directory, "water_quality")) %>% 
              prep_wq() %>% 
              bin_wq()

# 4. read meteorology -------------------------------------------------------------

  ## First, read in raw data
  cbv_met_raw <- read_dir(paste0(cbv_directory, "meteorology")) 
  owc_met_raw <- read_dir(paste0(owc_directory, "meteorology"))
  
  ## Clean and bin CBV data
  cbv_met <- prep_met(cbv_met_raw) %>% 
    bin_met()
  
  ## Clean and bin OWC data
  owc_met <- prep_met(owc_met_raw) %>% 
    bin_met()


# 5. Combine data ---------------------------------------------------------

  # Return combined high frequency data
  hf_cbv <- left_join(cbv_wq, cbv_met, by="datetime_round") %>% 
              select(-datetime_site)
  
  hf_owc <- left_join(owc_wq, owc_met, by="datetime_round") %>% 
              select(-datetime_site)


  #### Combine nutrient and hf data ####
  
    # set up parallel
    future::plan(multisession)
  
    # use calc_stats. functions to combine high frequency data with nutrients
  tic("combine datasets")
    cbv_combined <- prep_vectors(cbv_nuts, 1) %>% 
      future_pmap(., calc_stats_cbv) %>% 
      bind_rows(.) %>% 
      full_join(cbv_nuts, ., by = c("datetime_round" = "datetime_round", "site" = "site")) %>% 
      filter(na_rows(.) == FALSE)

    owc_combined <- prep_vectors(owc_nuts, 1) %>% 
      future_pmap(., calc_stats_owc) %>% 
      bind_rows(.) %>% 
      full_join(owc_nuts, ., by = c("datetime_round" = "datetime_round", "site" = "site")) %>% 
      filter(na_rows(.) == FALSE)
toc()


# use calc_stats. functions to combine high frequency data with nutrients
tic("combine 'first' datasets")
cbv_combined_first <- prep_vectors(cbv_nuts, 1) %>% 
  future_pmap(., calc_stats_cbv_first) %>% 
  bind_rows(.) %>% 
  full_join(cbv_nuts, ., by = c("datetime_round" = "datetime_round", "site" = "site")) %>% 
  filter(na_rows(.) == FALSE)

owc_combined_first <- prep_vectors(owc_nuts, 1) %>% 
  future_pmap(., calc_stats_owc_first) %>% 
  bind_rows(.) %>% 
  full_join(owc_nuts, ., by = c("datetime_round" = "datetime_round", "site" = "site")) %>% 
  filter(na_rows(.) == FALSE)
toc()

# 7. Save data ------------------------------------------------------------
  
  # Write out data for models
  write_csv(cbv_combined %>% filter(!is.na(datetime_round)) %>% 
              mutate(sin_doy = sin((yday(lubridate::date(datetime_round)) / 365.25) * pi)) %>% 
              filter(no3 < 1), 
            paste0(filepath_out, "cbv_for_models.csv"))
  write_csv(owc_combined %>% filter(!is.na(datetime_round)) %>% 
              mutate(sin_doy = sin((yday(lubridate::date(datetime_round)) / 365.25) * pi)), 
            paste0(filepath_out, "owc_for_models.csv"))
  
  # Write out high-frequency data for use later
  write_csv(hf_cbv %>% filter(datetime_round >= "2002-01-01"), paste0(filepath_out, "cbv_hf_wq.csv"))
  write_csv(hf_owc %>% filter(datetime_round >= "2002-01-01"), paste0(filepath_out, "owc_hf_wq.csv"))


