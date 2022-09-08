## This script creates creates models for all possible permutations of the 
## parameters manipulated. Models are constructed with a non-random (NRS) split 
## strategies, and a dataset with partial dependency plots is exported as a csv for
## use in constructing figures. 
## 
## Peter Regier
## 2022-04-07 (Updated 2022-05-13)

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
       DALEXtra, #explain_tidymodels
       tictoc)


# Variables treated as constants
source("R_scripts/constants.R")

# Functions used to create models
# source("R_scripts/functions_model.R")

# Set the dependent variable to model
var = "no3"

# 2. Read in datasets ----------------------------------------------------------

## Read in CBV
cbv_data <- read_csv("data/created/cbv_for_models.csv") %>%
  select({{var}}, all_predictors, datetime_round, site) 

## Read in OWC
owc_data <- read_csv("data/created/owc_for_models.csv") %>%
  select({{var}}, all_predictors, datetime_round, site) 


# 3. Set up models for all variables of interest -------------------------------

## First, create a list of variables to calculate PDPs for
pdp_var = c("SpCond", "sin_doy", "Temp")

## Second, create a modified model_list
model_list_pdp <- expand_grid(model_list, pdp_var) %>% 
  filter(predictors == "wq_predictors")

var = "no3"

# 4. Run models ----------------------------------------------------------------

calculate_pdp_test <- function(data = data,
                          predictors = predictors,
                          proportion = proportion,
                          model = model,
                          m_try = m_try,
                          ntree = ntree,
                          model_no = model_no,
                          pdp_var = pdp_var){
  
  ## Create dataset with columns to be used
  model_data <- eval(parse(text = data)) %>%
    dplyr::select({{var}}, eval(parse(text = predictors)), datetime_round, site) %>%
    mutate(dep = eval(parse(text = var))) %>%
    drop_na()
  
  mean_x = mean(model_data %>% select(pdp_var) %>% drop_na() %>% pull())
  sd_x = sd(model_data %>% select(pdp_var) %>% drop_na() %>% pull())
  
  ## Set training/test split
  set.seed(42)
  split_data <- initial_time_split(model_data, prop = proportion)
  
  ## Set up recipe
  model_recipe <- training(split_data) %>%
    dplyr::select(-datetime_round, -{{var}}) %>%
    recipe(dep ~ .) %>%
    step_integer(site) %>%
    step_corr(all_predictors()) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) %>%
    prep()
  
  ## Set up testing data
  testing_data <- model_recipe %>%
    bake(testing(split_data))
  
  n_test = nrow(testing_data)
  
  ## Set up training data (dont need to bake because set up in recipe)
  training_data <- juice(model_recipe)
  
  ## Set seed for before making model
  set.seed(42)
  
  ## Make the actual model
  rf_model <- rand_forest(trees = ntree, mtry = m_try, mode = "regression") %>%
    set_engine(model) %>%
    set_mode("regression")
  
  rf_wflow <- workflow() %>%
    add_formula(dep ~ .) %>%
    add_model(rf_model)
  
  rf_fit <- rf_wflow %>%
    fit(data = training_data)
  
  explainer_rf <- explain_tidymodels(
    rf_fit,
    data = dplyr::select(training_data, -dep),
    y = training_data$dep)
  
  pdp_rf <- as_tibble(model_profile(explainer_rf, variables = pdp_var)$agr_profiles) %>%
    dplyr::rename("dep" = `_yhat_`) %>%
    mutate(data = data,
           dep = dep,
           predictors = predictors,
           proportion = proportion,
           model = model,
           m_try = m_try,
           ntree = ntree,
           n_test = n_test,
           model_no = model_no,
           mean_x = mean_x,
           sd_x = sd_x)
  
  ## This line just shows which model is run (keep track of progress)
  print(paste(model, data , predictors , proportion, m_try, ntree))
  
  return(pdp_rf)
}

n = 10
tic("run models")
pdp_raw <- model_list_pdp %>% 
  #slice(1:n) %>% 
  pmap(calculate_pdp_test) %>% #pmap applies the function to each row
  bind_rows() %>% #recombine output into a single dataset 
  mutate(dataset = toupper(substr(data, 1, 3)))

pdp <- pdp_raw %>% 
  mutate(x = (`_x_` * sd_x) + mean_x) %>% 
  rename("predictor" = `_vname_`) %>% 
  select(-`_label_`)

write_csv(pdp, "data/created/pdp_data.csv")
toc()





