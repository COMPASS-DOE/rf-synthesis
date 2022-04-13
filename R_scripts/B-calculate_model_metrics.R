## This script creates creates models for all possible permutations of the 
## parameters manipulated. Models are constructed both using random (OOB) and 
## non-random (NRS) split strategies, and a dataset with metrics is exported 
## as a csv for use in constructing figures. 
## 
## Peter Regier
## 2022-04-07

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
       furrr,
       readr, 
       tictoc)


# Variables treated as constants
source("R_scripts/constants.R")

# Functions used to create models
source("R_scripts/functions_model.R")

# Set the dependent variable to model
var = "no3"

# 2. Read in datasets ----------------------------------------------------------

## Read in CBV
cbv_data <- read_csv("data/created/cbv_for_models.csv") %>%
  select({{var}}, all_predictors, datetime_round, site) 

## Read in OWC
owc_data <- read_csv("data/created/owc_for_models.csv") %>%
  select({{var}}, all_predictors, datetime_round, site) 


# 2. Run models ----------------------------------------------------------------

## This function calculates fit and error metrics (R2adj, NSE, MAE) using
## random sampling to split into training and test datasets
calculate_initial_metrics_oob <- function(data = data, 
                                             predictors = predictors, 
                                             proportion = proportion, 
                                             model = model, 
                                             m_try = m_try, 
                                             ntree = ntree, 
                                             model_no = model_no){
  
  ## Create dataset with columns to be used
  model_data <- eval(parse(text = data)) %>% 
    dplyr::select({{var}}, eval(parse(text = predictors)), datetime_round, site) %>% 
    mutate(dep = eval(parse(text = var))) %>% 
    drop_na()
  
  ## Set training/test split
  set.seed(42)
  split_data <- initial_split(model_data, prop = proportion)
  
  ## Set up recipe
  model_recipe <- training(split_data) %>% 
    dplyr::select(-datetime_round) %>% 
    recipe(dep ~ .) %>% 
    step_integer(site) %>% 
    step_corr(all_predictors()) %>%
    step_normalize(all_predictors(), -all_outcomes()) %>% 
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
    fit(dep ~ ., data = training_data)
  
  ## Calculate predicted variables
  model_fit <- testing_data %>% 
    mutate(predict(rf_model, testing_data)) 
  
  ## Calculate metrics to assess model performance
  rmse = hydroGOF::rmse(model_fit$.pred, model_fit$dep)
  mae = hydroGOF::mae(model_fit$.pred, model_fit$dep)
  r2 = hydroGOF::gof(model_fit$.pred, model_fit$dep)["R2", ]
  nse = hydroGOF::NSE(model_fit$.pred, model_fit$dep)
  
  ## This line just shows which model is run (keep track of progress)
  print(paste(model, data , predictors , proportion, m_try, ntree))
  
  ## All the information exported (one row per model)
  output = tibble(data = data, 
                  dep = {{var}},
                  model = model, 
                  predictors = predictors, 
                  proportion = proportion, 
                  m_try = m_try, 
                  ntree = ntree,
                  rmse = rmse, 
                  mae = mae, 
                  r2 = r2, 
                  nse = nse, 
                  n_test = n_test)
}

## This function calculates fit and error metrics (R2adj, NSE, MAE)
calculate_initial_metrics_nrs <- function(data = data, 
                                      predictors = predictors, 
                                      proportion = proportion, 
                                      model = model, 
                                      m_try = m_try, 
                                      ntree = ntree, 
                                      model_no = model_no){
  
  ## Create dataset with columns to be used
  model_data <- eval(parse(text = data)) %>% 
    dplyr::select({{var}}, eval(parse(text = predictors)), datetime_round, site) %>% 
    mutate(dep = eval(parse(text = var))) %>% 
    drop_na()
  
  ## Set training/test split
  set.seed(42)
  split_data <- initial_time_split(model_data, prop = proportion)
  
  ## Set up recipe
  model_recipe <- training(split_data) %>% 
    dplyr::select(-datetime_round) %>% 
    recipe(dep ~ .) %>% 
    step_integer(site) %>% 
    step_corr(all_predictors()) %>%
    step_normalize(all_predictors(), -all_outcomes()) %>% 
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
    fit(dep ~ ., data = training_data)
  
  ## Calculate predicted variables
  model_fit <- testing_data %>% 
    mutate(predict(rf_model, testing_data)) 
  
  ## Calculate metrics to assess model performance
  rmse = hydroGOF::rmse(model_fit$.pred, model_fit$dep)
  mae = hydroGOF::mae(model_fit$.pred, model_fit$dep)
  r2 = hydroGOF::gof(model_fit$.pred, model_fit$dep)["R2", ]
  nse = hydroGOF::NSE(model_fit$.pred, model_fit$dep)
  
  ## This line just shows which model is run (keep track of progress)
  print(paste(model, data , predictors , proportion, m_try, ntree))
  
  ## All the information exported (one row per model)
  output = tibble(data = data, 
                  dep = {{var}},
                  model = model, 
                  predictors = predictors, 
                  proportion = proportion, 
                  m_try = m_try, 
                  ntree = ntree,
                  rmse = rmse, 
                  mae = mae, 
                  r2 = r2, 
                  nse = nse, 
                  n_test = n_test)
}

n = 2
tic("run models")
models_oob <- model_list %>% 
  #slice(1:n) %>% 
  pmap(calculate_initial_metrics_oob) %>% #pmap applies the function to each row
  bind_rows() #recombine output into a single dataset

models_nrs <- model_list %>% 
  #slice(1:n) %>% 
  pmap(calculate_initial_metrics_nrs) %>% #pmap applies the function to each row
  bind_rows() #recombine output into a single dataset
toc()

## Join OOB and NRS models
models <- bind_rows(models_oob %>% mutate(split = "OOB"), 
                    models_nrs %>% mutate(split = "NRS")) %>% 
  mutate(dataset = toupper(substr(data, 1, 3)))

## Write out data
write_csv(models, "data/created/model_metrics.csv")
