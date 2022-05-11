## This script creates creates models for all possible permutations of the 
## parameters manipulated. Models are constructed with a non-random (NRS) split 
## strategies, and a dataset with feature importance is exported as a csv for
## use in constructing figures. 
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

## This function calculates fit and error metrics (R2adj, NSE, MAE)
## This function calculates feature importance
calculate_feature_importance <- function(data = data, 
                                         predictors = predictors, 
                                         proportion = proportion, 
                                         model = model, 
                                         m_try = m_try, 
                                         ntree = ntree, 
                                         model_no = model_no){
  
  ## Create dataset with columns to be used
  model_data <- eval(parse(text = data)) %>% 
    dplyr::select(no3, eval(parse(text = predictors)), datetime_round, site) %>% 
    mutate(dep = eval(parse(text = var))) %>% 
    drop_na()
  
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
  
  importance_string = ifelse(model == "ranger", "impurity", TRUE)
  
  ## Make the actual model
  rf_model <- rand_forest(trees = ntree, mtry = m_try, mode = "regression") %>%
    set_engine(model, importance = importance_string) %>%
    fit(dep ~ ., data = training_data)
  
  ## Identify what column to pull for feature importance (changes w/ model call)
  var_names <- ifelse(model == "ranger", "rf_model$fit$variable.importance", 
                      "rf_model$fit$importance")
  
  col_names <- ifelse(model == "ranger", c("predictors", "raw_fi"), 
                      c("predictors", "to_delete", "raw_fi"))
  
  fi0 <- as.data.frame(eval(parse(text = var_names))) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    select(1, last_col())
  
  colnames(fi0) = c("predictor", "raw_fi")
  
  fi = fi0 %>% 
    filter(predictor != {{var}}) %>% #remove dep from before calculating FI
    select(predictor, raw_fi) %>% 
    mutate(fi = raw_fi / sum(raw_fi), 
           data = data, 
           dep = {{var}},
           predictor_set = predictors, 
           proportion = proportion, 
           model = model, 
           m_try = m_try, 
           ntree = ntree, 
           n_test = n_test, 
           model_no = model_no)
  
  ## This line just shows which model is run (keep track of progress)
  print(paste(model, data , predictors , proportion, m_try, ntree))
  
  return(fi)
}

n = 2
tic("run models")
feature_importance <- model_list %>% 
  #slice(1:n) %>% 
  pmap(calculate_feature_importance) %>% #pmap applies the function to each row
  bind_rows() %>% #recombine output into a single dataset 
  mutate(dataset = toupper(substr(data, 1, 3)))

## Write out data
write_csv(feature_importance, "data/created/model_feature_importance.csv")

x <- feature_importance %>%
  group_by(predictor, data, predictor_set) %>% 
  summarize(median_fi = median(fi), 
            min_fi = min(fi), 
            max_fi = max(fi)) 

ggplot(x, aes(x = predictor, y = median_fi, fill = predictor)) + 
  geom_col(show.legend = F) + 
  coord_flip() +
  facet_wrap(data~predictor_set, scales = "free") 

  ggplot(x, 
       aes(reorder(predictor, -median_fi, sum), fill = predictor)) + 
  geom_col(aes(y = median_fi), show.legend = F) + 
  geom_errorbar(aes(ymin = min_fi, ymax = max_fi), width = 0.2) + 
  labs(x = "Predictor", y = "Feature Importance", title = title) + 
  scale_x_discrete(limits=rev) + 
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~predictor_set)


