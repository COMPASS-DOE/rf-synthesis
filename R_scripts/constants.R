## Constants used across all scripts to maintain continuity

## Set directories
cbv_directory = "data/cbv/"
owc_directory = "data/owc/"

## Bin rate
bin_rate = "15 min"

## Set columns for independent and dependent variables
dependents <- c("chla", "nh4", "no3", "po4")

## Water quality predictors
wq_predictors <- c("Temp", 
                   "SpCond", 
                   "DO_mgl", 
                   "Depth", 
                   "pH", 
                   "Turb", 
                   "sin_doy", 
                   "site")

## Meterological predictors
met_predictors <- c("ATemp", 
                    "RH", 
                    "BP", 
                    "WSpd", 
                    "Wdir", 
                    "TotPAR", 
                    "sin_doy", 
                    "site")

## List of flag columns
flag_list <- c(0, 4)

## Merge wq and met predictor variables
all_predictors <- unique(append(wq_predictors, met_predictors))

# Set ggplot theme
theme_set(theme_bw())

# 3. Prep list of models to run ------------------------------------------------

datasets = c("cbv_data", "owc_data")
model_packages = c("randomForest", "ranger")
predictors = c("wq_predictors", "met_predictors", "all_predictors")
proportion = c(0.7, 0.8, 0.9) #will add more if useful
m_try = c(2, 3, 4)
ntree = c(100, 500, 1000)

## Assemble a table of  the initial models to be run
model_list <- tibble(expand.grid(data = datasets,
                                     model = model_packages, 
                                     predictors = predictors, 
                                     m_try = m_try, 
                                     ntree = ntree,
                                     proportion = proportion)) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  mutate(model_no = seq.int(nrow(.)))



