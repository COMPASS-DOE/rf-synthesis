## This script creates the plots used in the paper
##
## 2022-04-07
## Peter Regier
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(rsample,
       tidymodels, 
       tidyverse)

# Read in Constants
source("R_scripts/constants.R")


# 2. Create OOB v NRS plots ----------------------------------------------------

## First, read in data
metrics <- read_csv("data/created/model_metrics.csv")
cbv_data <- read_csv("data/created/cbv_for_models.csv")

## Set split color-scheme
split_colors = c("#F0A861", "#37518B")
dataset_colors = c("#94A9D8", "#B1CF94")

## Create splits
set.seed(42)
oob_split <- initial_split(cbv_data, prop = 0.7)
nrs_split <- initial_time_split(cbv_data, prop = 0.7)

## Create labeled time-series
oob_ts <- bind_rows(training(oob_split) %>% mutate(subset = "Train"), 
                    testing(oob_split) %>% mutate(subset = "Test"))
nrs_ts <- bind_rows(training(nrs_split) %>% mutate(subset = "Train"), 
                    testing(nrs_split) %>% mutate(subset = "Test"))

oob_ts_plot <- ggplot(oob_ts, aes(datetime_round, no3, color = subset)) + 
  geom_point() + 
  scale_color_manual(values = split_colors) +
  labs(x = "", y = "NO3 (mg/L)", color = "", title = "Random (OOB) split strategy")
  
nrs_ts_plot <- ggplot(nrs_ts, aes(datetime_round, no3, color = subset)) + 
  geom_point() + 
  scale_color_manual(values = split_colors) +
  labs(x = "", y = "NO3 (mg/L)", color = "", title = "Non-random (NRS) split strategy")

  
## Create plot comparing NSE values for NRS and OOB models
nrs_v_oob_plot <- ggplot(metrics, aes(dataset, nse, fill = split)) + 
  geom_boxplot() + 
  labs(x = "Dataset", 
       y = "NSE", 
       fill = "Split \n strategy") + 
  scale_fill_manual(values = dataset_colors) + 
  theme(legend.title = element_text(hjust = 0.5), 
        legend.position = c(0.3, 0.2), 
        legend.background = element_blank(), 
        legend.key = element_rect(colour = "transparent", fill = "white"))

plot_grid(plot_grid(oob_ts_plot, nrs_ts_plot, ncol = 1), 
          nrs_v_oob_plot, nrow = 1, rel_widths = c(1, 0.5))
ggsave("figures/A-OOB_v_NRS.pdf", width = 10, height = 4)


# 2. Create Parameter plots ----------------------------------------------------

myboxplot <- function(my_factor, my_title){
  
  colors = c("#292F36", "#84DCC6", "#2D82B7")
  ggplot(metrics, 
         aes(x = data, y = nse, fill = as.factor({{my_factor}}))) + 
    geom_boxplot(alpha = 0.9, color = "#0D0E23") + 
    labs(x = "", y = "NSE", title = my_title, fill = "") + 
    scale_x_discrete(labels = c("CBV", "OWC")) + 
    scale_fill_manual(values = colors) + 
    theme(legend.position = c(0.3, 0.2), 
          legend.background = element_blank(),
          legend.key = element_rect(fill="transparent"),
          plot.title = element_text(hjust = 0.5))
}

plot_grid(myboxplot(predictors, "Predictor variables"), 
          myboxplot(model, "Model package"), 
          myboxplot(proportion, "Train:test dataset ratio"), 
          myboxplot(m_try, "Variables per split"), 
          myboxplot(ntree, "No. of trees"), 
          nrow = 1)
ggsave("figures/B-metrics.pdf", width = 13, height = 4)


## Create Feature Importance plots ---------------------------------------------

## First, read in data
fi_data <- read_csv("data/created/model_feature_importance.csv")

fi_stats <- fi_data %>% filter(predictor_set == "wq_predictors") %>% 
  group_by(predictor, data) %>% 
  summarize(median_fi = median(fi), 
            min_fi = min(fi), 
            max_fi = max(fi))

## set up a color scheme for water quality
wq_vars <- unique(fi_stats$predictor)
wq_colors <- PNWColors::pnw_palette("Bay", n = length(wq_vars))

fi_colors = tibble(var = wq_vars, 
                   colors = wq_colors)


make_fi_plot <- function(data, title){
  ggplot(data, 
         aes(reorder(predictor, -median_fi, sum), fill = predictor)) + 
    geom_col(aes(y = median_fi), show.legend = F) + 
    geom_errorbar(aes(ymin = min_fi, ymax = max_fi), width = 0.2) + 
    labs(x = "Predictor", y = "Feature Importance", title = title) + 
    scale_x_discrete(limits=rev) + 
    coord_flip() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual(values = fi_colors$colors)
}

cbv_fi <- make_fi_plot(fi_stats %>% filter(data == "cbv_data"), "CBV")
owc_fi <- make_fi_plot(fi_stats %>% filter(data == "owc_data"), "OWC")
  
plot_grid(cbv_fi, owc_fi, nrow = 1)
ggsave("figures/C-feature_importance.pdf", width = 6, height = 4)


## Create PDP plots ------------------------------------------------------------

# pdp_df %>% 
#   filter(predictors == "wq_predictors") %>% 
#   mutate(x = (`_x_` * sd_x) + mean_x) %>% 
#   ggplot(aes(x, no3, group = model_no)) + 
#   geom_line() +
#   facet_wrap(~data, scales = "free") + 
#   labs(x = "Sp. Cond. (mS/cm)", "NO3 (mg/L)")

