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
       tidyverse,
       janitor,
       ggpubr, 
       cowplot, 
       kableExtra)

# Read in Constants
source("R_scripts/constants.R")


# 2. Create Figure 1 (NRS v OOB) -----------------------------------------------

## First, read in data
metrics <- read_csv("data/created/model_metrics.csv")
cbv_data <- read_csv("data/created/cbv_for_models.csv")
owc_data <- read_csv("data/created/owc_for_models.csv")

## Set split color-scheme
split_colors = c("#F0A861", "#7F10B8")
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

plot_grid(plot_grid(oob_ts_plot, nrs_ts_plot, ncol = 1, labels = c("a", "b")), 
          nrs_v_oob_plot, nrow = 1, rel_widths = c(1, 0.5), labels = c("", "c"))
ggsave("figures/1-OOB_v_NRS.pdf", width = 10, height = 4)


# 3. Create Figure 3 (parameter boxplots) --------------------------------------

myboxplot <- function(my_factor, var, ylab, my_title){
  
  colors = c("#292F36", "#84DCC6", "#2D82B7")
  ggplot(metrics, 
         aes(x = data, y = {{var}}, fill = as.factor({{my_factor}}))) + 
    geom_boxplot(alpha = 0.9, color = "#0D0E23") + 
    labs(x = "", y = ylab, title = my_title, fill = "") + 
    scale_x_discrete(labels = c("CBV", "OWC")) + 
    scale_fill_manual(values = colors) + 
    stat_compare_means(aes(label = ..p.signif..)) +
    theme(legend.position = c(0.3, 0.2), 
          legend.background = element_blank(),
          legend.key = element_rect(fill="transparent"),
          plot.title = element_text(hjust = 0.5))
}

plot_grid(myboxplot(predictors, nse, "NSE", "Predictor variables"), 
          myboxplot(model, nse, "NSE", "Model package"), 
          myboxplot(proportion, nse, "NSE", "Train:test dataset ratio"), 
          myboxplot(m_try, nse, "NSE", "Variables per split"), 
          myboxplot(ntree, nse, "NSE", "No. of trees"), 
          nrow = 1, labels = c("a", "b", "c", "d", "e"))
ggsave("figures/3-metrics.pdf", width = 13, height = 4)


# 4. Create Figure 4 -------------------------------------------------------------

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

fi_stats_renamed <- fi_stats %>% 
  mutate(predictor = case_when(predictor == "Depth" ~ "Depth", 
                               predictor == "DO_mgl" ~ "DO", 
                               predictor == "pH" ~ "pH", 
                               predictor == "site" ~ "Station", 
                               predictor == "SpCond" ~ "SpCond", 
                               predictor == "Temp" ~ "Temp", 
                               predictor == "Turb" ~ "Turbidity", 
                               predictor == "sin_doy" ~ "DOY"))


make_fi_plot <- function(data, title){
  ggplot(data, 
         aes(reorder(predictor, -median_fi, sum), fill = predictor)) + 
    geom_col(aes(y = median_fi), show.legend = F) + 
    geom_errorbar(aes(ymin = min_fi, ymax = max_fi), width = 0.2) + 
    labs(x = "Predictor", y = "Predictor Importance", title = title) + 
    scale_x_discrete(limits=rev) + 
    coord_flip() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual(values = fi_colors$colors)
}

cbv_fi <- make_fi_plot(fi_stats_renamed %>% filter(data == "cbv_data"), "CBV dataset")
owc_fi <- make_fi_plot(fi_stats_renamed %>% filter(data == "owc_data"), "OWC dataset")

plot_grid(cbv_fi, owc_fi, nrow = 1, labels = c("a", "b"))
ggsave("figures/4-feature_importance.pdf", width = 6.5, height = 4)


# 5. Create Figure 5 (PDPs) ----------------------------------------------------

## Read in pdp data
pdp <- read_csv("data/created/pdp_data.csv") %>% 
  mutate(data_fct = as.factor(data))

pdp$data_fct <-  forcats::fct_recode(pdp$data_fct, "CBV" = "cbv_data", "OWC" = "owc_data")

plot_pdp <- function(var, xlab){
  
  pdp2 <- pdp %>% 
    filter(predictor == var)
  
  mean_pdp = pdp2 %>% 
    filter(predictor == var) %>% 
    group_by(x, data_fct) %>% 
    summarize(dep = mean(dep))
  
  ggplot() + 
    geom_line(data = pdp2, aes(x, dep, group = model_no), color = "gray") +
      geom_smooth(data = mean_pdp, aes(x, dep), color = "black", se = F) +
    facet_wrap(data_fct ~ ., scales = "free", ncol = 1) + 
    labs(x = xlab, y = "", color = "mtry")
}

plot_grid(plot_pdp("SpCond", "SpCond") + ylab("Nitrate (mg/L)"), 
          plot_pdp("sin_doy", "DOY"), 
          plot_pdp("Temp", "Temp. (C)"), 
          nrow = 1, labels = c("a", "b", "c"))
ggsave("figures/5-pdps.pdf", width = 9, height = 5)


# 6. Figure 6 ------------------------------------------------------------------

var_comparison <- read_csv("data/created/variable_combo_model_metrics.csv") %>% 
  mutate(includes_spcond = str_detect(predictors, pattern = "SpCond"), 
         includes_turb = str_detect(predictors, pattern = "Turb")) 


cbv_var_comparison_plot <- var_comparison %>% 
  filter(data == "cbv_data") %>% 
  ggplot(aes(as.factor(n_predictors), nse, fill = includes_spcond)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("gray", wq_colors[6])) + 
  labs(x = "Number of predictors", y = "NSE", fill = "Includes \n SpCond") + 
  theme(legend.position = c(0.8, 0.3), legend.background = element_blank())

owc_var_comparison_plot <- var_comparison %>% 
  filter(data == "owc_data") %>% 
  ggplot(aes(as.factor(n_predictors), nse, fill = includes_turb)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("gray", wq_colors[4])) + 
  labs(x = "Number of predictors", y = "NSE", fill = "Includes \n Turbidity") + 
  theme(legend.position = c(0.8, 0.3), legend.background = element_blank())

plot_grid(cbv_var_comparison_plot, 
          owc_var_comparison_plot, 
          nrow = 1, labels = c("a", "b"))
ggsave("figures/6-variable_combo_metrics.pdf", width = 8, height = 4)


# 7. Create Table 1 (model parameters) -----------------------------------------

table1 <- tibble(Parameter = c("Predictor variables", 
                               "Model package", 
                               "Training:testing ratio", 
                               "Variables per split (mtry)", 
                               "Number of trees (ntree)"), 
                 Levels = c("water quality, meteorology, both", 
                            "randomForest, ranger", 
                            "0.7, 0.8, 0.9", 
                            "2, 3, 4", 
                            "100, 500, 1000"), 
                 Rationale = c("Random forests only use a subset of predictors for each tree", 
                               "Different packages use different implementations and default parameters", 
                               "The ratio can influence over-fitting or under-fitting", 
                               "The number of variables for each split influence what data a tree receives", 
                               "Represents a trade-off between model performance and computational time"))

table1 %>% kbl(caption = "Table 1: Parameters manipulated", align = 'c') %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  column_spec(1, width = "10em") %>% 
  column_spec(2, width = "10em") %>% 
  column_spec(3, width = "15em") %>% 
  row_spec(0, bold = TRUE) %>% 
  save_kable("figures/Table1-parameters.pdf")


# 8. Create Figure S1 ----------------------------------------------------------

## Create nitrate by month
plot_grid(cbv_data %>% 
            mutate(month = lubridate::month(datetime_round)) %>% 
            ggplot(aes(as.factor(month), no3)) + 
            geom_boxplot() + 
            labs(x = "Month", y = "Nitrate (mg/L)", title = "CBV dataset"), 
          owc_data %>% 
            mutate(month = lubridate::month(datetime_round)) %>% 
            ggplot(aes(as.factor(month), no3)) + 
            geom_boxplot() + 
            labs(x = "Month", y = "Nitrate (mg/L)", title = "OWC dataset"), 
          ncol = 1)
ggsave("figures/S1-nitrate_by_month.pdf", width = 4, height = 5)


# 9. Create Figure S2 ----------------------------------------------------------

## Create an analogous series of plots to Figure 3, but for MAE
plot_mae <- plot_grid(myboxplot_p(predictors, mae, "MAE", "Predictor variables") +
                         theme(legend.position = "none"), 
                       myboxplot_p(model, mae, "MAE", "Model package") +
                         theme(legend.position = "none"), 
                       myboxplot_p(proportion, mae, "MAE", "Train:test dataset ratio") +
                         theme(legend.position = "none"), 
                       myboxplot_p(m_try, mae, "MAE", "Variables per split") +
                         theme(legend.position = "none"), 
                       myboxplot_p(ntree, mae, "MAE", "No. of trees") +
                         theme(legend.position = "none"), 
                       nrow = 1)

## Create an analogous series of plots to Figure 3, but for RMSE
plot_rmse <- plot_grid(myboxplot_p(predictors, rmse, "RMSE", "Predictor variables") +
                        theme(legend.position = c(0.3, 0.3)), 
                       myboxplot_p(model, rmse, "RMSE", "Model package") +
                        theme(legend.position = c(0.3, 0.3)), 
                       myboxplot_p(proportion, rmse, "RMSE", "Train:test dataset ratio") +
                        theme(legend.position = c(0.3, 0.3)), 
                       myboxplot_p(m_try, rmse, "RMSE", "Variables per split") +
                        theme(legend.position = c(0.3, 0.3)), 
                       myboxplot_p(ntree, rmse, "RMSE", "No. of trees") +
                        theme(legend.position = c(0.3, 0.3)), 
                       nrow = 1)

## Create an analogous series of plots to Figure 3, but for R2
plot_r2 <- plot_grid(myboxplot_p(predictors, r2, "R2", "Predictor variables") +
                       theme(legend.position = "none"), 
                      myboxplot_p(model, r2, "R2", "Model package") +
                       theme(legend.position = "none"), 
                      myboxplot_p(proportion, r2, "R2", "Train:test dataset ratio") +
                       theme(legend.position = "none"), 
                      myboxplot_p(m_try, r2, "R2", "Variables per split") +
                       theme(legend.position = "none"), 
                      myboxplot_p(ntree, r2, "R2", "No. of trees") +
                       theme(legend.position = "none"), 
                      nrow = 1)

## Create Figure S2 and export
plot_grid(plot_r2, plot_mae, plot_rmse, ncol = 1)
ggsave("figures/S2-all_metrics.pdf", width = 13, height = 12)


my_comparisons = list(c("all_predictors", "met_predictors"), 
                c("all_predictors", "wq_predictors"), 
                c("wq_predictors", "met_predictors"))

myboxplot(ntree, "Predictor variables") + 
  stat_compare_means(aes(label = ..p.signif..))


# 10. Create Table S1 ----------------------------------------------------------

table_s1 <- metrics %>% 
  group_by(dataset, split) %>% 
  summarize(across(c(rmse, mae, r2, nse), median)) %>% 
  mutate(across()) %>% 
  mutate(stat = "median") %>% 
    clean_names(case = "title") %>% 
  rename("RMSE" = Rmse, 
         "MAE" = Mae, 
         "NSE" = Nse) %>% 
  mutate_at(3:6, round, 2)


table_s1 %>% kbl(caption = "Table S1: Model metric statistics by split type", align = 'c') %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable("figures/TableS1-metrics.pdf")



