# Author: MSc candidate 
# Maintainers: MSc candidate 
# ===========================================================
# 


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, tidymodels, here, haven, Amelia, beepr, 
               finetune, vip)




# output Files -------------------------------------------------------------------
files <- list(anova_plot = here("models", "output", "anova_plot."),
              var_importance = here("models", "output", "var_importance."),
              pred_real = here("models", "output", "pred_real_xg."))

devices <- c("png", "svg")


# Data  -------------------------------------------------------------------
# final_d <- read_rds(here("impute/output/imputed.rds"))

# set.seed
set.seed(2018)

# impute the left hand data, using argument "ords"
# to indicate the ordinal (binary) nature of certain variables
# test <- read_csv(here("import/input/train.csv")) 

#a.out.more_outcomes <- amelia(test, m = 5, idvars = "challengeID",
 #                             ords = c("eviction", "layoff", "jobTraining"))
#beep(sound = 8, expr = NULL)

# outcome <- a.out.more_outcomes$imputations$imp3

# final_train <- na.omit(merge(outcome, final_d, by = "challengeID", all.x = T)) %>% 
 # select(-gpa, -grit, -eviction, -layoff, -jobTraining)


# rm(final_d, outcome, test, a.out.more_outcomes)

# write_rds(final_train, here("models", "output", "final_train.rds"))
  
final_train <- read_rds(here("models", "output", "final_train.rds"))

# Split data  -------------------------------------------------------------
set.seed(123)
families_split <- initial_split(final_train, strata = materialHardship)
families_train <- training(families_split)
families_test <- testing(families_split)


set.seed(234)
families_folds <- vfold_cv(families_train, strata = materialHardship)
families_folds


# Prepare model  ----------------------------------------------------------
families_rec <-
  recipe(materialHardship ~ ., data = families_train) %>%
  update_role(challengeID, new_role = "id") 


families_prep <- prep(families_rec)
bake(families_prep, new_data = NULL) %>% str()


xgb_spec <-
  boost_tree(
    trees = tune(),
    mtry = tune(),
    min_n = tune(),
    learn_rate = 0.01
  ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_wf <- workflow(families_rec, xgb_spec)
xgb_wf

### fit 
doParallel::registerDoParallel()

set.seed(234)
xgb_game_rs <-
  tune_race_anova(
    xgb_wf,
    families_folds,
    grid = 20,
    control = control_race(verbose_elim = TRUE)
  )

xgb_game_rs
beep(sound = 8, expr = NULL)



# Evaluate  ---------------------------------------------------------------
plot_race(xgb_game_rs) +
  labs(title = "Grid search with Racing Anova for Xgboost Model") +
  theme_minimal(base_family = "Courier New") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(face = "bold",  size=10.5),
        axis.text.x = element_text(face = "bold", size=10.5),
        text = element_text(size = 12)) 

walk(devices, ~ ggsave(filename = file.path(paste0(files$anova_plot, .x)),
                       device = .x, width = 8, height = 6))

best_hyper <- show_best(xgb_game_rs)
write_rds(best_hyper, here("models", "output", "best_hyper.rds"))

# mtry trees min_n learn_rate .metric .estimator  mean     n
# 1     1571    40    0.01      rmse    standard   0.135    10

### last fit 
xgb_last <-
  xgb_wf %>%
  finalize_workflow(select_best(xgb_game_rs, "rmse")) %>%
  last_fit(families_split)


metrics_df_xg <- xgb_last %>% 
  collect_metrics()

write_rds(metrics_df_xg, here("models", "output", "metrics_df_xg.rds"))


xgb_fit <- extract_fit_parsnip(xgb_last)
vip(xgb_fit, geom = "point", num_features = 10, aesthetics = list(color = "darkGreen", size = 3)) +
  theme_minimal() +
  labs(title = "Variable importance for Xgboost Model",
       subtitle = "Top 10") +
  theme_minimal(base_family = "Courier New") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(face = "bold",  size=10.5),
        axis.text.x = element_text(face = "bold", size=10.5),
        text = element_text(size = 12)) 


walk(devices, ~ ggsave(filename = file.path(paste0(files$var_importance, .x)),
                       device = .x, width = 8, height = 6))


xgb_last %>% 
  collect_predictions() %>% 
  ggplot(aes(materialHardship, .pred)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5, colour = "darkGreen") +
  theme_minimal() +
  labs(
    x = "Truth",
    y = "Predicted materialHardship",
    title = "Predicted results for Xgboost model"
  ) +
  theme_minimal(base_family = "Courier New") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(face = "bold",  size=10.5),
        axis.text.x = element_text(face = "bold", size=10.5),
        text = element_text(size = 12)) 

walk(devices, ~ ggsave(filename = file.path(paste0(files$pred_real, .x)),
                       device = .x, width = 8, height = 6))

# DONE.