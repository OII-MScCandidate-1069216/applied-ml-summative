# Author: MSc candidate 
# Maintainers: MSc candidate 
# ===========================================================
# 


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, tidymodels, here, haven)



# f1h3 race 
# m1e4a financial support 
# cf1hhinc median income 
# f2j12 depression 
### m3h1e1 year of high school completed HACER BIEN CASE WHEN


files <- list(lm_plot = here("models", "output", "linear_mod_plot."),
              lm_plot_single = here("models", "output", "linear_plot_single."))

devices <- c("png", "svg")


# Functions  --------------------------------------------------------------
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}


# data --------------------------------------------------------------------
train_labels <- read_csv(here("import/input/train.csv")) %>% 
  select(challengeID, materialHardship)


background <- read_csv(here("import/input/background.csv")) %>% 
  select(challengeID, f1h3, m1e4a, cf1hhinc, f2j12, m3h1e1) %>% 
  mutate(f1h3 = 
           case_when(
             f1h3 %in% c(-9, -3, -2, -1) ~ NA,
             T ~ f1h3),
         f1h3 = as.factor(f1h3),
         m1e4a = 
           case_when(
             m1e4a %in% c(-9, -3, -2) ~ NA,  
             T ~ m1e4a), 
         m1e4a = as.factor(m1e4a),
         f2j12 = 
           case_when(
             f2j12 %in% c(-9, -1, -11) ~ NA,
             T ~ f2j12),
         f2j12 = as.factor(f2j12),
         cf1hhinc = case_when(
           cf1hhinc == -9 ~ NA,
           T ~ cf1hhinc),
         m3h1e1 = case_when(
           m3h1e1 %in% c(-9, -6, -2) ~ NA,
           T ~ m3h1e1
         ),
         across(everything(), ~replace_na(.x, calc_mode(.x))),
         cf1hhinc = replace_na(cf1hhinc, mean(cf1hhinc, na.rm = T)),
         m3h1e1 = replace_na( m3h1e1, mean(m3h1e1, na.rm = T))
  )

background <-left_join(background, train_labels, by = c("challengeID")) %>% 
  na.omit() %>% 
  select(-challengeID)



# Split data --------------------------------------------------------------
set.seed(1234)
families_split <- background %>%
  initial_split(strata = materialHardship)

families_train <- training(families_split)
families_test <- testing(families_split)



# Train linear model ------------------------------------------------------
lm_spec <- linear_reg() %>%
  set_engine(engine = "lm")


lm_fit <- lm_spec %>%
  fit(materialHardship ~ .,
      data = families_train
  )


lm_fit <- lm_spec %>%
  fit(materialHardship ~ .,
      data = families_train
  )


lm_fit_tidy <- tidy(lm_fit)

write_rds(lm_fit_tidy, here("models", "output", "lm_fit.rds"))


# Evaluate ----------------------------------------------------------------
results_train <- lm_fit %>%
  predict(new_data = families_train) %>%
  mutate(
    truth = families_train$materialHardship,
    model = "lm"
  )



results_test <- lm_fit %>%
  predict(new_data = families_test) %>%
  mutate(
    truth = families_test$materialHardship,
    model = "lm"
  )


results_train %>%
  metrics(truth = truth, estimate = .pred)


metrics_lm <- results_test %>%
  metrics(truth = truth, estimate = .pred)


write_rds(metrics_lm, here("models", "output", "metrics_lm.rds"))

### plot results on train and test 
results_test %>%
  mutate(train = "testing") %>%
  bind_rows(results_train %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "black", size = 1.5) +
  geom_point(alpha = 0.5, colour = "darkRed") +
  facet_wrap(~ train) +
  theme_minimal() +
  labs(
    x = "Truth",
    y = "Predicted materialHardship",
    title = "Predicted results for baseline model"
  ) +
  theme_minimal(base_family = "Courier New") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(face = "bold",  size=10.5),
        axis.text.x = element_text(face = "bold", size=10.5),
        text = element_text(size = 12)) 



walk(devices, ~ ggsave(filename = file.path(paste0(files$lm_plot, .x)),
                       device = .x, width = 8, height = 6))


### plot on test 
results_test %>%
  mutate(train = "testing") %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "black", size = 1.5) +
  geom_point(alpha = 0.5, colour = "darkRed") +
  facet_wrap(~ train) +
  theme_minimal() +
  labs(
    x = "Truth",
    y = "Predicted materialHardship",
    title = "Predicted results for baseline model"
  ) +
  theme_minimal(base_family = "Courier New") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(face = "bold",  size=10.5),
        axis.text.x = element_text(face = "bold", size=10.5),
        text = element_text(size = 12)) 

walk(devices, ~ ggsave(filename = file.path(paste0(files$lm_plot_single, .x)),
                       device = .x, width = 8, height = 6))


# Done. 

