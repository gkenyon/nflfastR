nflpackages <- c('devtools', 'nflfastR', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools', 'tidymodels')
lapply(nflpackages, require, character.only = TRUE)

raw_plays <- read_rds(
  url("https://github.com/jthomasmock/nfl-workshop/blob/master/raw_plays.rds?raw=true"))
all_plays <- raw_plays %>%
  group_by(game_id, posteam) %>%
  mutate(
    run = if_else(play_type == "run", 1, 0),
    pass = if_else(play_type == "pass", 1, 0),
    total_runs = if_else(play_type == "run", cumsum(run) - 1, cumsum(run)),
    total_pass = if_else(play_type == "pass", cumsum(pass) - 1, cumsum(pass)),
    previous_play = if_else(posteam == lag(posteam),
                            lag(play_type), "First play of Drive"
    ),
    previous_play = if_else(is.na(previous_play),
                            replace_na("First play of Drive"), previous_play
    )
  ) %>%
  ungroup() %>%
  mutate_at(vars(
    play_type, shotgun, no_huddle,
    posteam_timeouts_remaining, defteam_timeouts_remaining,
    previous_play, goal_to_go
  ), as.factor) %>%
  mutate(
    down = factor(down, levels = c(1, 2, 3), ordered = TRUE),
    qtr = factor(qtr, levels = c(1, 2, 3, 4), ordered = TRUE),
    in_red_zone = if_else(yardline_100 <= 20, 1, 0),
    in_fg_range = if_else(yardline_100 <= 35, 1, 0),
    two_min_drill = if_else(half_seconds_remaining <= 120, 1, 0)
  ) %>%
  mutate(
    in_red_zone = factor(if_else(yardline_100 <= 20, 1, 0)),
    in_fg_range = factor(if_else(yardline_100 <= 35, 1, 0)),
    two_min_drill = factor(if_else(half_seconds_remaining <= 120, 1, 0))
  ) %>%
  select(-run, -pass)

# #Pre-Process & choose a model
# model_recipe <- recipe(pred ~ predictors, data = train_data)
#
# # Choose a model and an engine
# lr_mod <- logistic_reg(mode = "classification") %>%
#   set_engine("glm")
#
# # Combine the model and recipe to the workflow
# lr_wflow <- workflow() %>%
#   add_recipe(model_recipe) %>%
#   add_model(lr_mod)
#
# # Fit/train the model
# model_fit <- lr_wflow %>%
#   fit(data = train_data)
#
# # Get predictions
# pred_lr <- predict(pbp_fit_lr, test_data)
#
# # Check metrics
# pred_lr %>%
#   metrics(truth = pred, .pred_class) %>%
#   bind_cols(select(test_data, pred)) %>%
#   bind_cols(predict(fit_lr, test_data, type = "prob"))

# Split
split_pbp <- initial_split(all_plays, 0.75, strata = play_type)

# Split into test/train
train_data <- training(split_pbp)
test_data <- testing(split_pbp)

#Pre-Process & Choose a model
pbp_rec <- recipe(play_type ~ ., data = train_data)  %>%
  step_rm(half_seconds_remaining) %>% # remove
  step_string2factor(posteam, defteam) %>%  # convert to factors
  update_role(yards_gained, game_id, new_role = "ID") %>%  # add as ID
  step_corr(all_numeric(), threshold = 0.7) %>% # remove auto-correlated
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors())  # remove zero-variance predictors

# Choose a model and an engine
lr_mod <- logistic_reg(mode = "classification") %>%
  set_engine("glm")

#Combine into a workflow

# Combine the model and recipe to the workflow
lr_wflow <- workflow() %>%
  add_recipe(pbp_rec) %>%
  add_model(lr_mod)

# Fit/train the model
pbp_fit_lr <- lr_wflow %>%
  fit(data = train_data)

#Predict and get metrics
# Get predictions
pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>%
  bind_cols(test_data %>% select(play_type)) %>%
  bind_cols(predict(pbp_fit_lr, test_data, type = "prob"))
# Check metrics
pbp_pred_lr %>%
  metrics(truth = play_type, .pred_class)

## ---- fig.dim = c(5,5)-----------------------------------------------------------------------------------------------------------------------------
pbp_pred_lr %>%
  # calculate ROC curve
  roc_curve(truth = play_type, .pred_pass) %>%
  autoplot()


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Split
## split_pbp <- initial_split(all_plays, 0.75, strata = play_type)
##
## # Split into test/train
## train_data <- training(split_pbp)
## test_data <- testing(split_pbp)


## ---- eval =FALSE----------------------------------------------------------------------------------------------------------------------------------
## pbp_rec <- recipe(play_type ~ ., data = train_data)  %>%
##   step_rm(half_seconds_remaining) %>% # remove
##   step_string2factor(posteam, defteam) %>%  # convert to factors
##   update_role(yards_gained, game_id, new_role = "ID") %>%  # add as ID
##   step_corr(all_numeric(), threshold = 0.7) %>% # remove auto-correlated
##   step_center(all_numeric()) %>%  # substract mean from numeric
##   step_zv(all_predictors())  # remove zero-variance predictors
##
## # Choose a model and an engine
## lr_mod <- logistic_reg(mode = "classification") %>%
##   set_engine("glm")


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Combine the model and recipe to the workflow
## lr_wflow <- workflow() %>%
##   add_recipe(pbp_rec) %>%
##   add_model(lr_mod)
##
## # Fit/train the model
## pbp_fit_lr <- lr_wflow %>%
##   fit(data = train_data)


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Get predictions
## pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>%
##   bind_cols(test_data %>% select(play_type)) %>%
##   bind_cols(predict(pbp_fit_lr, test_data, type = "prob"))
##
## # Check metrics
## pbp_pred_lr %>%
##   metrics(truth = play_type, .pred_class)


## --------------------------------------------------------------------------------------------------------------------------------------------------
rf_mod <- rand_forest(trees = 100) %>%
  set_engine("ranger",
             importance = "impurity", # variable importance
             num.threads = 4) %>%     # Parallelize
  set_mode("classification")

rf_wflow <- workflow() %>%
  add_recipe(pbp_rec) %>% # Same recipe
  add_model(rf_mod)     # New model


pbp_fit_rf <- rf_wflow %>% # New workflow
  fit(data = train_data)   # Fit the Random Forest

# Get predictions and check metrics
pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>%
  bind_cols(test_data %>% select(play_type)) %>%
  bind_cols(predict(pbp_fit_rf, test_data, type = "prob"))


## ----compare_metrics call--------------------------------------------------------------------------------------------------------------------------
pbp_pred_rf %>% # Random Forest predictions
  metrics(truth = play_type, .pred_class)

pbp_pred_lr %>% # Logistic Regression predictions
  metrics(truth = play_type, .pred_class)


## ---- fig.dim=c(10,6)------------------------------------------------------------------------------------------------------------------------------
pbp_fit_rf %>%
  pull_workflow_fit() %>%
  vip(num_features = 20)


## --------------------------------------------------------------------------------------------------------------------------------------------------
roc_rf <- pbp_pred_rf %>%
  roc_curve(truth = play_type, .pred_pass) %>%
  mutate(model = "Ranger")

roc_lr <- pbp_pred_lr %>%
  roc_curve(truth = play_type, .pred_pass) %>%
  mutate(model = "Logistic Regression")

full_plot <- bind_rows(roc_rf, roc_lr) %>%
  # Note that autoplot() works here!
  ggplot(aes(x = 1 - specificity,
             y = sensitivity,
             color = model)) +
  geom_path(lwd = 1, alpha = 0.5) +
  geom_abline(lty = 3) +
  scale_color_manual(
    values = c("#374785", "#E98074")
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())




## ---- fig.dim=c(6,6)-------------------------------------------------------------------------------------------------------------------------------
full_plot


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------
calibration_plot <- pbp_pred_rf %>%
  mutate(
    pass = if_else(play_type == "pass", 1, 0),
    pred_rnd = round(.pred_pass, 1)
  ) %>%
  group_by(pred_rnd) %>%
  summarize(
    mean_pred = mean(.pred_pass),
    mean_obs = mean(pass),
    n = n()
  ) %>%
  ggplot(aes(x = mean_pred, y = mean_obs)) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(size = n)) +
  theme_minimal() +
  labs(
    x = "Predicted Pass",
    y = "Observed Pass"
  ) +
  coord_cartesian(
    xlim = c(0,1), ylim = c(0, 1)
  )


## ---- echo = FALSE, fig.dim=c(7,7)-----------------------------------------------------------------------------------------------------------------
calibration_plot


## ---- echo = FALSE, out.width="45%"----------------------------------------------------------------------------------------------------------------
knitr::include_graphics("images/resample-pic.png")


## --------------------------------------------------------------------------------------------------------------------------------------------------
vfold_cv(train_data, v = 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------
vfold_cv(train_data, v = 10, repeats = 5)


## ---- eval = FALSE, message=FALSE------------------------------------------------------------------------------------------------------------------
## set.seed(20201024)
## # Create 10 folds and 5 repeats
## pbp_folds <- vfold_cv(train_data, v = 10, repeats = 5)
##
## pbp_folds


## ---- echo = FALSE, message=FALSE------------------------------------------------------------------------------------------------------------------
pbp_folds <- read_rds("pbp_folds.rds")
pbp_folds


## ---- eval = FALSE, message=FALSE------------------------------------------------------------------------------------------------------------------
## keep_pred <- control_resamples(save_pred = TRUE, verbose = TRUE)
## set.seed(20201024)
## # Fit resamples
## rf_res <- fit_resamples(rf_wflow, resamples = pbp_folds, control = keep_pred)
##
## rf_res


## ---- echo = FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------
rf_res <- read_rds("rf_res.rds")
rf_res


## --------------------------------------------------------------------------------------------------------------------------------------------------
# Naive Model on Testing Data
rf_compare_df <- bind_rows(
  accuracy(pbp_pred_rf,
           truth = play_type, .pred_class),
  roc_auc(pbp_pred_rf,
          truth = play_type, .pred_pass)
)


## ---- fig.dim = c(4,4)-----------------------------------------------------------------------------------------------------------------------------
combo_plot <- rf_res %>%
  collect_metrics(summarize = FALSE) %>%
  ggplot(aes(x = .metric, y = .estimate)) +
  geom_jitter(width = 0.2) +
  geom_boxplot(width = 0.3, alpha = 0.5) +
  geom_point(
    data = rf_compare_df, #
    color = "red",
    size = 3
  )


## ---- fig.dim=c(6,6), echo = FALSE-----------------------------------------------------------------------------------------------------------------
combo_plot


## --------------------------------------------------------------------------------------------------------------------------------------------------
assess_res <- collect_predictions(rf_res)

assess_res


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------
res_calib_plot <- assess_res %>%
  mutate(
    pass = if_else(play_type == "pass", 1, 0),
    pred_rnd = round(.pred_pass, 2)
  ) %>%
  group_by(pred_rnd) %>%
  summarize(
    mean_pred = mean(.pred_pass),
    mean_obs = mean(pass),
    n = n()
  ) %>%
  ggplot(aes(x = mean_pred, y = mean_obs)) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(size = n), alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Predicted Pass",
    y = "Observed Pass"
  ) +
  coord_cartesian(
    xlim = c(0,1), ylim = c(0, 1)
  )


## ---- echo = FALSE, fig.dim=c(7,7)-----------------------------------------------------------------------------------------------------------------
res_calib_plot


## ---- echo = FALSE, out.width="25%"----------------------------------------------------------------------------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/tune.svg")


## --------------------------------------------------------------------------------------------------------------------------------------------------
tune_pbp_rf <- rand_forest(
  mtry = tune(), # add placeholder for tune
  trees = 100,
  min_n = tune() # add placeholder for tune
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_rf_wf <- workflow() %>%
  add_recipe(pbp_rec) %>%
  add_model(tune_pbp_rf)


## --------------------------------------------------------------------------------------------------------------------------------------------------
tune_rf_wf


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## set.seed(37)
##
## pbp_folds <- vfold_cv(train_data, v = 5)
##
## tic()
## tune_res <- tune_grid(
##   tune_rf_wf,
##   resamples = pbp_folds,
##   grid = 15, # 15 combos of model parameters
##   control = control_grid(verbose = TRUE)
## )
## toc()
## # 1478.385 sec elapsed


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------
tune_res <- read_rds("tuned_rf.rds")


## --------------------------------------------------------------------------------------------------------------------------------------------------
tune_res


## --------------------------------------------------------------------------------------------------------------------------------------------------
# Essentially the same as tune_res[[".metrics"]][[1]]
tune_res %>%
  pluck(".metrics", 3)




## ---- fig.dim = c(8,10)----------------------------------------------------------------------------------------------------------------------------
plot_tuned <- tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  dplyr::select(mean, mtry:min_n) %>%
  pivot_longer(mtry:min_n,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x", ncol = 1) +
  labs(x = NULL, y = "AUC")


## ---- fig.dim=c(4,6), echo = FALSE-----------------------------------------------------------------------------------------------------------------
plot_tuned


## ---- fig.dim = c(4,4)-----------------------------------------------------------------------------------------------------------------------------
plot_tuned <- tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  dplyr::select(mean, mtry:min_n) %>%
  pivot_longer(mtry:min_n,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x", ncol = 1) +
  labs(x = NULL, y = "AUC")


## ---- fig.dim=c(4,6)-------------------------------------------------------------------------------------------------------------------------------
plot_tuned +
  scale_y_continuous(limits = c(0.75, 0.85))


## --------------------------------------------------------------------------------------------------------------------------------------------------
# Which 5x were best?
show_best(tune_res, "roc_auc", n = 5)

# Select the best
best_fit_auc <- select_best(tune_res, "roc_auc")

# Select wflow for the model with best hyperparams
rf_tuned <- finalize_workflow(
  rf_wflow,
  parameters = best_fit_auc
)


## --------------------------------------------------------------------------------------------------------------------------------------------------
rf_tuned_fit <- last_fit(rf_tuned, split_pbp)

rf_tuned_fit %>%  # tuned model metrics
  collect_metrics()


## --------------------------------------------------------------------------------------------------------------------------------------------------
rf_compare_df # naive model metrics
