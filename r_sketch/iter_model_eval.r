fit_elastic_net <- function(em_train, cv_splits,
                         skip_features,
                         grid_size=30) {
  rval <- list(skip_features = skip_features)
  spec <- logistic_reg(penalty = tune(), mixture = tune()) %>% 
    set_engine('glmnet')
  
  # Hyperparameter grid
  logit_grid <- spec %>%
    parameters() %>%
    grid_latin_hypercube(size = grid_size)
  
  rec <- em_train %>% 
    recipe(sample_type ~ .) %>% 
    update_role(all_of(skip_features), new_role = 'ID')
  
  rval$wflow <- logit_wflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)

  rval$tuned_model <- tune_grid(rval$wflow,
                           resamples = cv_splits,
                           grid = logit_grid,
                           metrics = metric_set(mn_log_loss, pr_auc, gain_capture, roc_auc, sensitivity, specificity),
                           #control = control_resamples(verbose = TRUE)
                           )
  rval$best <- rval$tuned_model %>% 
    select_best(metric='mn_log_loss')
  rval$final <-  finalize_workflow(rval$wflow, rval$best) %>% 
    fit(em_train)
  return(rval)
}

elnet_least_important_term <- function(x) {
  x %>% 
    tidy() %>% filter(!str_detect(term, 'Intercept')) %>% 
    arrange(abs(estimate)) %>% head(1) %>% pull(term)
}

doParallel::registerDoParallel()

## FIT ALL MODELS
GRID_SIZE <- 30
skip_features <- c('donor', 'row_id')
cv_splits <- vfold_cv(em_train, v = 5, repeats = 3)
elnet_mix <- list()

for (i in (ncol(em_train) - length(skip_features)):2) {
  model_name <- str_glue('model_fsize_{i}')
  print(str_glue('Fitting {model_name} ...'))
  x <- fit_elastic_net(em_train, cv_splits, grid_size = GRID_SIZE, skip_features = skip_features) %>% 
    c(list(model_name = model_name))
  skip_features <- c(skip_features, elnet_least_important_term(x$final))
  elnet_mix[[model_name]] <- x
}

collect_best_features <- function(x) {
  x$tuned_model %>% 
    collect_metrics() %>% 
    filter(.config == x$best$.config) %>% 
    pivot_wider(id_cols = -everything(), names_from = .metric, values_from=mean)
}


tuned_model <- tune_grid(logit_wflow,
                         resamples = cv_splits,
                         grid = logit_grid,
                         metrics = metric_set(mn_log_loss, roc_auc, pr_auc, gain_capture),
                         control = control_resamples(verbose = TRUE))
(tuned_metrics = tuned_model %>% collect_metrics() %>% 
    pivot_wider(-std_err, names_from = .metric, values_from = mean) %>% 
    arrange(desc(roc_auc)))
logit_best <- tuned_model %>% 
  select_best(metric='roc_auc')

logit_final = finalize_workflow(logit_wflow, logit_best) %>% 
  fit(eqdf)
logit_final %>% 
  tidy() %>% 
  arrange(desc(abs(estimate))) %>% 
  view()

pred <- logit_final %>% 
  (function(f, test) {
    x <- predict(f, test, type = 'prob')
    y <- predict(f, test)
    bind_cols(select(test, donor, sample_type), x,y)
  })(test_out_of_sample)

(test_roc = pred %>% 
    roc_auc(truth = sample_type, .pred_tetpos))
(test_metrics = pred %>% 
    conf_mat(sample_type, .pred_class) %>% 
    summary())
z <- logit_final %>% 
  pull_workflow_prepped_recipe() %>% 
  bake(new_data=eqdf, all_predictors())