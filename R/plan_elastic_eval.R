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

collect_best_metrics <- function(x) {
  x$tuned_model %>% 
    collect_metrics() %>% 
    filter(.config == x$best$.config) %>% 
    pivot_wider(id_cols = -everything(), names_from = .metric, values_from=mean) %>% 
    mutate(model_name = x$model_name)
}

iter_fit_best_enet <- function(em_eqdf, markers, panel_name='mixpanel', GRID_SIZE=30, v = 5, r =3) {
  panel_features <- c(filter(markers, panel == panel_name) %>% pull(protein), 'sample_type')
  skip_features <- setdiff(colnames(em_eqdf), panel_features) 
  cv_splits <- vfold_cv(em_eqdf, v = v, repeats = r)
  elnet_res <- list()
  for (i in length(panel_features):3) {
    model_name <- str_glue('model_fsize_{i}')
    print(str_glue('Fitting {model_name} ...'))
    x <- fit_elastic_net(em_eqdf, cv_splits, grid_size = GRID_SIZE, skip_features = skip_features) %>% 
      c(list(model_name = model_name))
    skip_features <- c(skip_features, elnet_least_important_term(x$final))
    elnet_res[[model_name]] <- x
  }
  return(elnet_res)
}

iter_fit_best_lasso <- function(em_eqdf, markers, panel_name='mixpanel', GRID_SIZE=40, v = 5, r =3) {
  panel_features <- c(filter(markers, panel == panel_name) %>% pull(protein), 'sample_type')
  skip_features <- setdiff(colnames(em_eqdf), panel_features) 
  cv_splits <- vfold_cv(em_eqdf, v = v, repeats = r)
  
  # x <- fit_elastic_net(em_eqdf, cv_splits, grid_size = GRID_SIZE, skip_features = skip_features) 
  
  spec <- logistic_reg(penalty = tune(), mixture = 1) %>% 
    set_engine('glmnet')
  
  # Hyperparameter grid
  logit_grid <- tibble(penalty = 0:GRID_SIZE/(GRID_SIZE+1))
  
  rec <- em_eqdf %>% 
    recipe(sample_type ~ .) %>% 
    update_role(all_of(skip_features), new_role = 'ID')
  
  rval <- list()
  rval$wflow <- logit_wflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)
  
  rval$tuned_model <- tune_grid(rval$wflow,
                                resamples = cv_splits,
                                grid = logit_grid,
                                metrics = metric_set(mn_log_loss, pr_auc, gain_capture, roc_auc, sensitivity, specificity),
                                control = control_resamples(save_workflow = T, save_pred = T)
  )
  rval$best <- rval$tuned_model %>% 
    select_best(metric='mn_log_loss')
  rval$final <-  finalize_workflow(rval$wflow, rval$best) %>% 
    fit(em_eqdf)
  return(rval)
}



plan_elastic_eval <-
  drake_plan(
    GRID_SIZE=40,
    v=8,
    r=1,
    lasso_res_mixpanel = target(iter_fit_best_lasso(em_eqdf, markers, panel_name='mixpanel', GRID_SIZE=GRID_SIZE, v = v, r =r), format='qs'),
    lasso_res_oldpanel = target(iter_fit_best_lasso(em_eqdf, markers, panel_name='oldpanel', GRID_SIZE=GRID_SIZE, v = v, r =r), format='qs'),
    lasso_res_newpanel = target(iter_fit_best_lasso(em_eqdf, markers, panel_name='newpanel', GRID_SIZE=GRID_SIZE, v = v, r =r), format='qs'),
    
    elastic_res_mixpanel = target(iter_fit_best_enet(em_eqdf, markers, panel_name='mixpanel', GRID_SIZE=GRID_SIZE, v = v, r =r), format='qs'),
    elastic_res_oldpanel = target(iter_fit_best_enet(em_eqdf, markers, panel_name='oldpanel', GRID_SIZE=GRID_SIZE, v = v, r =r), format='qs'),
    elastic_res_newpanel = target(iter_fit_best_enet(em_eqdf, markers, panel_name='newpanel', GRID_SIZE=GRID_SIZE, v = v, r =r), format='qs'),

    elastic_scores = bind_rows(new = elastic_res_newpanel %>%
                                 map_dfr(collect_best_metrics),
                               old = elastic_res_oldpanel %>%
                                 map_dfr(collect_best_metrics),
                               mixed = elastic_res_mixpanel %>%
                                 map_dfr(collect_best_metrics),
                               .id = 'panel') %>%
      mutate(n = as.numeric(str_extract(model_name, '\\d+$')),
        model_name = fct_reorder(model_name, n, min)),
    elastic_auc_plot = elastic_scores %>%
      ggplot(aes(n, pr_auc, color = panel)) +
      geom_line() + scale_y_continuous(limits = c(0.5,1)) +
      labs(title='Model performance by number of features and panel',
           y = 'Area under the precision recall curve',
           x = 'Number of features')
)
# 
