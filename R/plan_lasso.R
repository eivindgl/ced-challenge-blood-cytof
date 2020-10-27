iter_fit_best_lasso <- function(em_eqdf, markers, panel_name='mixpanel', grid_size=30, v = 10, r =1, opt_skip=character(0)) {
  panel_features <- c(filter(markers, panel == panel_name) %>% pull(protein), 'sample_type')
  
  skip_features <- unique(c(setdiff(colnames(em_eqdf), panel_features),
                            intersect(opt_skip, colnames(em_eqdf))))
  cv_splits <- vfold_cv(em_eqdf, v = v, repeats = r)
  
  # x <- fit_elastic_net(em_eqdf, cv_splits, grid_size = GRID_SIZE, skip_features = skip_features) 
  
  spec <- logistic_reg(penalty = tune(), mixture = 1) %>% 
    set_engine('glmnet')
  
  # Hyperparameter grid
  logit_grid <- spec %>%
    parameters() %>%
    grid_latin_hypercube(size = grid_size)
  
  # logit_grid <- tibble(penalty = 0:GRID_SIZE/(2*(GRID_SIZE+1)))
  
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
                                # control = control_resamples(save_workflow = F, save_pred = T)
  )
  rval$best <- rval$tuned_model %>% 
    select_best(metric='mn_log_loss')
  rval$final <-  finalize_workflow(rval$wflow, rval$best) %>% 
    fit(em_eqdf)
  return(rval)
}

get_fit_coeff <- function(wflow, em_eqdf, penalty) {
  wflow %>% 
    update_model(logistic_reg(penalty = penalty, mixture = 1) %>% 
                   set_engine('glmnet')) %>% 
    fit(em_eqdf) %>% 
    tidy() %>% 
    arrange(desc(abs(estimate))) %>% 
    filter(estimate != 0) %>% 
    select(-penalty)
}

# the problem with lasso is that the predictor becomes useless --> everything is predicted to be tetneg
plan_lasso <- drake_plan(
  lasso_res_mixpanel = target(iter_fit_best_lasso(em_eqdf, markers, panel_name='mixpanel', grid_size=30, v = 10, r =1), format='qs'),
  lasso_res_oldpanel = target(iter_fit_best_lasso(em_eqdf, markers, panel_name='oldpanel', grid_size=30, v = 10, r =1), format='qs'),
  lasso_res_newpanel = target(iter_fit_best_lasso(em_eqdf, markers, panel_name='newpanel', grid_size=30, v = 10, r =1), format='qs'),
  
  lasso_panels = list(
    mix=lasso_res_mixpanel,
    new=lasso_res_newpanel,
    old=lasso_res_oldpanel
    ), 
  lasso_penalty_metrics = lasso_panels %>% 
    map(~ .x$tuned_model %>% 
          collect_metrics() %>% 
          filter(.metric %in% c('sens', 'spec'))) %>% 
    bind_rows(.id='panel'), 
  
  lasso_penalty_performance_plot = lasso_penalty_metrics %>% 
    ggplot(aes(penalty, mean)) +
    geom_line(aes(linetype=.metric, color=panel), size=2, alpha=0.7) +
    # geom_point(aes(shape=panel), size=2) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    geom_vline(xintercept = 0.1, linetype=2) +
    geom_text(aes(x=0.1, label="Suggested penalty", y=0.25), angle=90, size=5, nudge_x=-.01) +
    theme_minimal() +
    labs(title = 'Model performance by lasso penalty',
         subtitle = 'An increased penalty removes less important predictors',
         y = 'Model score by metric',
         caption = 'Vertical line Suggested penalty'),
  
  lasso_coeffs = map(lasso_panels, function(m) { list(full=get_fit_coeff(m$wflow, em_eqdf, penalty = 0),
                                                      optimal=get_fit_coeff(m$wflow, em_eqdf, penalty = 0.1))}),
  target_name = target(
    command = {
      rmarkdown::render(knitr_in("doc/lasso_report.Rmd"))
      file_out("doc/lasso_report.html")
    }
  )
)