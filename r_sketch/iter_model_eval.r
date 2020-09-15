


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