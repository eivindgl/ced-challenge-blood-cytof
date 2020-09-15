xgb_plan <- drake_plan(
  wf_xgb = workflow() %>%
    add_recipe(rec %>% 
                 update_role(CD123_LAG3, NKG2D, CD52, CD47, new_role='id')) %>% 
    add_model(boost_tree(trees = 1000, 
                         tree_depth = tune(), min_n = tune(), 
                         loss_reduction = tune(),                     ## first three: model complexity
                         sample_size = tune(), mtry = tune(),         ## randomness
                         learn_rate = 0.05,     ) %>% 
                set_mode('classification') %>% 
                set_engine('xgboost')),
  xgb_grid = grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), em_train),
    #learn_rate(),
    size = 300
  ),
  cv_folds = folds <- vfold_cv(em_train, strata = sample_type, v = 5, repeats = 3),
  
  em_res = tune_grid(
    wf_xgb,
    resamples = cv_folds,
    grid = xgb_grid,
    metrics = metric_set(mn_log_loss, roc_auc, precision, accuracy, pr_auc, gain_capture, mcc, f_meas, kap),
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  ),
  em_final = finalize_workflow(
    wf_xgb,
    select_best(em_res, metric='mn_log_loss')
  ) %>% 
    fit(em_train),
  
  em_xgb_pred = bind_cols(
    select(em_test, donor, sample_type, row_id),
    em_final %>% 
      predict(em_test),
    em_final %>% 
      predict(em_test, type='prob')
  ),
  
  em_xgb_auc = em_xgb_pred %>% roc_auc(sample_type, .pred_tetpos),
  em_xgb_cm = em_xgb_pred %>% conf_mat(sample_type, .pred_class),
  em_xgb_metrics = summary(em_glm_cm), 
  
  em_xgb_PE_pred = bind_cols(
    select(pre_tbl, donor, sample_time, row_id),
    em_final %>% 
      predict(pre_tbl),
    em_final %>% 
      predict(pre_tbl, type='prob')
  ), 
  
  em_xgb_PE_plot = em_xgb_PE_pred %>% 
    group_by(donor, sample_time) %>% 
    summarize(tetp_prop = mean(.pred_class == 'tetpos'), .groups='drop') %>% 
    ggplot(aes(donor, tetp_prop, fill=sample_time)) +
    geom_col(position = 'dodge2') +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = 'Estimated Prevalence of tetramer+ phenotype')
)
)