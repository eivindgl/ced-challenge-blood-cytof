spec <- logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine('glmnet') 

# Hyperparameter grid
logit_grid <- spec %>%
  parameters() %>%
  grid_latin_hypercube(size = 60)

rec <- eqdf %>% 
  recipe(sample_type ~ .) %>% 
  update_role(donor, row_id, new_role = 'ID')

logit_wflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(spec)

cv_splits <- vfold_cv(eqdf, v = 10)
doParallel::registerDoParallel()
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

vip(logit_final %>% pull_workflow_fit(), include_type=T)
vip(logit_final %>% pull_workflow_fit(), include_type=T, method='permute', train=z, 
    target=eqdf$sample_type, metric='AUC', 
    pred_wrapper=(function(o, newdata) predict(o, as.matrix(newdata), type='response')), reference_class='tetpos')
vip(logit_final %>% pull_workflow_fit(), include_type=T, method='shap', train=as.matrix(z), 
    #target=eqdf$sample_type, metric='AUC', 
    pred_wrapper=(function(o, newdata) predict(o, as.matrix(newdata), type='response')), 
    # reference_class='tetpos'
    nsim=100
    )
vip(logit_final %>% pull_workflow_fit(), include_type=T)

vi_shap(logit_final %>% pull_workflow_fit(), exact=T, feature_names=colnames(z))
tidy(logit_final) %>% 
  arrange(desc(abs(estimate))) %>% print(n=Inf)

top8_terms <- tidy(logit_final) %>% 
  arrange(desc(abs(estimate))) %>% 
  tail(-1) %>% 
  head(2) %>% 
  pull(term)

top8_rec <- rec %>% 
  update_role(all_predictors(), new_role='not_top_pred') %>% 
  update_role(all_of(top8_terms), new_role = 'predictor')
  
top_logit_wflow <- workflow() %>%
  add_recipe(top8_rec) %>%
  add_model(spec)

top_tuned_model <- tune_grid(top_logit_wflow,
                         resamples = cv_splits,
                         grid = logit_grid,
                         metrics = metric_set(mn_log_loss, roc_auc, pr_auc, gain_capture),
                         control = control_resamples(verbose = TRUE))
(top_tuned_metrics = top_tuned_model %>% collect_metrics() %>% 
    pivot_wider(-std_err, names_from = .metric, values_from = mean) %>% 
    arrange(desc(roc_auc)))
top_model <- top_tuned_model %>% 
  select_best() %>% 
  finalize_workflow(top_logit_wflow, .) %>% 
  fit(eqdf) 

top_model %>% 
  tidy()

top_pred <- top_model %>% 
  (function(f, test) {
    x <- predict(f, test, type = 'prob')
    y <- predict(f, test)
    bind_cols(select(test, donor, sample_type), x,y)
  })(test_out_of_sample)

(test_top_roc = top_pred %>% 
    roc_auc(truth = sample_type, .pred_tetpos))
(test_metrics = pred %>% 
    conf_mat(sample_type, .pred_class) %>% 
    summary())

day6_tbl %>%
  group_by(donor, sample_type) %>% 
  slice_sample(n = 100) %>% 
  ggplot(aes(CXCR3, NKG2D, color = sample_type)) +
  #geom_density_2d()
  geom_point(alpha=.4) + 
  facet_wrap(~donor, nrow = 3) +
  labs(title='top2 features separating tet+/- across donors',
       subtitle='too good to be true?')
ggsave('top_2_features_scatterplot_by_donor.png')  
  
