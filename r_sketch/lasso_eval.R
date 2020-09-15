elastic_scores %>% 
  ggplot(aes(n, pr_auc, color = panel)) + 
  geom_line() + scale_y_continuous(limits = c(0.5,1)) +
  labs(title='Model performance by number of features and panel',
       y = 'Area under the precision recall curve',
       x = 'Number of features')
lasso_res_mixpanel$best

lasso_res_mixpanel$tuned_model %>% 
  collect_metrics() %>% 
  filter(mean > 0.9, .metric == 'pr_auc') %>% 
  ggplot(aes(penalty, mean)) +
  geom_point()

z <- em_eqdf %>% 
  group_by(sample_type) %>% 
  slice_sample(n=500) %>% 
  ungroup()

z %>% 
  ggplot(aes(CD38, CXCR3, color=sample_type)) +
  geom_point()
s
rval$final %>% 
  tidy() %>% 
  filter(estimate != 0) %>% 
  arrange(desc(abs(estimate)))

glm(sample_type ~ . -1, family = binomial(), data = select(em_eqdf, sample_type, starts_with('CD')) %>% mutate(sample_type = sample_type == 'tetpos')) %>% 
  tidy()

iter_fit_best_lasso <- function(em_eqdf, markers, panel_name='mixpanel', v = 5, r =3) {
    panel_features <- c(filter(markers, panel == panel_name) %>% pull(protein), 'sample_type')
    skip_features <- setdiff(colnames(em_eqdf), panel_features) 
    cv_splits <- vfold_cv(em_eqdf, v = v, repeats = r)
    
    # x <- fit_elastic_net(em_eqdf, cv_splits, grid_size = GRID_SIZE, skip_features = skip_features) 
    
    spec <- logistic_reg(penalty = 0.1, mixture = 1) %>% 
      set_engine('glmnet')
    
    
    rec <- em_eqdf %>% 
      recipe(sample_type ~ .) %>% 
      update_role(all_of(skip_features), new_role = 'ID')
    
    rval <- list()
    rval$wflow <- logit_wflow <- workflow() %>%
      add_recipe(rec) %>%
      add_model(spec)
    
    rval$final <- rval$wflow %>% 
      fit(em_eqdf)
    rval$final %>% 
      tidy() %>% 
      filter(estimate != 0) %>% 
      arrange(desc(abs(estimate)))
      
      arrange(desc(abs(estimate)))
    rval$best <- rval$tuned_model %>% 
      select_best(metric='mn_log_loss')
    rval$final <-  finalize_workflow(rval$wflow, rval$best) %>% 
      fit(em_eqdf)
    return(rval)
}


lasso_res_mixpanel$final  %>% 
  tidy() %>% 
  arrange(desc(abs(estimate)))


lasso_res_newpanel$wflow %>% 
  update_model(logistic_reg(penalty = 0.1, mixture = 1) %>% 
                 set_engine('glmnet')) %>% 
  fit(em_train) %>% 
  tidy() %>% 
  filter(estimate != 0)

mt <- lasso_res_mixpanel$wflow %>% 
  update_model(logistic_reg(penalty = 0.1, mixture = 1) %>% 
                 set_engine('glmnet')) %>% 
  fit(em_train)

mt %>% 
  tidy() %>% 
  arrange(desc(abs(estimate)))
# mt %>% 
lasso_res_mixpanel$final %>% 
  predict(em_test) %>%
  bind_cols(select(em_test, sample_type)) %>% 
  conf_mat(sample_type, .pred_class) %>% 
  summary()
