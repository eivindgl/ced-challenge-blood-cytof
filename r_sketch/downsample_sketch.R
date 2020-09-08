z <- day6_raw %>%
  select(donor, sample_type, fcs) %>%
  unnest(cols=fcs) %>%
  mutate(row_id = 1:n())

df <- z %>% 
  group_by(donor, sample_type) %>% 
  slice_sample(n=260) %>% 
  ungroup()

xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  #learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

rec <- df %>% 
  recipe(sample_type ~ .) %>% 
  update_role(donor, row_id, new_role = 'ID')

wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(xgb_spec)

doParallel::registerDoParallel()
dsample_res = xgb_tune_simple(wf, df, grid_size = 50)
dsample_best <- dsample_res %>% 
  select_best(metric='roc_auc')
dsample_final_train <- finalize_workflow(wf, dsample_best) %>% 
  fit(df)

test <- z %>% 
  anti_join(df, by = 'row_id')
test_res <- bind_cols(
  dsample_final_train %>% 
    predict(test),
  dsample_final_train %>% 
    predict(test, type='prob'),
  test
)
test_res_conf_mat <- test_res %>% 
  conf_mat(sample_type, .pred_class)
test_res_conf_mat %>% 
  summary()

test_res %>% 
  roc_curve(sample_type, .pred_tetpos) %>%
  autoplot() + theme_minimal()

pfun <- function(object, newdata) predict(object, data = newdata)$predictions
dsample_final_train %>%
  pull_workflow_fit() %>%
  vip(num_features = 20, method='permute', train=df, target='sample_type', metric='auc')


glm_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(set_engine(logistic_reg(), "glm"))
glm_fit <- glm_wf %>% 
  fit(data = df)

glm_test_res <- bind_cols(
  predict(glm_fit, test),
  predict(glm_fit, test, type='prob'),
  select(test, sample_type)
)
glm_test_res_conf_mat <- glm_test_res %>% 
  conf_mat(sample_type, .pred_class)
glm_test_res_conf_mat %>% 
  summary()

glm_fit %>% 
  tidy() %>% 
  filter(p.value < 1e-2) %>%
  arrange(p.value)

glm_fit %>% 
  pull_workflow_fit() %>% 
  vip()
