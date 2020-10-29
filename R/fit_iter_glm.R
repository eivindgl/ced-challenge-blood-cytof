fit_vfold_glm <- function(em_train, cv_splits,
                            skip_features) {
  rval <- list(skip_features = skip_features)
  spec <- logistic_reg() %>% 
    set_engine('glm')
  
  rec <- em_train %>% 
    recipe(sample_type ~ .) %>% 
    update_role(all_of(skip_features), new_role = 'ID')
  
  rval$wflow <- logit_wflow <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)
  
  rval$tuned_model <- fit_resamples(rval$wflo, resamples=cv_splits)
  rval$best <- rval$tuned_model %>% 
    select_best(metric='roc_auc')
  rval$final <-  finalize_workflow(rval$wflow, rval$best) %>% 
    fit(em_train)
  return(rval)
}

glm_least_important_term <- function(x) {
  x %>% 
    tidy() %>% filter(!str_detect(term, 'Intercept')) %>% 
    arrange(p.value) %>% tail(1) %>% pull(term)
}

# collect_best_metrics <- function(x) {
#   x$tuned_model %>% 
#     collect_metrics() %>% 
#     filter(.config == x$best$.config) %>% 
#     pivot_wider(id_cols = -everything(), names_from = .metric, values_from=mean) %>% 
#     mutate(model_name = x$model_name)
# }

iter_fit_best_glm <- function(em_eqdf, markers, panel_name, v, r, opt_skip=character(0)) {
  df <- em_eqdf 
  panel_features <- c(filter(markers, panel == panel_name) %>% pull(protein), 
                      'sample_type') 
  skip_features <- unique(c(setdiff(colnames(df), panel_features),
                            intersect(opt_skip, colnames(df))))
  num_predictors <- length(setdiff(panel_features, skip_features)) - 1
  cv_splits <- vfold_cv(df, v = v, repeats = r)
  res_by_n_markers <- list()
  for (i in num_predictors:0) {
    model_name <- str_glue('model_fsize_{i}')
    print(str_glue('Fitting {model_name} ...'))
    x <- fit_vfold_glm(df, cv_splits, skip_features = skip_features) %>% 
      c(list(model_name = model_name))
    skip_features <- c(skip_features, glm_least_important_term(x$final))
    res_by_n_markers[[model_name]] <- x
  }
  return(res_by_n_markers)
}


collect_best_glm_metrics <- function(x, panel) {
  n <- nrow(x$final %>% tidy()) - 1
  x$tuned_model %>% 
    collect_metrics() %>% 
    pivot_wider(id_cols = -everything(), names_from = .metric, values_from=mean) %>% 
    mutate(model_name = str_glue('top_{n}_{panel}_markers'))
}


plan_iter_select_glm <-
  drake_plan(
    
    glm_res_mixpanel = target(iter_fit_best_glm(em_eqdf, markers, panel_name='mixpanel', v = 10, r =1), format='qs'),
    glm_res_oldpanel = target(iter_fit_best_glm(em_eqdf, markers, panel_name='oldpanel', v = 10, r =1), format='qs'),
    glm_res_newpanel = target(iter_fit_best_glm(em_eqdf, markers, panel_name='newpanel', v = 10, r =1), format='qs'),
    # 
    glm_scores = bind_rows(
      new = glm_res_newpanel %>%
        # keep(~ .$final %>% tidy() %>% nrow() > 1) %>% 
      map_dfr(collect_best_glm_metrics, panel='newpanel'),
      old = glm_res_oldpanel %>%
        # keep(~ .$final %>% tidy() %>% nrow() > 1) %>% 
        map_dfr(collect_best_glm_metrics, panel='oldpanel'),
      mixed = glm_res_mixpanel %>%
        # keep(~ .$final %>% tidy() %>% nrow() > 1) %>% 
        map_dfr(collect_best_glm_metrics, panel='mixpanel'),
      .id = 'panel') %>%
      mutate(n = as.numeric(str_extract(model_name, '\\d+')),
        model_name = fct_reorder(model_name, n, min)),
    
    opt_skip = c('cd62l', 'int_b7', 'cd45ra'),
    glm_res_sub_mixpanel = target(iter_fit_best_glm(em_eqdf, markers, panel_name='mixpanel', v = 10, r =3, opt_skip = opt_skip), format='qs'),
    glm_res_sub_oldpanel = target(iter_fit_best_glm(em_eqdf, markers, panel_name='oldpanel', v = 10, r =3, opt_skip = opt_skip), format='qs'),
    glm_res_sub_newpanel = target(iter_fit_best_glm(em_eqdf, markers, panel_name='newpanel', v = 10, r =3, opt_skip = opt_skip), format='qs'),
    
    glm_sub_scores = bind_rows(
      new = glm_res_sub_newpanel %>%
        map_dfr(collect_best_glm_metrics, panel='newpanel'),
      old = glm_res_sub_oldpanel %>%
        map_dfr(collect_best_glm_metrics, panel='oldpanel'),
      mixed = glm_res_sub_mixpanel %>%
        map_dfr(collect_best_glm_metrics, panel='mixpanel'),
      .id = 'panel') %>%
      mutate(n = as.numeric(str_extract(model_name, '\\d+')),
             model_name = fct_reorder(model_name, n, min)),
  )
# 

roc_plot <- function(glm_scores) {
  glm_scores %>%
    ggplot(aes(n, roc_auc, color = panel)) +
    geom_line(aes(linetype=panel), size=2) + 
    scale_y_continuous(limits = c(0.8,1), labels = scales::label_percent(accuracy=1)) +
    scale_x_continuous(limits = c(0,15)) +
    labs(title='Model performance by number of features and panel',
         y = 'ROC - Area under curve',
         x = 'Number of features',
         caption = 'Iteratively fitted glm models with weakest predictor removed per round.
           Models were evaluated using 10-fold cross validation with 3 repeats.') +
    theme_minimal()
}

extract_coeffs <- function(glm_res) {
 map(glm_res, ~ .x$final %>% tidy())
}

save_glm_coeffs <- function(glm_res_mixpanel, glm_res_newpanel, glm_res_oldpanel) {
  dir_create('out')
  extract_coeffs(glm_res_mixpanel) %>% 
    write_xlsx('out/mixpanel_top_N_predictors_all_markers.xlsx') 
  extract_coeffs(glm_res_oldpanel) %>% 
    write_xlsx('out/oldpanel_top_N_predictors_all_markers.xlsx') 
  extract_coeffs(glm_res_newpanel) %>% 
    write_xlsx('out/newpanel_top_N_predictors_all_markers.xlsx') 
  
  pdf('out/correlation_all_markers.pdf')
  em_eqdf %>% 
    select(-row_id, -donor, -sample_type) %>% 
    cor() %>% 
    corrplot(type='full', order = 'hclust')
  dev.off()
  
  p <- roc_plot(glm_scores)
  ggsave('out/roc_auc_all_markers.png', plot = p)
}

save_glm_sub_coeffs <- function(glm_res_sub_mixpanel, glm_res_sub_newpanel, glm_res_sub_oldpanel, glm_sub_scores) {
  dir_create('out')
  extract_coeffs(glm_res_sub_mixpanel) %>% 
    write_xlsx('out/mixpanel_top_N_predictors_sub_markers.xlsx') 
  extract_coeffs(glm_res_sub_oldpanel) %>% 
    write_xlsx('out/oldpanel_top_N_predictors_sub_markers.xlsx') 
  extract_coeffs(glm_res_sub_newpanel) %>% 
    write_xlsx('out/newpanel_top_N_predictors_sub_markers.xlsx') 
  glm_sub_scores %>% 
    write_xlsx('out/glm_scores.xlsx') 
  
  p <- roc_plot(glm_sub_scores)
  ggsave('out/roc_auc_sub_markers.png', plot = p)
}
