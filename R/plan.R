the_plan <-
  drake_plan(
    
    markers = read_markers(),
    
    day6_meta = read_day6_meta(),
    day6_raw = read_day6_fcs(day6_meta, filter(markers, panel == 'mixpanel')),
    day6_tbl = target(day6_raw %>%
                        select(donor, sample_type, fcs) %>%
                        unnest(cols=fcs) %>%
                        mutate(row_id = 1:n()) %>% 
                        as_tibble(),
                      format='fst_tbl'),
    eqdf = target(
      day6_tbl %>% 
        group_by(donor, sample_type) %>% 
        slice_sample(n=800) %>% 
        ungroup(), 
      format='fst_tbl'),
    test_out_of_sample = day6_tbl %>% 
      anti_join(eqdf, by = 'row_id'),
    
    d6split = initial_split(eqdf, strata = intersect(eqdf$donor, eqdf$sample_type)),
    train = training(d6split),
    test = testing(d6split),
    
    
    ## Simple GLM
    rec = train %>% 
      recipe(sample_type ~ .) %>% 
      update_role(donor, row_id, new_role='id'),
    
    wf_glm = workflow() %>%
      add_recipe(rec) %>% 
      add_model(logistic_reg() %>% 
                  set_engine('glm')),
    
    glm_final = wf_glm %>% 
      fit(train),
    
    glm_pred = bind_cols(
      select(test, donor, sample_type, row_id),
      glm_final %>% 
        predict(test),
      glm_final %>% 
        predict(test, type='prob')
    ),
    
    glm_auc = glm_pred %>% roc_auc(sample_type, .pred_tetpos),
    glm_cm = glm_pred %>% conf_mat(sample_type, .pred_class),
    glm_metrics = summary(glm_cm), 
    
    ## For prediction
    pre_enriched_meta = read_pre_enriched_meta(),
    pre_raw = read_pre_enriched_fcs(pre_enriched_meta, filter(markers, panel == 'mixpanel')),
    pre_tbl = target(pre_raw %>%
                        select(donor, sample_time, fcs) %>%
                        unnest(cols=fcs) %>%
                        mutate(row_id = 1:n()) %>% 
                        as_tibble(),
                      format='fst_tbl'),
  )
