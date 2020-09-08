the_plan <-
  drake_plan(
    
    markers = read_markers() %>% 
      filter(panel == 'mixpanel'),
    day6_meta = read_day6_meta(),
    day6_raw = read_day6_fcs(day6_meta, markers),
    xgdf = convert_fcs_to_xgboost_matrix(day6_raw),
    
    d6split = initial_split(xgdf, strata = sample_type),
    train = training(d6split),
    test = testing(d6split),
    xgb_simple_wf = xgb_simple_workflow(),
    xgb_simple_res = xgb_tune_simple(xgb_simple_wf, train),
    
    day6_tbl = day6_raw %>%
      select(donor, sample_type, fcs) %>%
      unnest(cols=fcs),
    xgb_downsample_rec = day6_tbl %>% 
      recipe(sample_type ~ .) %>% 
      step_mutate(donor_sample_type = interaction(donor, sample_type), role = 'ID') %>% 
      step_downsample(donor_sample_type) %>% 
      prep(retain=TRUE) %>% 
      juice()
      step_downsample()
)
