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
    xgb_simple_res = xgb_tune_simple(xgb_simple_wf, train)
    
    
    
    
    
    

)
