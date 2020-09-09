elastic_plan <-
  drake_plan(
    
    markers = read_markers() %>% 
      filter(panel == 'mixpanel'),
    day6_meta = read_day6_meta(),
    day6_raw = read_day6_fcs(day6_meta, markers),
    day6_tbl = target(day6_raw %>%
                        select(donor, sample_type, fcs) %>%
                        unnest(cols=fcs) %>%
                        mutate(row_id = 1:n()) %>% 
                        as_tibble(),
                      format='fst_tbl'),
    eqdf = target(
      day6_tbl %>% 
        group_by(donor, sample_type) %>% 
        slice_sample(n=260) %>% 
        ungroup(), 
      format='fst_tbl'),
    test_out_of_sample = day6_tbl %>% 
      anti_join(eqdf, by = 'row_id'),
    
    d6split = initial_split(eqdf, strata = intersect(eqdf$donor, eqdf$sample_type)),
    train = training(d6split),
    test = testing(d6split),
  )
