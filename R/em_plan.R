read_day6_em_tetp_fcs <- function(day6_meta, markers) {
  
  file_paths <- dir_ls('data/2008265_AC/CD4_d6_EN_TetPos_Tem_b7pos/', glob = '*.fcs')
  
  fcs_files <- map(file_paths, read_fcs, markers = markers)
  tibble(filename = basename(file_paths) %>% 
           str_replace('_Tem_b7Pos', '') %>% 
           str_replace('EN_d6', '_EN_d6'),
         fcs = fcs_files) %>% 
    left_join(day6_meta, by = 'filename')
}

em_plan <-
  drake_plan(
    
    # markers = read_markers(),
    # 
    # day6_meta = read_day6_meta(),
    day6_em_raw = read_day6_em_tetp_fcs(day6_meta, filter(markers, panel == 'mixpanel')),
    day6_tetp_em_tbl = target(day6_em_raw %>%
                                select(donor, sample_type, fcs) %>%
                                unnest(cols=fcs) %>%
                                mutate(row_id = 1:n()) %>%
                                as_tibble(.name_repair = 'unique') %>% 
                                bind_rows(filter(day6_tbl, sample_type == 'tetneg')),
                              format='fst_tbl'),
    em_eqdf = target(
      day6_tetp_em_tbl %>%
        group_by(donor, sample_type) %>%
        slice_sample(n=800) %>%
        ungroup(),
      format='fst_tbl'),
    em_test_out_of_sample = day6_tetp_em_tbl %>%
      anti_join(eqdf, by = 'row_id'),

    em_d6split = initial_split(em_eqdf, strata = intersect(em_eqdf$donor, em_eqdf$sample_type)),
    em_train = training(em_d6split),
    em_test = testing(em_d6split),

    
    em_glm_final = wf_glm %>% 
      fit(em_train),
    
    em_glm_pred = bind_cols(
      select(em_test, donor, sample_type, row_id),
      em_glm_final %>% 
        predict(em_test),
      em_glm_final %>% 
        predict(em_test, type='prob')
    ),
    
    em_glm_auc = em_glm_pred %>% roc_auc(sample_type, .pred_tetpos),
    em_glm_cm = em_glm_pred %>% conf_mat(sample_type, .pred_class),
    em_glm_metrics = summary(em_glm_cm), 
    
    em_glm_PE_pred = bind_cols(
      select(pre_tbl, donor, sample_time, row_id),
      em_glm_final %>% 
        predict(pre_tbl),
      em_glm_final %>% 
        predict(pre_tbl, type='prob')
    )
  )

    
