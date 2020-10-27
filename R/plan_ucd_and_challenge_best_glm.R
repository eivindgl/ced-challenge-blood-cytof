read_ucd_fcs <- function(markers) {
  
  file_paths <- c(
    dir_ls('data/2008265_AC/UCeD/UCeD_TetPos_Tem_b7Pos/', glob = '*.fcs'),
    dir_ls('data/2008265_AC/UCeD/UCeD_TetNeg_AllCells/', glob = '*.fcs')
    )
  
  fcs_files <- map(file_paths, read_fcs, markers = filter(markers, panel == 'mixpanel'))
  df <- tibble(file_id = basename(file_paths) %>% 
           str_replace('_Tem_b7Pos', '') %>% 
           path_ext_remove(),
         fcs = fcs_files)

  df %>% 
    mutate(donor = str_extract(file_id, '(?<=UCeD)(\\d+)'),
           donor = str_c('CD', donor),
           sample_type = if_else(str_detect(file_id, 'TetPos'),
                                 'tetpos', 'tetneg'),
           disease_status='untreated')
}

plan_ucd_and_challenge_best_glm <-
  drake_plan(
    ucd_raw = read_ucd_fcs(markers),
    ucd_full = target(ucd_raw %>%
                   select(donor, disease_status, sample_type, fcs) %>%
                   unnest(cols=fcs) %>%
                   mutate(row_id = 1:n()) %>%
                   as_tibble(.name_repair = 'unique') %>% 
                   clean_names(),
                 format='fst_tbl'),
    ucd_eqdf = target(
      ucd_full %>%
        group_by(donor, sample_type) %>%
        slice_sample(n=300) %>%
        ungroup(),
      format='fst_tbl'),
    ucd_chal_df = target(ucd_eqdf %>% 
                           bind_rows(em_eqdf %>% 
                                       mutate(disease_status='challenge') %>% 
                                       group_by(donor, sample_type) %>%
                                       slice_sample(n=40) %>%
                                       ungroup()),
                         format = 'fst_tbl'),
    # ucd_chal_glm_res_mixpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='mixpanel', v = 10, r =1), format='qs'),
    # ucd_chal_glm_res_oldpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='oldpanel', v = 10, r =1), format='qs'),
    # ucd_chal_glm_res_newpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='newpanel', v = 10, r =1), format='qs'),
    # # 
    ucd_chal_glm_scores = bind_rows(
      new = ucd_chal_glm_res_newpanel %>%
        # keep(~ .$final %>% tidy() %>% nrow() > 1) %>%
        map_dfr(collect_best_glm_metrics, panel='newpanel'),
      old = ucd_chal_glm_res_oldpanel %>%
        # keep(~ .$final %>% tidy() %>% nrow() > 1) %>%
        map_dfr(collect_best_glm_metrics, panel='oldpanel'),
      mixed = ucd_chal_glm_res_mixpanel %>%
        # keep(~ .$final %>% tidy() %>% nrow() > 1) %>%
        map_dfr(collect_best_glm_metrics, panel='mixpanel'),
      .id = 'panel') %>%
      mutate(n = as.numeric(str_extract(model_name, '\\d+')),
             model_name = fct_reorder(model_name, n, min)),

    # opt_skip = c('cd62l', 'int_b7', 'cd45ra'),
    ucd_chal_glm_res_sub_mixpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='mixpanel', v = 10, r =1, opt_skip = opt_skip), format='qs'),
    ucd_chal_glm_res_sub_oldpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='oldpanel', v = 10, r =1, opt_skip = opt_skip), format='qs'),
    ucd_chal_glm_res_sub_newpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='newpanel', v = 10, r =1, opt_skip = opt_skip), format='qs'),
    # 
    ucd_chal_glm_sub_scores = bind_rows(
      new = ucd_chal_glm_res_sub_newpanel %>%
        map_dfr(collect_best_glm_metrics, panel='newpanel'),
      old = ucd_chal_glm_res_sub_oldpanel %>%
        map_dfr(collect_best_glm_metrics, panel='oldpanel'),
      mixed = ucd_chal_glm_res_sub_mixpanel %>%
        map_dfr(collect_best_glm_metrics, panel='mixpanel'),
      .id = 'panel') %>%
      mutate(n = as.numeric(str_extract(model_name, '\\d+')),
             model_name = fct_reorder(model_name, n, min)),
  )
# 


