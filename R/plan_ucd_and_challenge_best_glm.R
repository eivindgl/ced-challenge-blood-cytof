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
           donor = if_else(donor == 'CD2048', 'CD5048', donor),
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
        slice_sample(n=164) %>%
        ungroup(),
      format='fst_tbl'),
    ucd_chal_df = target(ucd_eqdf %>% 
                           bind_rows(em_eqdf %>% 
                                       mutate(disease_status='challenge') %>% 
                                       group_by(donor, sample_type) %>%
                                       slice_sample(n=40) %>%
                                       ungroup()),
                         format = 'fst_tbl'),
    ucd_chal_glm_res_mixpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='mixpanel', v = 10, r =1), format='qs'),
    ucd_chal_glm_res_oldpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='oldpanel', v = 10, r =1), format='qs'),
    ucd_chal_glm_res_newpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='newpanel', v = 10, r =1), format='qs'),
    #
    

    # opt_skip = c('cd62l', 'int_b7', 'cd45ra'),
    ucd_chal_glm_res_sub_mixpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='mixpanel', v = 10, r =3, opt_skip = opt_skip), format='qs'),
    ucd_chal_glm_res_sub_oldpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='oldpanel', v = 10, r =3, opt_skip = opt_skip), format='qs'),
    ucd_chal_glm_res_sub_newpanel = target(iter_fit_best_glm(ucd_chal_df, markers, panel_name='newpanel', v = 10, r =3, opt_skip = opt_skip), format='qs'),
    # # 
    ucd_chal_glm_scores =  extract_chal_glm_scores(ucd_chal_glm_res_newpanel, ucd_chal_glm_res_oldpanel,
                                           ucd_chal_glm_res_mixpanel),
    ucd_chal_glm_sub_scores =  extract_chal_glm_scores(ucd_chal_glm_res_sub_newpanel, ucd_chal_glm_res_sub_oldpanel,
                                                       ucd_chal_glm_res_sub_mixpanel)
  )
# 


extract_chal_glm_scores <- function(ucd_chal_glm_res_newpanel, ucd_chal_glm_res_oldpanel, 
                                    ucd_chal_glm_res_mixpanel) {
  bind_rows(
    new = ucd_chal_glm_res_newpanel %>%
      map_dfr(collect_best_glm_metrics, panel='newpanel'),
    old = ucd_chal_glm_res_oldpanel %>%
      map_dfr(collect_best_glm_metrics, panel='oldpanel'),
    mixed = ucd_chal_glm_res_mixpanel %>%
      map_dfr(collect_best_glm_metrics, panel='mixpanel'),
    .id = 'panel') %>%
    mutate(n = as.numeric(str_extract(model_name, '\\d+')),
           model_name = fct_reorder(model_name, n, min))
}
ucd_save_glm_sub_coeffs <- function(ucd_chal_glm_res_sub_newpanel, ucd_chal_glm_res_sub_oldpanel,
                                    ucd_chal_glm_res_sub_mixpanel, ucd_chal_glm_sub_scores) {
  dir_create('out')
  extract_coeffs(ucd_chal_glm_res_sub_mixpanel) %>% 
    writexl::write_xlsx('out/ucd_and_chal_mixpanel_sub_top_N_predictors_markers.xlsx') 
  extract_coeffs(ucd_chal_glm_res_sub_oldpanel) %>% 
    writexl::write_xlsx('out/ucd_and_chal_oldpanel_sub_top_N_predictors_markers.xlsx') 
  extract_coeffs(ucd_chal_glm_res_sub_newpanel) %>% 
    writexl::write_xlsx('out/ucd_and_chal_newpanel_sub_top_N_predictors_markers.xlsx') 
  
  ucd_chal_glm_sub_scores %>% 
    writexl::write_xlsx('out/ucd_chal_glm_scores.xlsx')
  
  (p <- roc_plot(ucd_chal_glm_sub_scores))
  ggsave('out/ucd_and_chal_roc_auc_sub_markers.pdf', plot = p)
}

