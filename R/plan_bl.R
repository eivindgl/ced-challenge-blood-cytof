read_day0_em_fcs <- function(markers) {
  file_paths <- c(
    dir_ls('data/2008265_AC/CD4_BL_EN_TetPos_Tem_b7pos/', glob = '*.fcs'),
    dir_ls('data/2008265_AC/CD4_BL_EN_TetPos_neg/', glob = '*TetNeg*.fcs')
  )
  
  fcs_files <- map(file_paths, read_fcs, markers = filter(markers, panel == 'mixpanel'))
  df <- tibble(filename = basename(file_paths)) %>% 
    mutate(donor = str_extract(filename, '^\\d+') %>% str_c('CD', .),
           sample_type = if_else(str_detect(filename, stringr::fixed('tetpos', ignore_case=T)), 
                                 'tetpos', 'tetneg'),
           sample_time = 'day_0',
           fcs = fcs_files
    ) %>%
    select(donor, sample_type, sample_time, fcs) %>%
    unnest(cols=fcs) %>%
    mutate(row_id = 1:n()) %>%
    as_tibble(.name_repair = 'unique')
  return (df)
}


bl_plan <-
  drake_plan(
    bl_raw = target(read_day0_em_fcs(markers), format='fst_tbl'),
    bl_chal_df = target(
      bl_raw %>%
        group_by(donor, sample_type) %>%
        slice_sample(n=200) %>%
        ungroup(),
      format='fst_tbl'),
    
    bl_chal_glm_res_mixpanel = target(iter_fit_best_glm(bl_chal_df, markers, panel_name='mixpanel', v = 10, r =1), format='qs'),
    bl_chal_glm_res_oldpanel = target(iter_fit_best_glm(bl_chal_df, markers, panel_name='oldpanel', v = 10, r =1), format='qs'),
    bl_chal_glm_res_newpanel = target(iter_fit_best_glm(bl_chal_df, markers, panel_name='newpanel', v = 10, r =1), format='qs'),
    
    bl_chal_glm_scores =  extract_chal_glm_scores(bl_chal_glm_res_newpanel, bl_chal_glm_res_oldpanel,
                                                       bl_chal_glm_res_mixpanel)
  )

bl_save_glm_sub_coeffs <- function(bl_chal_glm_res_mixpanel, bl_chal_glm_res_newpanel, 
                                    bl_chal_glm_res_oldpanel, bl_chal_glm_scores) {
  dir_create('out')
  extract_coeffs(bl_chal_glm_res_mixpanel) %>% 
    writexl::write_xlsx('out/bl_mixpanel_top_N_predictors_markers.xlsx') 
  extract_coeffs(bl_chal_glm_res_oldpanel) %>% 
    writexl::write_xlsx('out/bl_oldpanel_top_N_predictors_markers.xlsx') 
  extract_coeffs(bl_chal_glm_res_newpanel) %>% 
    writexl::write_xlsx('out/bl_newpanel_top_N_predictors_markers.xlsx') 
  
  bl_chal_glm_scores %>% 
    writexl::write_xlsx('out/bl_chal_glm_scores.xlsx')
  
  p <- roc_plot(bl_chal_glm_scores)
  ggsave('out/bl_roc_auc_markers.png', plot = p)
}

