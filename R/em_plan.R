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
    # eqdf = target(
    #   day6_tbl %>% 
    #     group_by(donor, sample_type) %>% 
    #     slice_sample(n=260) %>% 
    #     ungroup(), 
    #   format='fst_tbl'),
    # test_out_of_sample = day6_tbl %>% 
    #   anti_join(eqdf, by = 'row_id'),
    # 
    # d6split = initial_split(eqdf, strata = intersect(eqdf$donor, eqdf$sample_type)),
    # train = training(d6split),
    # test = testing(d6split),
    # 
    # ## For prediction
    # pre_enriched_meta = read_pre_enriched_meta(),
    # pre_raw = read_pre_enriched_fcs(pre_enriched_meta, filter(markers, panel == 'mixpanel')),
    # pre_tbl = target(pre_raw %>%
    #                    select(donor, sample_time, fcs) %>%
    #                    unnest(cols=fcs) %>%
    #                    mutate(row_id = 1:n()) %>% 
    #                    as_tibble(),
    #                  format='fst_tbl'),
  )
