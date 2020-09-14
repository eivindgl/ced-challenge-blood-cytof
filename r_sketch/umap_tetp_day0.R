read_day0_em_fcs <- function(markers) {
  markers <- filter(markers, panel == 'mixpanel')
  file_paths <- dir_ls('data/2008265_AC/CD4_BL_EN_TetPos_Tem_b7pos/', glob = '*.fcs')
  
  fcs_files <- map(file_paths, read_fcs, markers = markers)
  tibble(filename = basename(file_paths)) %>% 
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
}

day0_tetp_tbl <- read_day0_em_fcs(markers)

dimred_sample <- bind_rows(
  pre_tbl %>% 
    group_by(donor) %>% 
    slice_sample(n = 3000) %>% 
    mutate(sample_type = 'pre'),
  em_eqdf %>% 
    mutate(sample_time = 'day_6'),
  day0_tetp_tbl
)

umap_rec <- dimred_sample %>% 
  recipe(sample_type ~ .) %>% 
  update_role(donor, sample_time, sample_type, row_id, new_role = 'ID') %>% 
  step_umap(all_predictors()) %>% 
  prep()

udf <- umap_rec %>% 
  juice()

udf %>% 
  ggplot(aes(umap_1, umap_2, color = sample_type)) +
  geom_point(alpha=0.4) + 
  facet_wrap(sample_time ~ sample_type)
