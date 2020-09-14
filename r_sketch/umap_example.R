dimred_sample <- bind_rows(
  pre_tbl %>% 
    group_by(donor) %>% 
    slice_sample(n = 3000) %>% 
    mutate(sample_type = 'pre'),
  em_eqdf %>% 
    mutate(sample_time = 'day_6')
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
