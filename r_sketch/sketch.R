glm_scores %>%
  ggplot(aes(n, roc_auc, color = panel)) +
  geom_line(aes(linetype=panel), size=2) + 
  scale_y_continuous(limits = c(0.8,1), labels = scales::label_percent(accuracy=1)) +
  scale_x_continuous(limits = c(0,15)) +
  labs(title='Model performance by number of features and panel',
       y = 'ROC - Area under curve',
       x = 'Number of features',
       caption = 'Iteratively fitted glm models with weakest predictor removed per round.
       Models were evaluated using 10-fold cross validation with 3 repeats.') +
  theme_minimal()

library(corrplot) # Correlation plot

panel_name <- 'mixpanel'
df <- em_eqdf %>% 
  clean_names()
panel_features <- c(filter(markers, panel == panel_name) %>% pull(protein), 
                    'sample_type') %>% 
  make_clean_names()
skip_features <- setdiff(colnames(df), panel_features) 
recipes(em_eqdf) %>% 
  formula(set)

em_eqdf %>% 
  # filter(sample_type=='tetpos') %>% 
  select(-row_id, -donor, -sample_type) %>% 
  cor() %>% 
  corrplot(type='full', order = 'hclust')
