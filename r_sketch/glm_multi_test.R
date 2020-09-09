mass_df <- eqdf %>% 
  select(-donor, -row_id) %>% 
  janitor::clean_names()
Xy <- mass_df %>% 
  mutate(y = sample_type) %>% 
  select(-sample_type)
m_full <- glm(formula = sample_type ~ ., family = binomial, data = mass_df)

m_full %>% 
  MASS::stepAIC(scope=list(lower=~1), direction='backward')
library(bestglm)

eqdf %>% 
  mutate(S)
  select(-donor) %>% 
  select(-sample_type, sample_type) %>% 
  bestglm(family=binomial)

  devtools::install_github("stevenpawley/recipeselectors")
library(recipeselectors)

rec_base <- eqdf %>% 
  select(-donor) %>% 
  recipe(sample_type ~ ., data = .) 

get_names <- function(r) {
  r %>% 
    prep() %>% 
    juice(all_predictors()) %>% 
    names()
}

rec_base %>%
  step_select_roc(all_predictors(), outcome = "sample_type", top_p = 10, threshold = 0.9) %>%
  get_names()  

rec_base %>%
  step_select_roc(all_predictors(), outcome = "sample_type", top_p = 10, threshold = 0.9) %>%
  get_names()  

recipeselectors:::step_select_roc_new

step.model <- MASS::stepAIC(m_full, direction = "both", 
                      trace = FALSE, k = log(ncol(mass_df)+3))
summary(step.model)
step.model %>% 
  tidy()

ncol(mass_df) -1 
leaps(mass_df[-1], mass_df[1], nvmax=8)

library(glmulti)
glmulti(sample_type ~ cd47 + cd70 + cd49b + cxcr3 + cd95 + nkg2d + ccr7 + cd45ra + pd_1 + ccr4 + cxcr6 + cd52 + cd28 + cd123_lag3 + icos + cd38 + cd132 + cd127 + int_a4 + cd103 + cd36 + cd161 + cd25 + cxcr5 + ccr9 + hla_dr + cd94, 
        data = mass_df,
        level = 1,               # No interaction considered
        method = "h",            # Exhaustive approach
        crit = "aic",            # AIC as criteria
        confsetsize = 5,         # Keep 5 best models
        plotty = F, report = F,  # No plot or interim reports
        fitfunction = "glm",     # glm function
        family = binomial) 

library(meifly)
z <- fitall(mass_df$sample_type, select(mass_df, -sample_type), method='glm')

library(doMC)
doMC::registerDoMC(cores=4)
library(caret)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
iter_ctrl <- rfeControl(functions = lmFuncs)
rfeIter(select(train,-sample_type), train$sample_type,
        select(test,-sample_type), test$sample_type,
        rfeControl = iter_ctrl,
        sizes = 6:11)

library(e1071)
subsets <- c(1:5, 10, 15, 20, 25)
rfe(select(mass_df,-sample_type), mass_df['sample_type'],
        rfeControl = ctrl,
        sizes = subsets)


