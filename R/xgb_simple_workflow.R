##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author Eivind Gard Lund
##' @export
xgb_simple_workflow <- function() {

  xgb_spec <- boost_tree(
    trees = 1000, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## first three: model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    #learn_rate = tune(),                         ## step size
  ) %>% 
    set_engine("xgboost") %>% 
    set_mode("classification")
  
  workflow() %>%
    add_formula(sample_type ~ .) %>%
    add_model(xgb_spec)
}
