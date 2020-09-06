##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param xgb_simple_wf
##' @param train
##' @return
##' @author Eivind Gard Lund
##' @export
xgb_tune_simple <- function(xgb_simple_wf, train) {

  xgb_grid = grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train),
    #learn_rate(),
    size = 50
  )
  
  set.seed(234)
  folds = vfold_cv(train, strata = sample_type)
  set.seed(234)
  tune_grid(
    xgb_simple_wf,
    resamples = folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE, verbose=TRUE)
  )

}
