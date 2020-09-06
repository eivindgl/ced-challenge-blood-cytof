##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param day6_raw
##' @return
##' @author Eivind Gard Lund
##' @export
convert_fcs_to_xgboost_matrix <- function(day6_raw) {
  day6_raw %>% 
    select(sample_type, fcs) %>% 
    unnest(cols=fcs)
}
