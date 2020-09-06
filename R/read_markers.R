##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

read_markers <- function() {
  df <- read_xlsx(file_in('data/2008265_AC/Fil- og markøroversikt/200825 markers for ranking.xlsx'))
  df %>% 
    pivot_longer(everything(), names_to='panel', values_to = 'metal_and_protein') %>% 
    filter(!is.na(metal_and_protein)) %>% 
    separate(metal_and_protein, into = c('metal', 'protein'), 
             sep = '_', extra = 'merge', remove=FALSE)
}
