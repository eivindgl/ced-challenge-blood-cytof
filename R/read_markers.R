##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

read_markers <- function() {
  df <- read_xlsx(file_in('data/2008265_AC/Fil- og markøroversikt/200825 markers for ranking.xlsx'))
  z <- df %>% 
    pivot_longer(everything(), names_to='panel', values_to = 'metal_and_protein') %>% 
    filter(!is.na(metal_and_protein)) %>% 
    separate(metal_and_protein, into = c('metal', 'protein_unclean'), 
             sep = '_', extra = 'merge', remove=FALSE) %>% 
    mutate(metal=toupper(metal),
           metal_and_protein=toupper(metal_and_protein))
  
  z %>% 
    distinct(protein_unclean) %>% 
    mutate(protein=make_clean_names(protein_unclean)) %>% 
    inner_join(z) %>% 
    select(-protein_unclean)
}
