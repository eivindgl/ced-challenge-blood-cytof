##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' 

read_fcs <- function(fcs_path, markers, cofactor = 5) {
  z <- read.FCS(fcs_path, 
                transformation = FALSE,
                truncate_max_range = FALSE)
  # metal_map <- 
  p <- flowCore::parameters(z) 
  name_map <- tibble(flow_name = p$name,
                     metal_and_protein = toupper(p$desc)) %>% 
    left_join(markers, by = 'metal_and_protein') %>% 
    filter(!is.na(protein))
  
  are_equal(nrow(distinct(name_map, protein)), 
            nrow(distinct(markers,protein)))
  df <- asinh(exprs(z)[, name_map$flow_name, drop = F] / cofactor) %>% 
    as_tibble() %>% 
    set_names(name_map$protein)
}

read_day6_fcs <- function(day6_meta, markers) {
  
  file_paths <- dir_ls('data/2008265_AC/CD4_d6_EN_TetPos_Neg/', glob = '*.fcs')
  
  fcs_files <- map(file_paths, read_fcs, markers = markers)
  tibble(filename = basename(file_paths),
         fcs = fcs_files) %>% 
    left_join(day6_meta, by = 'filename')
}
