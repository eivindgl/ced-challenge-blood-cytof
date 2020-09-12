##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author Eivind Gard Lund
##' @export
read_pre_enriched_meta <- function() {

  df <- read_xlsx(file_in('data/2008265_AC/Fil- og markøroversikt/200825 files for prediction.xlsx'))
  df %>% 
    rename(filename=`Files for prediction`, orig_meta=PRE_BL_d6) %>% 
    mutate(donor = str_extract(filename, '^\\d+'),
           donor = str_c('CD', donor),
           sample_type = 'pre',
           sample_time = case_when(str_detect(orig_meta, 'bl$') ~ 'day_0',
                                   str_detect(orig_meta, 'd6$') ~ 'day_6',
                                   TRUE ~ 'day_unknown'))
}

read_pre_enriched_fcs <- function(pre_meta, markers) {
  
  file_paths <- dir_ls('data/2008265_AC/CD4_PRE_BL_d6/', glob = '*.fcs')
  
  fcs_files <- map(file_paths, read_fcs, markers = markers)
  tibble(filename = basename(file_paths),
         fcs = fcs_files) %>% 
    left_join(pre_meta, by = 'filename')
}
