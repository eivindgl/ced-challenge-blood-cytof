##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author Eivind Gard Lund
##' @export
read_day6_meta <- function() {

  df <- read_xlsx(file_in('data/2008265_AC/Fil- og markøroversikt/200825 files for ranking.xlsx'))
  df %>% 
    rename(filename=`Files for random forest`, orig_meta=d6_EN_Tet_Pos_Neg) %>% 
    mutate(donor = str_extract(filename, '^\\d+'),
           donor = str_c('CD', donor),
           sample_type = str_extract(orig_meta, 'tet\\w+$'),
           sample_type = fct_relevel(sample_type, 'tetpos'),
           sample_time = 'day_6')
}
