#' @importFrom dplyr rename

addMFs <- function(rel,MF){
  MFs <- select(MF,MF,Isotope,Adduct,`Measured m/z`,)
  rel <- filter(rel, `m/z1` %in% MF$`Measured m/z`,`m/z2` %in% MF$`Measured m/z`) %>%
    left_join(MFs,by = c('m/z1' = 'Measured m/z')) %>%
    rename(MF1 = MF)
  rel[is.na(rel)] <- ''
  rel <- filter(rel,Isotope1 == Isotope & Adduct1 == Adduct) %>%
    select(`m/z1`:MF1) %>%
    left_join(MFs,by = c('m/z2' = 'Measured m/z')) %>%
    rename(MF2 = MF)
  rel[is.na(rel)] <- ''
  rel <- filter(rel,Isotope2 == Isotope & Adduct2 == Adduct) %>%
    filter(MF1 == MF2) %>%
    select(`m/z1`:Isotope2,Adduct1:MF1) %>%
    rename(MF = MF1)
  rel[rel == ''] <- NA
  return(rel)
}