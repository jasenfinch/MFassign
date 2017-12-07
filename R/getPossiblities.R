#' @importFrom mzAnnotation calcM
#' @export

getPossibilities <- function(relationships){
  bind_rows(
    tibble(`m/z` = relationships$`m/z1`,
           Adduct = relationships$Adduct1,
           Isotope = relationships$Isotope1,
           Transformation = relationships$Transformation1
    ),
    tibble(`m/z` = relationships$`m/z2`,
           Adduct = relationships$Adduct2,
           Isotope = relationships$Isotope2,
           Transformation = relationships$Transformation2
    )) %>%
    distinct() %>%
    rowwise() %>%
    mutate(M = calcM(`m/z`,Adduct,Isotope,Transformation)) %>%
    arrange(M)
}