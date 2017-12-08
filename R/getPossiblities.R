#' @importFrom mzAnnotation calcM
#' @export

getPossibilities <- function(relationships){
  relationships %>% 
    split(.$Cluster) %>%
    map(~{
      r <- .
      r %>%
        split(.$Community) %>%
        map(~{
          rel <- .
          bind_rows(
            tibble(`m/z` = rel$`m/z1`,
                   Adduct = rel$Adduct1,
                   Isotope = rel$Isotope1,
                   Transformation = rel$Transformation1
            ),
            tibble(`m/z` = rel$`m/z2`,
                   Adduct = rel$Adduct2,
                   Isotope = rel$Isotope2,
                   Transformation = rel$Transformation2
            )) %>%
            distinct() %>%
            rowwise() %>%
            mutate(M = calcM(`m/z`,Adduct,Isotope,Transformation)) %>%
            arrange(M)
        }) %>%
        bind_rows(.id = 'Community')
    }) %>%
    bind_rows(.id = 'Cluster')
}