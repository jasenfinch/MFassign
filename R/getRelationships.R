#' @export

getRelationships <- function(edges){
  edges %>%
  split(.$Cluster) %>%
    map(~{
      e <- .
      e %>%
        split(.$Community) %>%
        map(~{
          e <- .
          relationships(e,nodes,adducts,isotopes)  
        }) %>%
        bind_rows(.id = 'Community')
    }) %>%
    bind_rows(.id = 'Cluster')
}