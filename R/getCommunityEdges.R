#' @export

getCommunityEdges <- function(edges,nodes){
  nodes %>%
    split(.$Cluster) %>%
    map(~{
      n <- .
      n %>%
        split(.$Community) %>%
        map(~{
          no <- .
          edges %>%
            filter(Feature1 %in% no$Feature & Feature2 %in% no$Feature)
        }) %>%
        bind_rows(.id = 'Community')
    }) %>%
    bind_rows(.id = 'Cluster') %>%
    select(Feature1:r,Cluster,Community)
}