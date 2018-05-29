#' @export

getCommunityEdges <- function(edges,nodes){
  nodes %>%
    split(.$RTgroup) %>%
    map(~{
      no <- .
      no %>%
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
        bind_rows(.id = 'Cluster')
    }) %>%
    bind_rows() %>%
    select(Feature1:r,RTgroup,Cluster,Community)
}