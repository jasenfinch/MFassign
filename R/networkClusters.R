#' @importFrom igraph clusters
#' @export

networkClusters <- function(nodes,network){
  map(1:length(network),~{
    n <- .
    net <- network[[n]]
    nodes %>%
      filter(RTgroup == n) %>%
      mutate(Degree = degree(net,mode = 'all'),
             Cluster = clusters(net)$membership)
  }) %>%
    bind_rows()
}