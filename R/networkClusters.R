#' @importFrom igraph clusters
#' @export

networkClusters <- function(nodes,network){
  nodes %>%
    mutate(Degree = degree(network,mode = 'all'),
           `Hub Score` = hub_score(network,weights = NA)$vector,
           `Authority Score` = authority_score(network,weights = NA)$vector,
           Cluster = clusters(network)$membership)
}