#' @importFrom igraph induced_subgraph
#' @export

getSubNetworks <- function(network,nodes){
  nodes %>%
    split(.$Cluster) %>%
    map(~{
      no <- .
      induced_subgraph(network,no$Feature)
    })
}