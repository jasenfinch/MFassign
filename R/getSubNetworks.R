#' @importFrom igraph induced_subgraph
#' @export

getSubNetworks <- function(network,nodes){
  nodes %>%
    split(.$RTgroup) %>%
    map(~{
      no <- .
      no %>%
      split(.$Cluster) %>%
        map(~{
          n <- .
          induced_subgraph(network[[n$RTgroup[1]]],n$Feature)
        })
    })
    
}