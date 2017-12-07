#' @importFrom igraph graph_from_data_frame
#' @export

buildNetwork <- function(nodes,edges){
  graph_from_data_frame(edges,vertices = nodes,directed = F)
}