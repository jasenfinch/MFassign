#' @importFrom igraph graph_from_data_frame
#' @export

buildNetwork <- function(nodes,edges){
  nodes %>%
    split(.$RTgroup) %>%
    map(~{
      no <- .
      e <- edges %>%
        filter(RTgroup == no$RTgroup[1])
      graph_from_data_frame(e,vertices = no,directed = F) 
    }) 
}