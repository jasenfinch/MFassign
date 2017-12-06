#' @export

getEdges <- function(nodes,intensityMatrix){
  nodes %>%
    split(.$RTgroup) %>%
    map(~{
      g <- .
      correlate(intensityMatrix %>% select(g$Feature))
    }) %>%
    bind_rows(.id = 'RTgroup') %>%
    mutate(RTgroup = RTgroup %>% as.numeric())
}