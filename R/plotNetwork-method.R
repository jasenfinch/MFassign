#' plotNetwork-Assignment
#' @rdname plotNetwork
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph vertex_attr set_vertex_attr
#' @importFrom ggraph ggraph geom_edge_link geom_node_point theme_graph geom_node_text
#' @importFrom ggthemes scale_colour_ptol
#' @importFrom ggplot2 labs aes
#' @export

setMethod('plotNetwork',signature = 'Assignment',
          function(assignment, layout = 'kk', rThreshold = 0.7, labels = F){
            
            network <- assignment@correlations %>%
              filter(r > rThreshold) %>%
              as_tbl_graph(directed = F)
            
            nodes <- vertex_attr(network) %>% 
              as_tibble() %>%
              mutate(Mode = str_sub(name,1,1))
            
            network <- set_vertex_attr(network,'Mode',value = nodes$Mode)
            
            
            pl <- ggraph(network,layout = layout) +
              geom_edge_link(alpha = 0.1) +
              geom_node_point(aes(colour = Mode),alpha = 1) +
              scale_colour_ptol() +
              theme_graph(base_family = 'sans') +
              labs(title = str_c('Assignment correlation Network (r > ',rThreshold,')'))
            
            if (labels) {
              pl + geom_node_text(aes(label = name),repel = T)
            }
            
            return(pl)
          })