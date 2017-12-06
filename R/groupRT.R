#' @export

groupRT <- function(nodes,RTwindow){
  groups <- data.frame(RetentionTime = nodes$RT,row.names = nodes$Feature) %>%
    dist() %>%
    hclust() %>%
    cutree(h = RTwindow) %>%
    as_tibble()
  nodes %>%
    bind_cols(groups) %>%
    rename(RTgroup = value)
}