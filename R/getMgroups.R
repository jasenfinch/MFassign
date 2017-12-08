#' @export

getMgroups <- function(possibilities){
  possibilities %>%
    split(.$Cluster) %>%
    map(~{
      p <- .
      p %>%
        split(.$Community) %>%
        map(~{
          pos <- .
          pos %>%
            group_by(Mgroup) %>%
            summarise(Size = n(),AdjustedM = mean(M) %>% round(5))
        }) %>%
        bind_rows(.id = 'Community')
    }) %>% bind_rows(.id = 'Cluster')
}