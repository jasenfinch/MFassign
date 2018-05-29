#' @export

subNetworkCommunities <- function(nodes,subNetworks){
  com <- subNetworks %>%
    map(~{
      map(.,~{
        cl <- cluster_fast_greedy(.,weights = E(.)$r) %>%
          membership()
        cl <- tibble(Feature = names(cl),Community = cl %>% as.numeric())
        return(cl)  
      }) %>% 
        bind_rows()
    }) %>%
    bind_rows()
  nodes %>%
    left_join(com,by = "Feature") %>%
    arrange(RTgroup,Cluster,Community)
} 