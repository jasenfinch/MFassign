#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom mzAnnotation ppmError
#' @export

Mgroups <- function(possibilities,ppm = 1){
  possibilities %>%
    split(.$Cluster) %>%
    map(~{
      p <- .
      p %>%
        split(.$Community) %>%
        map(~{
          pos <- .
          ppmMatrix <- pos$M %>%
            unique()
          
          ppmMatrix <- expand.grid(ppmMatrix,ppmMatrix) %>%
            as_tibble() %>%
            rename(M1 = Var1,M2 = Var2) %>%
            mutate(ppmError = ppmError(M1,M2) %>% abs())
          
          Ms <- pos$M %>% 
            unique() %>%
            as_tibble() %>%
            rename(M1 = value) %>%
            rowid_to_column(var = 'ID')
          
          ppmMatrix <- ppmMatrix %>%
            left_join(Ms,by = "M1") %>%
            spread(M2,ppmError) %>%
            select(-ID)
          
          ppmMatrix <- data.frame(ppmMatrix[,-1],row.names = ppmMatrix$M1,check.names = F)
          
          Mclusters <-  ppmMatrix %>%
            as.dist %>%
            hclust() %>%
            cutree(h = ppm)
          Mclusters <- tibble(M = names(Mclusters),Mgroup = Mclusters) %>%
            mutate(M = as.numeric(M))
          
          pos %>%
            left_join(Mclusters,by = "M")
        }) %>%
        bind_rows(.id = 'Community')
    }) %>%
    bind_rows() %>%
    select(Cluster,Community,`m/z`:Mgroup)
}
