#' @importFrom mzAnnotation ppmError
#' @export

Mgroups <- function(possibilities,ppm = 1){
  ppmMatrix <- possibilities$M %>%
    unique()
  
  ppmMatrix <- expand.grid(ppmMatrix,ppmMatrix) %>%
    as_tibble() %>%
    rename(M1 = Var1,M2 = Var2) %>%
    mutate(ppmError = ppmError(M1,M2) %>% abs())
  
  Ms <- possibilities$M %>% 
    unique() %>%
    as_tibble() %>%
    rename(M1 = value) %>%
    rowid_to_column(var = 'ID')
  
  ppmMatrix <- ppmMatrix %>%
    left_join(Ms) %>%
    spread(M2,ppmError) %>%
    select(-ID)
  
  ppmMatrix <- data.frame(ppmMatrix[,-1],row.names = ppmMatrix$M1,check.names = F)
  
  Mclusters <-  ppmMatrix %>%
    as.dist %>%
    hclust() %>%
    cutree(h = ppm)
  Mclusters <- tibble(M = names(Mclusters),Mgroup = Mclusters) %>%
    mutate(M = as.numeric(M))
  
  possibilities <- possibilities %>%
    left_join(Mclusters)
  return(possibilities)
}
