#' @importFrom parallel makeCluster detectCores parLapply stopCluster
#' @importFrom mzAnnotation relationshipPredictor
#' @export

relationships <- function(edges, nodes, adducts, isotopes, nCores = detectCores(), clusterType = 'FORK'){
  clus <- makeCluster(nCores,type = clusterType)
  rel <- edges %>%
    split(seq_len(nrow(.))) %>%
    parLapply(cl = clus,function(edges,nodes,adducts,isotopes){
      mz <- nodes %>%
        filter(Feature %in% edges$Feature1 | Feature %in% edges$Feature2)
      relationshipPredictor(mz$`m/z`,adducts = adducts,isotopes = isotopes,modes = mz$Mode) %>%
        as_tibble()
    },nodes = nodes,adducts = adducts,isotopes = isotopes) %>%
    bind_rows()
  stopCluster(clus)
  return(rel)
}
