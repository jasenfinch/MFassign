#' @export

getMFs <- function(Mgroups, MFscoreThreshold = 5, nCores = detectCores(), clusterType = 'FORK'){
  clus <- makeCluster(nCores,type = clusterType)
  mf <- Mgroups %>%
    split(seq_len(nrow(.))) %>%
    parLapply(cl = clus,function(mf){
      MFgen(mf$AdjustedM)
    }) %>% bind_rows() %>%
    rowwise() %>%
    mutate(Score = MFscore(MF)) %>%
    filter(Score < MFscoreThreshold)
  stopCluster(clus)
  return(mf)
}