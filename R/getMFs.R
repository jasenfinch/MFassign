#' @importFrom parallel parLapplyLB
#' @export

getMFs <- function(Mgroups, ppm = 6, MFscoreThreshold = 5, nCores = detectCores(), clusterType = 'FORK'){
  clus <- makeCluster(nCores,type = clusterType)
  mf <- Mgroups %>%
    split(.$Cluster) %>%
    map(~{
      p <- .
      p %>%
        split(.$Community) %>%
        map(~{
          m <- .
          m %>%
          sample_n(nrow(.)) %>%
            split(seq_len(nrow(.))) %>%
            parLapplyLB(cl = clus,function(mf,ppm){
              MFgen(mf$AdjustedM,ppm = ppm)
            },ppm = ppm) %>% 
            bind_rows() %>%
            rowwise() %>%
            mutate(Score = MFscore(MF)) %>%
            filter(Score < MFscoreThreshold)
        }) %>% 
        bind_rows(.id = 'Community')
    }) %>%
    bind_rows(.id = 'Cluster')
  stopCluster(clus)
  return(mf)
}