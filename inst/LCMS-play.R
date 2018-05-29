library(tidyverse)
library(MFassign)

adducts <- list(n = c("[M-H]1-", "[M+Cl]1-", "[M+K-2H]1-", 
                      "[M-2H]2-", "[M+Cl37]1-","[2M-H]1-"),
                p = c('[M+H]1+','[M+K]1+','[M+Na]1+','[M+K41]1+',
                      '[M+NH4]1+','[M+2H]2+','[2M+H]1+'))
isotopes <- c('13C','18O','13C2')
MgroupPPM <- 6
MFgenPPM <- 6
MFscoreThreshold <- 5
nCores <- detectCores()
clusterType <- 'FORK'

data("exampleRPLCMS")

intensityMatrix <- exampleRPLCMS

nodes <- intensityMatrix %>% 
  getNodes() %>%
  groupRT(5/60)

RTgroups <- nodes %>%
  getRTgroups()

edges <- nodes %>%
  getEdges(intensityMatrix) %>%
  filter(r > 0)

network <- buildNetwork(nodes,edges)

nodes <- networkClusters(nodes,network)

subNetworks <- getSubNetworks(network,nodes)

nodes <- subNetworkCommunities(nodes,subNetworks) 

communitiesSummary <- nodes %>%
  group_by(RTgroup,Cluster,Community) %>%
  summarise(Size = n())

communityEdges <- getCommunityEdges(edges,nodes)

communityRelationships <- communityEdges %>%
  getRelationships()

possibilities <- getPossibilities(communityRelationships) %>%
  Mgroups(ppm = MgroupPPM)

Mclusters <- possibilities %>%
  getMgroups()

MFs <- getMFs(Mclusters,ppm = MFgenPPM,MFscoreThreshold = MFscoreThreshold,nCores = nCores,clusterType = clusterType)

Mclusters <- Mclusters %>%
  left_join(MFs,by = c('AdjustedM' = 'Measured M')) %>%
  filterMFs()

possibilities <- possibilities %>%
  left_join(Mclusters) %>%
  select(-Size)
