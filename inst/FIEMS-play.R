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

data("exampleFIEMS")

intensityMatrix <- exampleFIEMS


nodes <- intensityMatrix %>% 
  getNodes() %>%
  mutate(RTgroup = 1)

edges <- nodes %>%
  getEdges(intensityMatrix) %>%
  select(-RTgroup) %>%
  filter(r > 0)

network <- buildNetwork(nodes,edges)

# network %>%
#   plot(vertex.size = 2,vertex.label = NA, layout = layout_with_kk)

nodes <- networkClusters(nodes,network)

clusters <- nodes %>%
  group_by(Cluster) %>%
  summarise(Size = n())

subNetworks <- getSubNetworks(network,nodes)

# subNetworks[[1]] %>%
#   plot(vertex.size = 2,vertex.label = NA, layout = layout_with_kk)

nodes <- subNetworkCommunities(nodes,subNetworks) 

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
