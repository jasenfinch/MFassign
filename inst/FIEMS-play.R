library(tidyverse)
library(MFassign)

adducts <- list(n = c("[M-H]1-", "[M+Cl]1-", "[M+K-2H]1-", 
                      "[M-2H]2-", "[M+Cl37]1-","[2M-H]1-"),
                p = c('[M+H]1+','[M+K]1+','[M+Na]1+','[M+K41]1+',
                      '[M+NH4]1+','[M+2H]2+','[2M+H]1+'))
isotopes <- c('13C','18O','13C2')


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
  split(.$Cluster) %>%
  map(~{
    e <- .
    e %>%
      split(.$Community) %>%
      map(~{
        e <- .
        relationships(e,nodes,adducts,isotopes)  
      }) %>%
      bind_rows(.id = 'Community')
  }) %>%
  bind_rows(.id = 'Cluster')
    

possibilities <- getPossibilities(communityRelationships) %>%
  Mgroups(ppm = 6)

Mclusters <- possibilities %>%
  getMgroups()

MFs <- getMFs(Mclusters)

Mclusters <- Mclusters %>%
  left_join(MFs,by = c('AdjustedM' = 'Measured M')) %>%
  filterMFs()

possibilities <- possibilities %>%
  left_join(Mclusters) %>%
  select(-Size)
