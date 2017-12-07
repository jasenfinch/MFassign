library(tidyverse)
library(igraph)
library(MFassign)

data("exampleFIEMS")

intensityMatrix <- exampleFIEMS


nodes <- intensityMatrix %>% 
  getNodes() %>%
  mutate(RTgroup = 1)

edges <- nodes %>%
  getEdges(intensityMatrix) %>%
  select(-RTgroup) %>%
  filter(r > 0)

network <- graph_from_data_frame(edges,vertices = nodes,directed = F)

network %>%
  plot(vertex.size = 2,vertex.label = NA, layout = layout_with_kk)

nodes <- nodes %>%
  mutate(Degree = degree(network,mode = 'all'),
         `Hub Score` = hub_score(network,weights = NA)$vector,
         `Authority Score` = authority_score(network,weights = NA)$vector,
         Cluster = clusters(network)$membership)

clusters <- nodes %>%
  group_by(Cluster) %>%
  summarise(Size = n())

subNetworks <- nodes %>%
  split(.$Cluster) %>%
  map(~{
    no <- .
    induced_subgraph(network,no$Feature)
  })

subNetworks[[1]] %>%
  plot(vertex.size = 2,vertex.label = NA, layout = layout_with_kk)

ceb <- cluster_fast_greedy(subNetworks[[1]],weights = E(subNetworks[[1]])$r) 

dendPlot(ceb, mode = "hclust")

plot(ceb,subNetworks[[1]],vertex.size = 4,vertex.label = NA, layout = layout_with_kk)

exampleNodes <- nodes %>%
  filter(Cluster == 1) %>%
  mutate(Community = membership(ceb))

exampleEdges <- edges %>%
  filter(Feature1 %in% exampleNodes$Feature & Feature2 %in% exampleNodes$Feature)

exampleCommunitys <- exampleNodes %>%
  group_by(Community) %>%
  summarise(Size = n())

exampleCommunityNodes <- exampleNodes %>%
  filter(Community == 4)

exampleCommunityEdges <- exampleEdges %>%
  filter(Feature1 %in% exampleCommunityNodes$Feature & Feature2 %in% exampleCommunityNodes$Feature)

communityNetwork <- graph_from_data_frame(exampleCommunityEdges,vertices = exampleCommunityNodes,directed = F)

communityNetwork %>%
  plot(vertex.size = 2,vertex.label = NA, layout = layout_with_kk)

adducts <- list(n = c("[M-H]1-", "[M+Cl]1-", "[M+K-2H]1-", 
                      "[M-2H]2-", "[M+Cl37]1-","[2M-H]1-"),
                p = c('[M+H]1+','[M+K]1+','[M+Na]1+','[M+K41]1+',
                      '[M+NH4]1+','[M+2H]2+','[2M+H]1+'))
isotopes <- c('13C','18O','13C2')

communityRelationships <- relationships(exampleCommunityEdges,exampleCommunityNodes,adducts,isotopes)

possibilities <- getPossibilities(communityRelationships) %>%
  Mgroups(ppm = 3)

Mclusters <- possibilities %>%
  group_by(Mgroup) %>%
  summarise(Size = n(),AdjustedM = mean(M) %>% round(5))
