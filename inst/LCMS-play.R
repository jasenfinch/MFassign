library(tidyverse)
library(igraph)
library(MFassign)
library(mzAnnotation)

data("exampleRPLCMS")

intensityMatrix <- exampleRPLCMS

nodes <- intensityMatrix %>% 
  getNodes() %>%
  groupRT(5/60)

RTgroups <- nodes %>%
  getRTgroups()

edges <- nodes %>%
  getEdges(intensityMatrix)

exampleNodes <- nodes %>%
  filter(RTgroup == 13)

exampleEdges <- edges %>%
  filter(RTgroup == 13) %>%
  select(-RTgroup)

network <- graph_from_data_frame(exampleEdges,vertices = exampleNodes,directed = F)

network %>%
  plot(vertex.size = 2,vertex.label = NA, layout = layout_with_kk)

exampleNodes <- exampleNodes %>%
  mutate(Degree = degree(network,mode = 'all'),
         `Hub Score` = hub_score(network,weights = NA)$vector,
         `Authority Score` = authority_score(network,weights = NA)$vector,
         Cluster = clusters(network)$membership)

exampleClusters <- exampleNodes %>%
  group_by(Cluster) %>%
  summarise(Size = n())

subNetworks <- exampleNodes %>%
  split(.$Cluster) %>%
  map(~{
    no <- .
    induced_subgraph(network,no$Feature)
  })

ceb <- cluster_edge_betweenness(subNetworks[[22]],weights = E(subNetworks[[22]])$r) 

dendPlot(ceb, mode = "hclust")

plot(ceb,subNetworks[[22]],vertex.size = 4,vertex.label.cex = 0.7,vertex.label.dist = 1, layout = layout_with_kk)

exampleClusterNodes <- exampleNodes %>%
  filter(Cluster == 22) %>%
  mutate(Community = membership(ceb))

exampleClusterEdges <- exampleEdges %>%
  filter(Feature1 %in% exampleClusterNodes$Feature & Feature2 %in% exampleClusterNodes$Feature)

exampleCommunityNodes <- exampleClusterNodes %>%
  filter(Community == 4)

exampleCommunityEdges <- exampleClusterEdges %>%
  filter(Feature1 %in% exampleCommunityNodes$Feature & Feature2 %in% exampleCommunityNodes$Feature)

adducts <- list(n = c("[M-H]1-", "[M+Cl]1-", "[M+K-2H]1-", 
                      "[M-2H]2-", "[M+Cl37]1-","[2M-H]1-"),
                p = c('[M+H]1+','[M+K]1+','[M+Na]1+','[M+K41]1+',
                      '[M+NH4]1+','[M+2H]2+','[2M+H]1+'))
isotopes <- c('13C','18O','13C2')

communityRelationships <- relationships(exampleCommunityEdges,exampleCommunityNodes,adducts,isotopes)

possibilities <- getPossibilities(communityRelationships) %>%
  Mgroups()

Mclusters <- possibilities %>%
  group_by(Mgroup) %>%
  summarise(Size = n(),AdjustedM = mean(M) %>% round(5))

possibilities <- possibilities %>%
  left_join(select(Mclusters,Mgroup,AdjustedM))

MFs <- Mclusters %>%
  split(seq_len(nrow(.))) %>%
  map(~{
    mf <- .
    MFgen(mf$AdjustedM)
  }) %>% bind_rows() %>%
  rowwise() %>%
  mutate(Score = MFscore(MF)) %>%
  filter(Score < 5)

Mclusters <- Mclusters %>%
  left_join(MFs,by = c('AdjustedM' = 'Measured M'))
