library(tidyverse)
library(igraph)

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

exampleClusterNodes <- exampleEdges %>%
  filter(Cluster == 22) %>%
  mutate(Community = membership(ceb))

exampleClusterEdges <- exampleEdges %>%
  filter(Feature1 %in% exampleClusterNodes$Feature & Feature2 %in% exampleClusterNodes$Feature)

exampleCommunity <- exampleClusterNodes %>%
  filter(Community == 4)

