library(tidyverse)
library(Hmisc)
library(igraph)

options(digits = 10)

correlationsMethod <- 'pearson'
adjustmentMethod <- 'bonferroni'
correlationPvalue <- 0.001

data("exampleFIEMS")

intensityMatrix <- exampleFIEMS

nodes <- intensityMatrix %>%
  gather('Feature','Intensity') %>%
  group_by(Feature) %>%
  summarise(Intensity = mean(Intensity)) %>%
  mutate(Mode = str_sub(Feature,1,1),`m/z` = str_replace_all(Feature,'[:alpha:]','') %>% as.numeric()) %>%
  select(Feature,Mode,`m/z`,Intensity)

edges <- intensityMatrix %>%
  as.matrix()
edges[edges == 0] <- NA
edges <- edges %>% 
  rcorr(type = correlationsMethod)
edges$P <- edges$P %>%
  apply(1,p.adjust,method = adjustmentMethod)
edges$r[edges$P > correlationPvalue] <- 0
edges <- edges$r
edges[lower.tri(edges)] <- NA
edges <- edges %>%
  as_tibble() %>%
  mutate(Feature1 = colnames(edges)) %>%
  gather('Feature2','r',-Feature1) %>%
  filter(Feature1 != Feature2, !is.na(r), r > 0.8)

network <- graph_from_data_frame(edges,vertices = nodes,directed = F)

nodes <- nodes %>%
  mutate(Degree = degree(network,mode = 'all'),
         `Hub Score` = hub_score(network,weights = NA)$vector,
         `Authority Score` = authority_score(network,weights = NA)$vector)

network %>%
  plot(vertex.size = 2,vertex.label = NA, layout = layout_with_kk)


networkCliques <- cliques(network)