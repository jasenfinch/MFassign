library(tidyverse)
library(Hmisc)
library(igraph)

correlationsMethod <- 'pearson'
adjustmentMethod <- 'bonferroni'
correlationPvalue <- 0.001

data("exampleRPLCMS")

intensityMatrix <- exampleRPLCMS

nodes <- intensityMatrix %>% 
  getNodes() %>%
  groupRT(5/60)

RTgroups <- nodes %>%
  getRTgroups()


edges <- nodes %>%
  split(.$RTgroup) %>%
  map(~{
    g <- .
    getEdges(intensityMatrix %>% select(g$Feature))
  }) %>%
  bind_rows(.id = 'RTgroup') %>%
  mutate(RTgroup = RTgroup %>% as.numeric())
  


