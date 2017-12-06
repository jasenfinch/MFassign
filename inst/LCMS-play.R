library(tidyverse)
library(Hmisc)
library(igraph)

correlationsMethod <- 'pearson'
adjustmentMethod <- 'bonferroni'
correlationPvalue <- 0.001

data("exampleRPLCMS")

intensityMatrix <- exampleRPLCMS

nodes <- getNodes(intensityMatrix)

edges <- getEdges(intensityMatrix)


