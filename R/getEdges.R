#' @importFrom Hmisc rcorr
#' @importFrom tibble as_tibble
#' @export

getEdges <- function(intensityMatrix){
  edges <- intensityMatrix %>%
    as.matrix()
  edges[edges == 0] <- NA
  suppressWarnings({edges <- edges %>% 
    rcorr(type = correlationsMethod)})
  edges$P <- edges$P %>%
    apply(1,p.adjust,method = adjustmentMethod)
  edges$r[edges$P > correlationPvalue] <- 0
  edges <- edges$r
  edges[lower.tri(edges)] <- NA
  edges <- edges %>%
    as_tibble() %>%
    mutate(Feature1 = colnames(edges)) %>%
    gather('Feature2','r',-Feature1) %>%
    filter(Feature1 != Feature2, !is.na(r), r != 0)
  return(edges)
}