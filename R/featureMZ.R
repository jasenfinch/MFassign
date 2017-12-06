#' @export

featureMZ <- function(feature){
  str_split_fixed(feature,coll('@'),2)[1,1] %>%
    str_replace('[:alpha:]','') %>%
    as.numeric()
}