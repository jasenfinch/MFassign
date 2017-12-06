#' @export

featureRT <- function(feature){
  str_split_fixed(feature,coll('@'),2)[1,2] %>%
    as.numeric()
}
