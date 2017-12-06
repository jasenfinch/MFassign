#' @importFrom stringr str_sub
#' @export

featureMode <- function(feature){
  str_sub(feature,1,1)
}