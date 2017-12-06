#' @export

getNodes <- function(intensityMatrix){
  intensityMatrix %>%
    gather('Feature','Intensity') %>%
    group_by(Feature) %>%
    summarise(Intensity = mean(Intensity)) %>%
    rowwise() %>%
    mutate(Mode = featureMode(Feature),
           `m/z` = featureMZ(Feature),
           RT = featureRT(Feature)) %>%
    select(Feature,Mode,`m/z`,RT,Intensity)
}