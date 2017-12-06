#' @export

getRTgroups <- function(nodes){
  nodes %>%
    group_by(RTgroup) %>%
    summarise(Size = n(),
              Center = mean(RT),
              minRT = min(RT),
              maxRT = max(RT),
              rangeRT = maxRT - minRT)
}
