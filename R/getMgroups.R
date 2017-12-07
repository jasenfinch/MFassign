#' @export

getMgroups <- function(possibilities){
  possibilities %>%
    group_by(Mgroup) %>%
    summarise(Size = n(),AdjustedM = mean(M) %>% round(5))
}