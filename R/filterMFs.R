#' @export

filterMFs <- function(MFs){
  MFs %>%
  split(.$Mgroup) %>%
    map(~{
      m <- .
      if (nrow(m) > 1) {
        m <- m %>%
          filter(Score == min(Score))
      }
      
      if (nrow(m) > 1) {
        m <- m %>%
          filter(ppmError == min(ppmError))
      }
      return(m)
    }) %>%
    bind_rows()
}