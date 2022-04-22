boutFinder <- function (idx) {
  ## Identify consecutive indices
  counter <- 1 - (as.numeric(c(NA, diff(idx) == 1)))
  counter <- ifelse(is.na(counter), 0, counter)
  bouts <- cumsum(counter)
  
  ## Calculate bout lengths
  boutlengths <- rle(bouts)$lengths
  rep(1:length(boutlengths), boutlengths)
  
}
