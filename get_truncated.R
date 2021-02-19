get_truncated <- function(x, trunc.low=NA, trunc.up=NA){
  if(!is.na(trunc.low)){x[x<trunc.low] <- trunc.low}
  if(!is.na(trunc.up)){x[x>trunc.up] <- trunc.up}
  x}