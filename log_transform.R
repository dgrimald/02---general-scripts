log_transform  <- function(x){
  sign.x <- ifelse(x<0, -1, +1)
  abs.x <- abs(x)
  ifelse(is.na(sign.x*log(abs(x))), NA, sign.x*log(abs(x)))
}

log_transform2  <- function(x){
  sign.x <- ifelse(x<0, -1, +1)
  abs.x <- abs(x)
  ifelse(is.na(sign.x*log(abs(x)+1)), NA, sign.x*log(abs(x)+1))
} 