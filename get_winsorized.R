get_winsorized <- function(x, p.tails, group="none", InftoNA=T){
  if(group=="none"){
    tails <- quantile(x, probs=c(p.tails, 1-p.tails), na.rm=TRUE)
    x[x<tails[1]] <- tails[1]
    x[x>tails[2]] <- tails[2]
  }else{
    list.group <- unique(group)
    for(i in list.group){
      tails <- quantile(x[group %in% i], probs=c(p.tails, 1-p.tails), na.rm=TRUE)
      x[x<tails[1] & group==i] <- tails[1]
      x[x>tails[2] & group==i] <- tails[2]
    }
  }
  if(InftoNA){x[is.infinite(x)] <- NA}
  x}