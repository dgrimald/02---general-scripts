checking_balance <- function(data, var.list, group){
  
  # group="fcp.d"
  # var.list=c("cash.flow", "ind.sales.growth")

  # load packages
  require(dplyr)
  require(tidyr)
  
  # preparing general references
  ref_group <- match(group, names(data))
  # data$group.d <- as.numeric(data[,ref_group]==group.value)
  data$group.d <- data[,ref_group]
  var.ref <- match(c("group.d", var.list), names(data))
  data <- data[,var.ref]
  
  # contagem de tratados e controles
  
  # functions to apply to each variable
  count_n <- function(x){sum(!is.na(x))}
  mean_n <- function(x){mean(x, na.rm=TRUE)}
  median_n <- function(x){median(x, na.rm=TRUE)}
  sd_n <- function(x){sd(x, na.rm=TRUE)}
  quant_low <- function(x){quantile(x, probs=c(0.025), na.rm=TRUE, type=1)}
  quant_high <- function(x){quantile(x, probs=c(0.975), na.rm=TRUE, type=1)}
  
  # calculate basic statistics
  stat <- data %>%
    filter(!is.na(group.d)) %>% 
    group_by(group.d) %>%
    summarise_all(list(n.obs=count_n, mean=mean_n, median=median_n, sd=sd_n, q.low=quant_low, q.high=quant_high)) %>%
    pivot_longer(cols=-group.d, names_to="variable", values_to="value") %>%
    pivot_wider(names_from=group.d, values_from=value) %>% 
    as.data.frame()
  
  mean_diff <- stat[grep("_mean", stat$variable),-1]-stat[grep("_mean", stat$variable),rep(2,ncol(stat)-1)]
  sd_obs <- stat[grep("_sd", stat$variable), -1]^2/stat[grep("_n.obs", stat$variable), -1]
  sd_obs <- sqrt(sd_obs + sd_obs[,rep(1,ncol(stat)-1)])
  sd_dist <- stat[grep("_sd", stat$variable), -1]^2
  sd_dist <- sqrt(sd_dist + sd_dist[,rep(1,ncol(stat)-1)])
  t_stat <- mean_diff / sd_obs
  t_stat$variable <- paste(names(data)[-1], "_t", sep="")
  norm_diff <- mean_diff / sd_dist
  norm_diff$variable <- paste(names(data)[-1], "_Norm.Diff", sep="")
  log_ratio <- log(stat[grep("_sd", stat$variable), -1])-log(stat[grep("_sd", stat$variable), rep(2,ncol(stat)-1)])
  log_ratio$variable <- paste(names(data)[-1], "_log.ratio.sd", sep="")
  
  balance <- bind_rows(stat, t_stat, norm_diff, log_ratio)
  
  # find counterfactual outliers (group 1 is reference)
  q.low_range <- balance[grep("_q.low", balance$variable),2]
  q.high_range <- balance[grep("_q.high", balance$variable),2]
  
  outlier.data <- filter(data, !is.na(group.d))
  for(i in 2:ncol(outlier.data)){
    outlier.data[,i] <- pmax(as.numeric(outlier.data[,i]<q.low_range[i-1]),
                                       as.numeric(outlier.data[,i]>q.high_range[i-1]))}
  
  relative_sum <- function(x){sum(x, na.rm=T)/sum(!is.na(x))}
  outlier.data <- group_by(outlier.data, group.d) %>% 
    summarise_all(relative_sum) %>%
    pivot_longer(cols=-group.d, names_to="variable", values_to="value") %>%
    pivot_wider(names_from=group.d, values_from=value) %>% 
    as.data.frame()
  outlier.data$variable <- paste(outlier.data$variable, "_p5.ref", sep="")
  balance <- bind_rows(balance, outlier.data)
  
  table_collection <- list()
  stats <- c("_n.obs", "_mean", "_median", "_sd", "_t", "_Norm.Diff", "_log.ratio.sd", "_p5.ref")
  count=1
  for (i in names(balance)[-1]){
    table_i <- select(balance, variable, i)
    ref <- sapply(stats, grep, table_i$variable, simplify = F)
    data.temp <- data.frame(variables=var.list)
    data.temp$n.obs <- table_i[ref$`_n.obs`, 2]
    data.temp$mean <- table_i[ref$`_mean`, 2]
    data.temp$median <- table_i[ref$`_median`, 2]
    data.temp$t <- table_i[ref$`_t`, 2]
    data.temp$norm.diff <- table_i[ref$`_Norm.Diff`, 2]
    data.temp$sd <- table_i[ref$`_sd`, 2]
    data.temp$log.ratio <- table_i[ref$`_log.ratio.sd`, 2]
    data.temp$p5.ref <- table_i[ref$`_p5.ref`, 2]
    table_collection[[count]] <- data.temp
    count=count+1}
  names(table_collection) <- names(balance)[-1]
  
  return(balance.data = table_collection)}