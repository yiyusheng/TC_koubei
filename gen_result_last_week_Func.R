#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: gen_result_last_week.R
#
# Description: Functions of generating result from last week
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-24 09:37:29
#
# Last   modified: 2017-01-24 09:37:31
#
#
#

# F1.extract data from shop_pay, recognize the missing data and replace them with efficient data
extract_data <- function(shop_pay,k,test_start,teset_end){
  data_pred <- subset(shop_pay,uni_time >= (test_start - 86400*k) & uni_time < test_start)
  
  broken_shop <- dcast(shop_id~uni_time,data = data_pred,value.var = 'value')
  broken_shop$num_na <- apply(broken_shop,1,function(x)sum(is.na(x)))
  
  missing_shop <- c(broken_shop$shop_id[broken_shop$num_na >= 3],
                    setdiff(unique(shop_pay$shop_id),unique(data_pred$shop_id)))
  
  weekday_end <- as.POSIXlt(test_end - 86400)$wday
  missing_data <- lapply(missing_shop,function(i){
    shop_data <- subset(shop_pay,shop_id == i & uni_time < (test_start - 86400*k))
    last_day <- max(shop_data$uni_time[as.POSIXlt(shop_data$uni_time)$wday == weekday_end]) + 86400
    md <- subset(shop_data,uni_time < last_day & uni_time >= (last_day - 86400*k))
    md$uni_time <- md$uni_time + difftime(test_start,last_day,units = 'secs')
    md
  })
  missing_data <- do.call(rbind,missing_data)
  data_pred <- rbind(subset(data_pred,!(shop_id %in% missing_shop)),missing_data)
}

# F2. fill missing data
source('fill_na.R')
fill_missing_data <- function(data_pred,k){
  k1 <- k+1
  data_pred_dcast <- dcast(shop_id~uni_time,data = data_pred,value.var = 'value')
  data_pred_dcast$num_na <- apply(data_pred_dcast[,2:k1],1,function(x)sum(is.na(x)))
  data_pred_dcast$mean <- apply(data_pred_dcast[,2:k1],1,mean,na.rm = T)
  data_pred_dcast$sd <- apply(data_pred_dcast[,2:k1],1,sd,na.rm = T)
  data_pred_dcast$sdrate <- data_pred_dcast$sd/data_pred_dcast$mean
  
  data_pred_dcast <- fill_with_mean_all(data_pred_dcast,k)
  # data_pred_dcast <- fill_with_mannual(data_pred_dcast)
  data_pred_dcast[,1:k1] <- round(data_pred_dcast[,1:k1])
  
  
  data_pred_melt <- melt(data_pred_dcast[,1:k1],id.vars = 'shop_id',variable.name = 'uni_time')
  data_pred_melt$uni_time <- as.p(data_pred_melt$uni_time)
  
  list(data_pred_melt,data_pred_dcast)
}

# F3A. volt limit for each shop to set the value larger or less to the limit
volt_limitA <- function(data_pred_dcast,k,rate){
  for(i in 1:nrow(data_pred_dcast)){
    ori_value <- as.numeric(data_pred_dcast[i,2:(k+1)])
    limit_min <- mean(ori_value) - rate*sd(ori_value)
    limit_max <- mean(ori_value) + rate*sd(ori_value)
    ori_value[ori_value > limit_max] <- limit_max
    ori_value[ori_value < limit_min] <- limit_min
    data_pred_dcast[i,2:(k+1)] <- ori_value
  }
  data_pred <- melt(data_pred_dcast[,1:(k+1)],id.vars = 'shop_id')
  names(data_pred) <- c('shop_id','uni_time','value')
  data_pred$uni_time <- as.p(data_pred$uni_time)
  data_pred
}

# F3B. volt limit for each shop to compress all value
linMap <- function(x, from, to){
  (x - mean(x)) / ((max(x) - min(x))/2) * ((to - from)/2) + mean(x)
}
  
volt_limitB <- function(data_pred_dcast,k,rate){
  for(i in 1:nrow(data_pred_dcast)){
    ori_value <- as.numeric(data_pred_dcast[i,2:(k+1)])
    limit_min <- max(1,mean(ori_value) - rate*sd(ori_value))
    limit_max <- mean(ori_value) + rate*sd(ori_value)
    if(any(ori_value > limit_max) | any(ori_value < limit_min)){
      data_pred_dcast[i,2:(k+1)] <- linMap(ori_value,limit_min,limit_max)
    }
  }
  data_pred <- melt(data_pred_dcast[,1:(k+1)],id.vars = 'shop_id')
  names(data_pred) <- c('shop_id','uni_time','value')
  data_pred$uni_time <- as.p(data_pred$uni_time)
  data_pred
}

# F4. expand data to two weeks
expand_data <- function(data_pred,k){
  rp <- ceiling(14/k)
  data_pred_expand <- lapply(seq_len(rp),function(i){
    data_pred$uni_time <- data_pred$uni_time + 86400 * k * i
    data_pred
  })
  data_pred_expand <- do.call(rbind,data_pred_expand)
}

# F5. Add real data
add_real <- function(data_pred,test_start,teset_end){
  data_pred <- subset(data_pred,uni_time >= test_start & uni_time < test_end)
  data_real <- subset(shop_pay,uni_time >= test_start & uni_time < test_end)
  data_comp <- merge(data_real,data_pred,by = c('shop_id','uni_time'))
  names(data_comp) <- c('shop_id','uni_time','value','pred')
  data_comp$ms <- abs(data_comp$value - data_comp$pred)/(data_comp$value + data_comp$pred)
  data_comp
}

# F6.Check measure of each shop,categary and etc.
check_result <- function(out){
  out$shop_id <- factor(out$shop_id)
  # C1. aggresive of measure of each shop: mean,sd,max,min,max_day,min_day
  aggr_ms <- data.frame(shop_id = levels(out$shop_id),
                        mean = as.numeric(tapply(out$ms,out$shop_id,mean)),
                        sd = as.numeric(tapply(out$ms,out$shop_id,sd)),
                        max = as.numeric(tapply(out$ms,out$shop_id,max)),
                        median = as.numeric(tapply(out$ms,out$shop_id,median)),
                        min = as.numeric(tapply(out$ms,out$shop_id,min)),
                        max_d = as.numeric(tapply(out$ms,out$shop_id,which.max)),
                        min_d = as.numeric(tapply(out$ms,out$shop_id,which.min)))
  
  # C2. Plot and return
  p1 <- ggplot(aggr_ms,aes(x = mean)) + geom_histogram(bins = 100) + ggtitle('mean')
  p2 <- ggplot(aggr_ms,aes(x = sd)) + geom_histogram(bins = 100) + ggtitle('sd')
  p3 <- ggplot(aggr_ms,aes(x = mean,y = sd)) + geom_point() + ggtitle('mean-sd')
  p4 <- ggplot(aggr_ms,aes(x = max)) + geom_histogram(bins = 100) + ggtitle('max')
  p5 <- ggplot(aggr_ms,aes(x = median)) + geom_histogram(bins = 100) + ggtitle('median')
  p6 <- ggplot(aggr_ms,aes(x = min)) + geom_histogram(bins = 100) + ggtitle('min')
  p7 <- ggplot(aggr_ms,aes(x = max_d)) + geom_histogram(bins = 100) + ggtitle('max_d')
  p8 <- ggplot(aggr_ms,aes(x = min_d)) + geom_histogram(bins = 100) + ggtitle('min_d')
  # multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols = 4)
  
  # return(list(aggr_ms,list(p1,p2,p3,p4,p5,p6,p7,p8),out))
  return(aggr_ms)
}

# F7. generate result
gen_csv <- function(data_pred,title){
  r7 <- dcast(shop_id~uni_time,data = data_pred,value.var = 'value')
  names(r7) <- c('shop_id',paste('day_',1:14,sep=''))
  r7 <- r7[order(r7$shop_id),]
  write.table(r7,file = file.path(dir_data,'result',title),quote = F,sep=',',row.names = F,col.names = F)
  return(r7)
}
