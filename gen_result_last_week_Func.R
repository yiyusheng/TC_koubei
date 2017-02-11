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
  
  data_na <- is.na(data_pred_dcast)
  data_pred_dcast <- fill_with_mean_all(data_pred_dcast,k)
  # data_pred_dcast <- fill_with_mannual(data_pred_dcast)
  data_pred_dcast[,1:k1] <- round(data_pred_dcast[,1:k1])
  
  
  data_pred_melt <- melt(data_pred_dcast[,1:k1],id.vars = 'shop_id',variable.name = 'uni_time')
  data_pred_melt$uni_time <- as.p(data_pred_melt$uni_time)
  
  list(data_pred_melt,data_pred_dcast,data_na)
}

# F3A. volt limit for each shop to set the value larger or less to the limit
smp_tuningA <- function(data_pred_dcast,k,rate){
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
  
smp_tuningB <- function(data_pred_dcast,data_na,k,volt_limit_weight,volt_limit_set = NULL){
  for(i in 1:nrow(data_pred_dcast)){
    ori_value <- as.numeric(data_pred_dcast[i,2:(k+1)])
    meanD <- mean(ori_value[!data_na[i,2:(k+1)]])
    sdD <- sd(ori_value[!data_na[i,2:(k+1)]])
    if(is.na(sdD))next
    
    if(!is.null(volt_limit_set)){
      limit_min <- max(1,meanD - volt_limit_set[i]*sdD)
      limit_max <- meanD + volt_limit_set[i]*sdD
    }else{
      limit_min <- max(1,meanD - volt_limit_weight*sdD)
      limit_max <- meanD + volt_limit_weight*sdD
    }
    # cat(sprintf('%d\n',i))
    if(any(ori_value > limit_max) | any(ori_value < limit_min)){
      tmp <- linMap(ori_value,limit_min,limit_max)
      if(all(tmp > 0)){
        data_pred_dcast[i,2:(k+1)] <- linMap(ori_value,limit_min,limit_max)
      }
    }
  }
  data_pred <- melt(data_pred_dcast[,1:(k+1)],id.vars = 'shop_id')
  names(data_pred) <- c('shop_id','uni_time','value')
  data_pred$uni_time <- as.p(data_pred$uni_time)
  data_pred
}

# F3C. tuning value of sample based on last k samples' mean and sd including itself
gen_smp_aggra <- function(shop_pay,test_start,k){
  shop_pay_train <- subset(shop_pay,uni_time < test_start)
  min_date <- min(shop_pay_train$uni_time)
  cut_date <- seq.POSIXt(test_start,min_date,by = -86400*k)
  shop_pay_train$datecut <- cut.POSIXt(shop_pay_train$uni_time,cut_date)
  
  split_shop_pay <- split(shop_pay_train,shop_pay_train$shop_id)
  smp_aggra <- lapply(split_shop_pay,function(df){
    r <- tapply(df$value,factor(df$datecut),function(x)list(mean(x),sd(x)))
    data.frame(uni_time = names(r),
               smp_mean = sapply(r,'[[',1),
               smp_sd = sapply(r,'[[',2))
  })
}


smp_tuningC <- function(data_pred_dcast,smp_aggra,
                        k,volt_limit_weight,last_k){
  for(i in 1:nrow(data_pred_dcast)){
    ori_value <- as.numeric(data_pred_dcast[i,2:(k+1)])
    len_aggr <- nrow(smp_aggra[[i]])
    
    data_mean <- smp_aggra[[i]]$smp_mean;data_mean <- data_mean[!is.na(data_mean)]
    data_mean <- mean(data_mean[max(1,(length(data_mean)-last_k+1)):length(data_mean)],na.rm = T)
    data_sd <- smp_aggra[[i]]$smp_sd;data_sd <- data_sd[!is.na(data_sd)]
    data_sd <- mean(data_sd[max(1,(length(data_sd)-last_k+1)):length(data_sd)],na.rm = T)
    
    limit_min <- max(1,data_mean - volt_limit_weight*data_sd)
    limit_max <- data_mean + volt_limit_weight*data_sd
    # cat(i)
    if(any(ori_value > limit_max) | any(ori_value < limit_min)){
      data_pred_dcast[i,2:(k+1)] <- linMap(ori_value,limit_min,limit_max)
    }
  }
  data_pred <- melt(data_pred_dcast[,1:(k+1)],id.vars = 'shop_id')
  names(data_pred) <- c('shop_id','uni_time','value')
  data_pred$uni_time <- as.p(data_pred$uni_time)
  data_pred$value <- abs(data_pred$value)
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

# F6. generate result
gen_csv <- function(data_pred,title){
  r7 <- dcast(shop_id~uni_time,data = data_pred,value.var = 'value')
  names(r7) <- c('shop_id',paste('day_',1:14,sep=''))
  r7 <- r7[order(r7$shop_id),]
  write.table(r7,file = file.path(dir_data,'result',title),quote = F,sep=',',row.names = F,col.names = F)
  return(r7)
}
