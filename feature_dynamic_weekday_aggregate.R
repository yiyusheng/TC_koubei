#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: feature_dynamic.R
#
# Description: generate dynamic features for each shop + each day
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-19 17:01:37
#
# Last   modified: 2017-01-19 17:01:38
#
#
#

rm(list = ls())
source('head.R')
load(file.path(dir_data,'shop_info.Rda'))
load(file.path(dir_data,'shop_pay.Rda'))
load(file.path(dir_data,'shop_view.Rda'))

feature_start <- as.p('2016-05-01')
feature_end <- as.p('2016-11-01')
test_start <- feature_end - 86400 * 14

# F1. historical number of pay/read
his_pay <- function(his_day,nh){
  r <- his_day
  for(i in seq_len(nrow(his_day))){
    cat(sprintf('i:%.0f',i))
    idx_j <- which(!is.na(his_day[i,]))[2]
    r[i,2:min(ncol(r),(idx_j+nh-1))] <- NA
    for(j in min(ncol(r),(idx_j + nh)):ncol(his_day)){
      cat(sprintf('\tj:%.0f\tidx:%s\n',j,names(his_day)[2]))
      tmp <- his_day[i,2:(j-1)]
      tmp <- tmp[!is.na(tmp)]
      len <- length(tmp)
      if(len == 0){
        r[i,j] <- NA
      }else{
        idx_last_efficient <- max(which(tmp != 0))
        r[i,j] <- mean(tmp[max(1,(idx_last_efficient-nh + 1)):idx_last_efficient])
      }
    }
  }
  r <- melt(r,id.vars = 'shop_id',na.rm = T)
  names(r) <- c('shop_id','date','value')
  r
}

num_his <- c(1,4,12)
shop_pay_cleartest <- subset(shop_pay,uni_time >= feature_start & uni_time < feature_end)
shop_pay_cleartest$wday <- as.POSIXlt(shop_pay_cleartest$uni_time)$wday
shop_pay_cleartest$value[shop_pay_cleartest$uni_time >= test_start] <- 0

his_wday <- lapply(unique(shop_pay_cleartest$wday),function(x){
  shop_pay_wday <- subset(shop_pay_cleartest,wday == x)
  shop_pay_wday <- dcast(shop_pay_wday,shop_id ~ uni_time,value.var = 'value')
})

his_wday_1 <- lapply(his_wday,his_pay,nh = 1)
his_wday_1 <- do.call(rbind,his_wday_1)
his_wday_4 <- lapply(his_wday,his_pay,nh = 4)
his_wday_4 <- do.call(rbind,his_wday_4)
his_wday_12 <- lapply(his_wday,his_pay,nh = 12)
his_wday_12 <- do.call(rbind,his_wday_12)

feature_dynamic_wa <- merge(merge(his_wday_1,his_wday_4,by = c('shop_id','date')),his_wday_12,by = c('shop_id','date'))
names(feature_dynamic_wa) <- c('shop_id','date','hwday_1','hwday_4','hwday_12')
feature_dynamic_wa$date <- as.Date(feature_dynamic_wa$date)

# F2. day of a week , day of a month
feature_dynamic_wa$wday <- as.POSIXlt(feature_dynamic_wa$date)$wday
feature_dynamic_wa$mday <- as.POSIXlt(feature_dynamic_wa$date)$mday

# F.save
save(feature_dynamic_wa,file = file.path(dir_data,'feature_dynamic_wa.Rda'))
