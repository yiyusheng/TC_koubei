#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: evaluate_pay.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-16 21:44:43
#
# Last   modified: 2017-01-19 11:23:32
#
#
#

rm(list = ls())
source('head.R')
source('check_result.R')
load(file.path(dir_data,'shop_pay.Rda'))

# test_end <- as.p('2016-11-01')
# test_start <- test_end - 14*86400

test_end <- as.p('2016-11-15')
test_start <- test_end - 14*86400


add_real <- function(data_pred){
  data_real <- subset(shop_pay,uni_time >= test_start & uni_time < test_end)
  data_comp <- merge(data_real,data_pred,by = c('shop_id','uni_time'))
  names(data_comp) <- c('shop_id','uni_time','value','pred')
  data_comp
}

expand_last_kdays <- function(k){
  rp <- ceiling(14/k)
  data_pred <- subset(shop_pay,uni_time >= (test_start - 86400*k) & uni_time < test_start)
  data_pred_expand <- lapply(seq_len(rp),function(i){
    data_pred$uni_time <- data_pred$uni_time + 86400 * k * i
    data_pred
  })
  data_pred_expand <- do.call(rbind,data_pred_expand)
  data_pred <- subset(data_pred_expand,uni_time >= test_start & uni_time < test_end)
  data_comp <- add_real(data_pred)
  cat(sprintf('Last %.0f days\t',k))
  
  if(nrow(data_comp) == 0){
    return(data_pred)
  }else{
    r <- check_result(data_comp)
    return(list(r,data_comp))
  }
}

# E1.last 14 days
r14 <- expand_last_kdays(14)


# E2.last 7 days
r7 <- expand_last_kdays(7)

# E3.generate result
r7a <- dcast(shop_id~uni_time,data = r7,value.var = 'value')
names(r7a) <- c('shop_id',paste('day_',1:14,sep=''))
tmp_1707 <- shop_pay$value[shop_pay$shop_id == '1707' & 
                             shop_pay$uni_time >= as.p('2016-10-18') & 
                             shop_pay$uni_time < as.p('2016-10-25')]
tmp_1824 <- shop_pay$value[shop_pay$shop_id == '1824' & 
                             shop_pay$uni_time >= as.p('2016-10-04') & 
                             shop_pay$uni_time < as.p('2016-10-11')]
r7a <- rbind(r7a,
             c(1707,tmp_1707,tmp_1707),
             c(1824,tmp_1824,tmp_1824))
r7a <- r7a[order(r7a$shop_id),]
write.table(r7a,file = file.path(dir_data,'0208a.csv'),quote = F,sep=',',row.names = F,col.names = F)
