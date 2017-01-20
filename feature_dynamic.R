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

feature_start <- as.p('2016-10-01')
feature_end <- as.p('2016-11-01')
period_list <- seq.POSIXt(feature_start,feature_end,by = 'days')
shop <- expand.grid(shop_info$shop_id,period_list)
attr(shop,'out.attrs') <- NULL
names(shop) <- c('shop_id','date')
shop$shop_id <- factor(shop$shop_id)

# F1. day of a week , day of a month
shop$wday <- as.POSIXlt(shop$date)$wday
shop$mday <- as.POSIXlt(shop$date)$mday

# F2. historical number of pay/read
num_his <- c(1,4,12)
shop_pay$wday <- as.POSIXlt(shop_pay$uni_time)$wday
shop_pay$mday <- as.POSIXlt(shop_pay$uni_time)$mday

lapply(unique(shop_pay$wday),function(x){
  shop_pay_wday <- subset(shop_pay,wday == x)
  shop_pay_wday <- dcast(shop_pay_wday,shop_id ~ uni_time,value.var = 'value')
})