#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: tsModel.R
#
# Description: time seris model test to decide different between history aggresive values and real value
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-16 16:02:34
#
# Last   modified: 2017-01-16 16:02:35
#
#
#

rm(list = ls())
source('head.R')
load(file.path(dir_data,'data_load_A.Rda'))

test_start <- as.p('2016-11-01') - 14*86400
test_end <- as.p('2016-11-01')

time_interval <- 86400;gen_start <- as.p('2015-06-26');gen_end <- as.p('2016-11-01')

gen_shop_pay <- function(time_interval,gen_start,gen_end){
  data_shop_up <- subset(user_pay,time_stamp >= gen_start & time_stamp < gen_end)
  data_shop_up$uni_time <- as.POSIXct(floor(as.numeric(data_shop_up$time_stamp)/time_interval)*time_interval,tz = 'UTC',origin = '1970-01-01')
  
  shop_pay <- aggregate(data_shop_up$user_id,by = list(data_shop_up$shop_id,data_shop_up$uni_time),FUN = length)
  names(shop_pay) <- c('shop_id','uni_time','value')
  shop_pay <- shop_pay[order(shop_pay$shop_id,shop_pay$uni_time),]
  shop_pay
}


gen_shop_view <- function(time_interval,gen_start,gen_end){
  data_shop_uv <- subset(user_view,time_stamp >= gen_start & time_stamp < gen_end)
  data_shop_uv$uni_time <- as.POSIXct(floor(as.numeric(data_shop_uv$time_stamp)/time_interval)*time_interval,tz = 'UTC',origin = '1970-01-01')
  
  shop_view <- aggregate(data_shop_uv$user_id,by = list(data_shop_uv$shop_id,data_shop_uv$uni_time),FUN = length)
  names(shop_view) <- c('shop_id','uni_time','value')
  shop_view <- shop_view[order(shop_view$shop_id,shop_view$uni_time),]
  shop_view
}


plot_shop_pay <- function(shop_pay){
  shop_pay_split <- split(shop_pay,shop_pay$shop_id)
  lapply(shop_pay_split,function(df){
    df <- subset(df,uni_time >=test_start & uni_time < test_end)
    p <- ggplot(df,aes(x = uni_time,y = value)) + geom_line() + ggtitle(paste('shop',df$shop_id[1],sep='_')) +
      xlab('time') + ylab('number of user paid')
    ggsave(file = file.path(dir_data,'shop_pay',paste('shop_',df$shop_id[1],'.jpg',sep='')),
           plot = p,width = 12,height = 8,dpi = 100)
  })
}

######cut line######
shop_pay <- gen_shop_pay(time_interval,gen_start,gen_end)
save(shop_pay,file = file.path(dir_data,'shop_pay.Rda'))
shop_view <- gen_shop_view(time_interval,gen_start,gen_end)
save(shop_view,file = file.path(dir_data,'shop_view.Rda'))




