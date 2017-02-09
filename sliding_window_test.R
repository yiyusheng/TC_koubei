#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sliding_window_test.R
#
# Description: test sliding window of 7/14 days for each shop
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
source('gen_result_last_week_Func.R')
load(file.path(dir_data,'shop_pay.Rda'))
load(file.path(dir_data,'shop_info.Rda'))


flag_gen <- 0
ifelse(flag_gen == 0,test_end <- as.p('2016-11-01'),test_end <- as.p('2016-11-15'))
test_start <- test_end - 14*86400
start_wday <- as.POSIXlt(test_start)$wday
shop_pay$wday <- as.POSIXlt(shop_pay$uni_time)$wday
split_shop_pay <- split(shop_pay,shop_pay$shop_id)

sliding_window <- function(df,win_size = 7){
  target <- subset(df,uni_time >= test_start & uni_time < test_end)
  if(nrow(target) != 14)return(data.frame(shop_id = df$shop_id[1],
                                          pred_start = target$uni_time[1],
                                          ms = -2))
  
  pred_start <- df$uni_time[df$wday == start_wday & df$uni_time < test_start]
  
  r <- sapply(pred_start, function(ps){
    pred <- subset(df,uni_time >= ps & uni_time < ps + 86400*win_size)
    if(nrow(pred) != win_size)return(-1)
    else{
      pred <- rep(pred$value,win_size/7)
      return(mean(abs(pred - target$value)/(pred + target$value)))
    }
  })
  data.frame(shop_id = df$shop_id[1],
             pred_start = pred_start,
             ms = r)
}

r7 <- lapply(split_shop_pay,sliding_window,win_size = 7)
r7 <- do.call(rbind,r7)

sta_r7 <- by(r7,r7$shop_id,function(df){
  if(nrow(df) == 1)return(NULL)
  df <- df[order(df$ms),]
  df <- subset(df,ms >= 0)
  df$id <- 1:nrow(df)
  df$pred_start <- as.character(df$pred_start)
  return(list(ms = dcast(shop_id~id,data = df[1:5,c('shop_id','id','ms')],value.var = 'ms'),
              date = dcast(shop_id~id,data = df[1:5,c('shop_id','id','pred_start')],value.var = 'pred_start')))
})
sta_r7_ms <- lapply(sta_r7,'[[','ms');sta_r7_ms <- do.call(rbind,sta_r7_ms)
sta_r7_date <- lapply(sta_r7,'[[','date');sta_r7_date <- do.call(rbind,sta_r7_date)
