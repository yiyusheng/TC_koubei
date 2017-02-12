#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: base.R
#
# Description: commenly used function in this project
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

get_shop <- function(id,sp = shop_pay){
  subset(sp,shop_id %in% id)
}

plot_week_aggra <- function(i){
  cat(i)
  df <- smp_aggra[[i]]
  p <- ggplot(df, aes(x=1:nrow(df), y=smp_mean, group = 1)) + 
    geom_errorbar(aes(ymin=smp_mean-smp_sd, ymax=smp_mean+smp_sd), width=.1) +
    geom_line() + geom_point() + xlab('weeks') + ylab('mean') + ggtitle(i)
  ggsave(p,file = file.path(dir_data,'figure','week_aggra',paste(i,'.jpg',sep='')))
}


arima <- function(df){
  require(forecast)
  fit_mean <- auto.arima(df$smp_mean,D = 1,trace = F,
                    max.P = 0,max.Q = 0,max.p = 20,max.q = 20,
                    ic = 'aic')
  pred_mean <- forecast.Arima(fit_mean,h = 2)
  
  fit_sd <- auto.arima(df$smp_sd,D = 1,trace = F,
                         max.P = 0,max.Q = 0,max.p = 20,max.q = 20,
                         ic = 'aic')
  pred_sd <- forecast.Arima(fit_sd,h = 2)
  
  
  df$uni_time <- as.p(df$uni_time)
  max_ut <- max(df$uni_time)
  tmp <- data.frame(uni_time = c(max_ut+86400*7,max_ut+86400*14),
                    smp_mean = pred_mean$mean[1:2],
                    smp_sd = pred_sd$mean[1:2])
  df <- rbind(df,)
}



