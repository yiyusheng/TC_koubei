#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: gen_result_mean_week.R
#
# Description: [MAIN]Generate predicted value based on mean value of last N weeks to decrease error cased by indivisual week.
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

title <- '0208c.csv'
flag_gen <- 0

x <- ifelse(flag_gen == 0,test_end <- as.p('2016-11-01'),test_end <- as.p('2016-11-15'))
test_start <- test_end - 14*86400

k <- 7
data_pred <- extract_data(shop_pay,k)
list[data_pred,data_pred_dcast] <- fill_missing_data(data_pred,k)

data_pred_dcast <- data_pred_dcast[,1:(k+1)]
fold_weeks <- function(data_pred_dcast,k,numf){
  idx <- data.frame(date = names(data_pred_dcast[2:(k+1)]),
                  id = rep(1:numf,each = ceiling(k/numf)))
  for(i in 1:numf){
    if(i == 1)s = data_pred_dcast[,as.character(idx$date[idx$id == i])]
    else{s = s + data_pred_dcast[,as.character(idx$date[idx$id == i])]}
  }
  s <- s/numf
  data_pred_mean <- cbind(data_pred_dcast$shop_id,s)
  names(data_pred_mean) <- c('shop_id',as.character(idx$date[idx$id == numf]))
  r <- melt(data_pred_mean,id.vars = 'shop_id')
  names(r) <- c('shop_id','uni_time','value')
  r$uni_time <- as.p(r$uni_time)
  r$value <- round(r$value)
  r
}
data_pred <- fold_weeks(data_pred_dcast,k,2)

data_pred <- expand_data(data_pred,k)
data_comp <- add_real(data_pred)
r <- check_result(data_comp)
