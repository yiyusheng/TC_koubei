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
source('gen_result_last_week_Func.R')
load(file.path(dir_data,'shop_pay.Rda'))
load(file.path(dir_data,'shop_info.Rda'))

title <- '0208b.csv'
# test_end <- as.p('2016-11-01')
test_end <- as.p('2016-11-15')
test_start <- test_end - 14*86400


k <- 7
data_pred <- extract_data(shop_pay,k)
list[data_pred,data_pred_dcast] <- fill_missing_data(data_pred)
data_pred <- expand_data(data_pred,k)
data_comp <- add_real(data_pred)
cat(sprintf('Last %.0f days\t',k))

if(nrow(data_comp) != 0){
  r <- check_result(data_comp)
}else{
  r <- gen_csv(data_pred,title)
}



