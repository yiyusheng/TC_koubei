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
load(file.path(dir_data,'data_load_A.Rda'))
load(file.path(dir_data,'shop_pay.Rda'))

test_start <- as.p('2016-11-01') - 14*86400
test_end <- as.p('2016-11-01')

evaluate_pay <- function(data_pred){
  shop_pay_test <- subset(shop_pay,uni_time >= test_start & uni_time < test_end)
  data_comp <- merge(shop_pay_test,data_pred,by = c('shop_id','uni_time'))
  names(data_comp) <- c('shop_id','uni_time','real','pred')
  eval <- mean(abs((data_comp$real - data_comp$pred)/(data_comp$real + data_comp$pred)))
}

# E1.last 14 days
data_pred <- subset(shop_pay,uni_time >= (test_start - 86400*14) & uni_time < (test_end - 86400*14))
data_pred$uni_time <- data_pred$uni_time + 86400*14

# E2.last 7 days expand into two weeks
data_pred <- subset(shop_pay,uni_time >= (test_start - 86400*7) & uni_time < (test_end - 86400*7))
data_pred$uni_time <- data_pred$uni_time + 86400 * 7
tmp <- data_pred
tmp$uni_time <- tmp$uni_time + 86400 * 7
data_pred <- rbind(data_pred,tmp)
