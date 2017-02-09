#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: gen_result_last_week.R
#
# Description: [MAIN]Generate predicted value based on last week's number of pay
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


flag_gen <- 1;title <- '0209c.csv'
x <- ifelse(flag_gen == 1,test_end <- as.p('2016-11-15'),
            ifelse(flag_gen == 0,test_end <- as.p('2016-11-01'),test_end <- as.p('2016-11-01')))


main <- function(pa){
  test_start <- test_end - 14*86400
  k <- 7;vl <- 1.2
  
  data_pred <- extract_data(shop_pay,k,test_start,teset_end)
  list[data_pred,data_pred_dcast] <- fill_missing_data(data_pred,k)
  data_pred <- volt_limitB(data_pred_dcast,k,vl)
  data_pred <- expand_data(data_pred,k)
  data_comp <- add_real(data_pred,test_start,teset_end)
  error <- mean(data_comp$ms)
  cat(sprintf('START:%s\tNumber of efficient value:%.0f\tpa:%s\tError:%.4f\n',
              test_start,nrow(data_comp),pa,error))
  
  
  if(nrow(data_comp) != 0){
    return(data_comp)
  }else{
    data_pred$value <- round(data_pred$value*1.2)
    r <- gen_csv(data_pred,title)
    return(r)
  }
}


r <- main(0)
# r <- lapply(seq(0.7,1.1,0.05),main)
