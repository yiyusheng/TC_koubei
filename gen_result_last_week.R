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
source('base.R')
load(file.path(dir_data,'shop_pay.Rda'))
load(file.path(dir_data,'shop_info.Rda'))
load(file.path(dir_data,'smp_aggra.Rda')) #smp_aggra <- gen_smp_aggra(shop_pay,test_start,k)

flag_gen <- 1 #If generate the result
title <- '0210c.csv'  #file title
k <- 7  #days to extract as samples
volt_limit_weight <- 1.04
active_weight <- 1.0
last_k <- 1 #number of period used to adjust result
x <- ifelse(flag_gen == 1,test_end <- as.p('2016-11-15'),
            ifelse(flag_gen == 0,test_end <- as.p('2016-11-01'),test_end <- as.p('2016-10-31')))
test_start <- test_end - 14*86400

main <- function(pa1,pa2){
  data_pred <- extract_data(shop_pay,k,test_start,teset_end)
  list[data_pred,data_pred_dcast,data_na] <- fill_missing_data(data_pred,k)
  # data_pred <- smp_tuningB(data_pred_dcast,data_na,k,pa2)
  data_pred <- smp_tuningC(data_pred_dcast,smp_aggra,k,volt_limit_weight,last_k)
  data_pred <- expand_data(data_pred,k)
  
  data_comp <- add_real(data_pred,test_start,teset_end)
  error <- mean(data_comp$ms)
  cat(sprintf('pa1:%s\tpa2:%s\tError:%.8f\n',
              pa1,pa2,error))
  
  if(nrow(data_comp) != 0){
    return(data_comp)
  }else{
    data_pred$value <- round(data_pred$value*active_weight)
    r <- gen_csv(data_pred,title)
    cat(sprintf('Length:%d\tAll positive:%s\tAll numeric:%s',nrow(r),all(r >= 0),all(!is.na(r))))
    return(r)
  }
}


r <- main(0,1)
# r <- lapply(seq(1.02,1.06,0.01),function(x)main(1,x))
