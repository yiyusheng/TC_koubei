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
# load(file.path(dir_data,'volt_limit_set.Rda'))

flag_gen <- 1 #If generate the result
title <- '0212c.csv'  #file title
k <- 7  #days to extract as samples
volt_limit_weight <- 1.05
active_weight <- 1.05
last_k <- 1 #number of period used to adjust result
x <- ifelse(flag_gen == 1,test_end <- as.p('2016-11-15'),
            ifelse(flag_gen == 0,test_end <- as.p('2016-11-01'),test_end <- as.p('2016-10-31')))
test_start <- test_end - 14*86400

main <- function(pa){
  data_pred <- extract_data(shop_pay,k,test_start,teset_end)
  list[data_pred,data_pred_dcast,data_na] <- fill_missing_data(data_pred,k)
  data_pred <- smp_tuningB(data_pred_dcast,data_na,k,volt_limit_weight)
  # data_pred <- smp_tuningC(data_pred_dcast,smp_aggra,k,volt_limit_weight,last_k)
  data_pred <- expand_data(data_pred,k)
  
  data_comp <- add_real(data_pred,test_start,teset_end)
  error <- mean(data_comp$ms)
  cat(sprintf('pa:%s\t\tError:%.8f\n',
              pa,error))
  
  if(nrow(data_comp) != 0){
    data_comp$shop_id <- factor(data_comp$shop_id)
    aggr_ms <- data.frame(shop_id = levels(data_comp$shop_id),
                          mean  = as.numeric(tapply(data_comp$ms,data_comp$shop_id,mean)))
    aggr_ms$pa <- pa
    return(aggr_ms)
    return(data_comp)
  }else{
    data_pred$value[data_pred$uni_time == as.p('2016-11-11')] <- data_pred$value[data_pred$uni_time == as.p('2016-11-11')]*active_weight
    data_pred$value[data_pred$uni_time == as.p('2016-11-10')] <- data_pred$value[data_pred$uni_time == as.p('2016-11-10')]*active_weight
    data_pred$value[data_pred$uni_time == as.p('2016-11-12')] <- data_pred$value[data_pred$uni_time == as.p('2016-11-11')]*active_weight
    r <- gen_csv(data_pred,title)
    cat(sprintf('Length:%d\tAll positive:%s\tAll numeric:%s',nrow(r),all(r >= 0),all(!is.na(r))))
    return(r)
  }
}


r <- main(1.05)
# r <- lapply(seq(0.01,2.3,0.01),main)


# r <- lapply(seq(0.01,2.3,0.01),main)
# r1 <- do.call(rbind,r)
# r2 <- dcast(shop_id~pa,data = r1,value.var = 'mean')
# r2$shop_id <- fct2num(r2$shop_id)
# r3 <- data.frame(shop_id = r2$shop_id,
#                  vt = seq(0.01,2.3,0.01)[apply(r2,1,function(x)order(x[2:ncol(r2)]))[1:5]])
# r3 <- rbind(r3,c(1824,1))
# r3 <- r3[order(r3$shop_id),]
# volt_limit_set <- r3$vt
# save(r2,file = file.path(dir_data,'volt_limit_set.Rda'))
# x <- lapply(1:2000,plot_week_aggra)
