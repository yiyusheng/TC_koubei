#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: fill_na.R
#
# Description: fill na when extract data
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-24 09:37:29
#
# Last   modified: 2017-01-24 09:37:31
#
#
#

# F1. fill na with mannual value
fill_with_mannual <- function(data_pred_dcast){
  if(test_end == as.p('2016-11-01')){
    data_pred_dcast[352,6:7] <- 308
    data_pred_dcast[363,7] <- 63
    data_pred_dcast[459,4] <- 25
    data_pred_dcast[547,3] <- 52
    data_pred_dcast[632,8] <- 14
    data_pred_dcast[722,4] <- 130
    data_pred_dcast[1053,3:4] <- 19
    data_pred_dcast[1464,2:5] <- 2
    data_pred_dcast[1661,4:5] <- 469
  }else if(test_end == as.p('2016-11-15')){
    data_pred_dcast[5,3:4] <- (302+215)/2
    data_pred_dcast[444,3:4] <- (76+290)/2
    data_pred_dcast[470,3:4] <- (55+86)/2
    data_pred_dcast[513,6] <- (167+85)/2
    data_pred_dcast[547,3] <- (99+59)/2
    data_pred_dcast[632,2] <- (12+36)/2
    data_pred_dcast[659,2] <- (21+28)/2
    data_pred_dcast[987,8] <- 37
    data_pred_dcast[1163,5] <- (102+28)/2
    data_pred_dcast[1185,3] <- (341+346)/2
    data_pred_dcast[1486,2] <- 51
    data_pred_dcast[1556,2:4] <- (200+195)/2
    data_pred_dcast[1716,6:7] <- (151+190)/2
    data_pred_dcast[1831,6:7] <- (661+679)/2
    data_pred_dcast[1858,2:3] <- (51+57)/2
    data_pred_dcast[1918,4] <- 6
    data_pred_dcast[1959,2] <- (490+407)/2
  }
  data_pred_dcast
}

# F2.fill with mean of all
fill_with_mean_all <- function(data_pred_dcast,k){
  idx <- which(apply(data_pred_dcast,1,function(x)any(is.na(x))))
  for(i in idx){
    data_pred_dcast[i,is.na(data_pred_dcast[i,])] <- mean(as.numeric(data_pred_dcast[i,2:(k+1)]),na.rm = T)
  }
  data_pred_dcast
}