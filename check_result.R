#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: check_result.R
#
# Description: Check measure of each shop,categary and etc.
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


check_result <- function(out){
  out$shop_id <- factor(out$shop_id)
  out$ms <- abs(out$value - out$pred)/(out$value + out$pred)
  cat(sprintf('Error: %.4f\n',mean(out$ms)))
  
  # C1. aggresive of measure of each shop: mean,sd,max,min,max_day,min_day
  aggr_ms <- data.frame(shop_id = levels(out$shop_id),
                        mean = as.numeric(tapply(out$ms,out$shop_id,mean)),
                        sd = as.numeric(tapply(out$ms,out$shop_id,sd)),
                        max = as.numeric(tapply(out$ms,out$shop_id,max)),
                        median = as.numeric(tapply(out$ms,out$shop_id,median)),
                        min = as.numeric(tapply(out$ms,out$shop_id,min)),
                        max_d = as.numeric(tapply(out$ms,out$shop_id,which.max)),
                        min_d = as.numeric(tapply(out$ms,out$shop_id,which.min)))
  
  # C2. Plot and return
  p1 <- ggplot(aggr_ms,aes(x = mean)) + geom_histogram(bins = 100) + ggtitle('mean')
  p2 <- ggplot(aggr_ms,aes(x = sd)) + geom_histogram(bins = 100) + ggtitle('sd')
  p3 <- ggplot(aggr_ms,aes(x = mean,y = sd)) + geom_point() + ggtitle('mean-sd')
  p4 <- ggplot(aggr_ms,aes(x = max)) + geom_histogram(bins = 100) + ggtitle('max')
  p5 <- ggplot(aggr_ms,aes(x = median)) + geom_histogram(bins = 100) + ggtitle('median')
  p6 <- ggplot(aggr_ms,aes(x = min)) + geom_histogram(bins = 100) + ggtitle('min')
  p7 <- ggplot(aggr_ms,aes(x = max_d)) + geom_histogram(bins = 100) + ggtitle('max_d')
  p8 <- ggplot(aggr_ms,aes(x = min_d)) + geom_histogram(bins = 100) + ggtitle('min_d')
  # multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols = 4)
  
  # return(list(aggr_ms,list(p1,p2,p3,p4,p5,p6,p7,p8),out))
  return(aggr_ms,out)
}

