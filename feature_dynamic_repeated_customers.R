#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: feature_dynamic.R
#
# Description: generate dynamic features for each shop + each day
# feature1: weekday and monthday aggresive
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-19 17:01:37
#
# Last   modified: 2017-01-19 17:01:38
#
#
#

rm(list = ls())
source('head.R')
load(file.path(dir_data,'shop_info.Rda'))
load(file.path(dir_data,'shop_pay.Rda'))
load(file.path(dir_data,'shop_view.Rda'))
load(file.path(dir_data,'data_load_A.Rda'))

# F1. numbers of repeating customer (number of customers who paid for the same shop in a period[weeks/month])
repeated_customer <- function(df,rday,paynum){
  mind <- min(df$time_stamp)
  # maxd <- max(df$time_stamp)
  maxd <- as.Date(feature_end)
  r <- data.frame(shop_id = df$shop_id[1],
                  date = seq.Date(min(mind + rday,maxd),maxd,by = 'days'),
                  num_rc = 0) 
  for(i in seq_len(nrow(r))){
    dfd <- subset(df,time_stamp < r$date[i] & time_stamp >= (r$date[i] - rday))
    if(nrow(dfd) == 0){
      if(i > 1){
        r$num_rc[i] <- r$num_rc[i - 1]
      }else{
        r$num_rc[i] <- 0
      }
    }else{
      tbl <- melt(table(dfd$user_id))
      # We add an efficient_days and calculate the weighted number of repeated customers
      # in order to exclude situation the shop is closed in some day
      num_efficient_days <- length(unique(as.Date(dfd$time_stamp)))
      r$num_rc[i] <- sum(tbl$value > paynum)/num_efficient_days*rday
    }
  }
  cat(sprintf('%s\n',df$shop_id[1]))
  r
}

feature_start <- as.p('2016-05-01')
feature_end <- as.p('2016-11-01')
test_start <- feature_end - 86400 * 14

# F2.Prepare
user_pay_day <- subset(user_pay,time_stamp < test_start & time_stamp >= feature_start)
user_pay_day$time_stamp <- as.Date(user_pay_day$time_stamp)
user_pay_day$shop_id <- factor(user_pay_day$shop_id)
split_user_pd <- split(user_pay_day,user_pay_day$shop_id)
rday <- c(7,30,90);paynum <- 2

# F3.Generate number of repeated customers for 7/30/90 days
require(doParallel)
idx <- seq_len(length(split_user_pd))
# ck <- makeCluster(min(floor(detectCores()),length(idx)),type = 'FORK',outfile = 'out_fd_rc')
ck <- makeCluster(min(floor(detectCores()*0.9),length(idx)),outfile = 'out_fd_rc')
registerDoParallel(ck)
r7 <- foreach(i = idx,.verbose = T,.packages = 'reshape2') %dopar% repeated_customer(split_user_pd[[i]],rday[1],paynum)
r30 <- foreach(i = idx,.verbose = T,.packages = 'reshape2') %dopar% repeated_customer(split_user_pd[[i]],rday[2],paynum)
r90 <- foreach(i = idx,.verbose = T,.packages = 'reshape2') %dopar% repeated_customer(split_user_pd[[i]],rday[3],paynum)
stopCluster(ck)

# F4.Merge features
feature_dynamic_rc_7 <- do.call(rbind,r7)
feature_dynamic_rc_30 <- do.call(rbind,r30)
feature_dynamic_rc_90 <- do.call(rbind,r90)
names(feature_dynamic_rc_7)[3] <- 'num_rc_7'
names(feature_dynamic_rc_30)[3] <- 'num_rc_30'
names(feature_dynamic_rc_90)[3] <- 'num_rc_90'
feature_dynamic_rc <- merge(merge(feature_dynamic_rc_7,feature_dynamic_rc_30),feature_dynamic_rc_90)

# F5.Save
save(feature_dynamic_rc,file = file.path(dir_data,'feature_dynamic_rc.Rda'))


