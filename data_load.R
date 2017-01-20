#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: data_load.R
#
# Description: Load data
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-15 10:45:19
#
# Last   modified: 2017-01-15 10:45:21
#
#
#
rm(list = ls())
source('head.R')

shop_info <- read.csv(file = file.path(dir_data,'datasets','shop_info.txt'),header = F,
                      col.names = c('shop_id','city_name','location_id','per_pay','score',
                                    'comment_cnt','shop_level','cate_1_name','cate_2_name','cate_3_name'))

user_pay <- read.csv(file = file.path(dir_data,'datasets','user_pay.txt'),header = F,
                     col.names = c('user_id','shop_id','time_stamp'))

user_view_1223 <- read.csv(file = file.path(dir_data,'datasets','user_view.txt'),header = F,
                      col.names = c('user_id','shop_id','time_stamp'))
user_view_0112 <- read.csv(file = file.path(dir_data,'datasets','20170112','extra_user_view.txt'),head = F,
                           col.names = c('user_id','shop_id','time_stamp'))



user_pay$time_stamp <- as.p(user_pay$time_stamp)
user_view_1223$time_stamp <- as.p(user_view_1223$time_stamp)
user_view_0112$time_stamp <- as.p(user_view_0112$time_stamp)

user_view <- rbind(user_view_1223,user_view_0112)


save(shop_info,user_pay,user_view,file = file.path(dir_data,'data_load_A.Rda'))
save(shop_info,file = file.path(dir_data,'shop_info.Rda'))
