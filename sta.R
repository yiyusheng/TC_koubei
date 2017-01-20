#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: sta.R
#
# Description: preliminary statistic, shop[A], user_view[B], user_pay[C]
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-15 11:28:55
#
# Last   modified: 2017-01-15 11:28:57
#
#
#
rm(list = ls())
source('head.R')
load(file.path(dir_data,'data_load_A.Rda'))

# A1. city and location
sta_city_location <- melt(tapply(shop_info$location_id,shop_info$city_name,function(x)length(unique(x))))

# A2. location and shop
sta_location_shop <- melt(tapply(shop_info$shop_id,shop_info$location_id,function(x)length(unique(x))))

# A3. city and shop 
sta_city_shop <- melt(tapply(shop_info$shop_id,shop_info$city_name,function(x)length(unique(x))))

# B1. user_id reading
sta_uidB <- melt(table(user_view$user_id))

# B2. shop being read
sta_shopB <- melt(table(user_view$shop_id))
# ggplot(sta_shopB,aes(x = value)) + geom_histogram(binwidth = 100)

# B3. date being read
sta_dateB <- melt(table(as.Date(user_view$time_stamp)))
ggplot(sta_dateB,aes(x = as.p(Var1),y = value)) + geom_line()

# B4. hour being read
sta_hourB <- melt(table(format(user_view$time_stamp,'%H')))
ggplot(sta_hourB,aes(x = Var1,y = value)) + geom_line()

# C1. user_id paid
sta_uidC <- melt(table(user_pay$user_id))
