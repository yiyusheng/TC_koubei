#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: gen_model.R
#
# Description: generate model by using all features. We treat all days as the same object and ignore the degree of information completement.
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

rm(list = ls())
source('head.R')
library(gbm)

load(file = file.path(dir_data,'feature_dynamic_rc.Rda'))
load(file = file.path(dir_data,'feature_dynamic_wa.Rda'))
load(file = file.path(dir_data,'feature_static.Rda'))
# load(file.path(dir_data,'shop_info.Rda'))
load(file.path(dir_data,'shop_pay.Rda'))
load(file.path(dir_data,'shop_view.Rda'))

feature_start <- as.p('2016-05-01')
feature_end <- as.p('2016-11-01')
test_start <- feature_end - 86400 * 14

# M1. merge static features
shop_static <- merge(shop_info,corr_pv,by = 'shop_id')
shop_static <- merge(shop_static,pay_mday,by = 'shop_id')
shop_static <- merge(shop_static,pay_wday,by = 'shop_id')

# M2. filter dynamic features by time and etc.
feature_dynamic_wa <- subset(feature_dynamic_wa,date >= as.Date(feature_start))
# feature_dynamic_rc_7 <- subset(feature_dynamic_rc_7,date >= as.Date(feature_start))
# feature_dynamic_rc_30 <- subset(feature_dynamic_rc_30,date >= as.Date(feature_start))
# feature_dynamic_rc_90 <- subset(feature_dynamic_rc_90,date >= as.Date(feature_start))


# M3. merge dynamic features by shop_id and date
feature_dynamic <- feature_dynamic_wa
# feature_dynamic <- merge(feature_dynamic,feature_dynamic_rc_7)
# feature_dynamic <- merge(feature_dynamic,feature_dynamic_rc_30)
# feature_dynamic <- merge(feature_dynamic,feature_dynamic_rc_90)

# M4. merge static features , dynamic features and the number of paid
shop_info$shop_id <- factor(shop_info$shop_id)
names(shop_pay)[2] <- 'date'
feature <- merge(feature_dynamic,shop_info,by = 'shop_id')
feature <- merge(shop_pay,feature,by = c('shop_id','date'))

# M5. generate train and test
test <- subset(feature,date > test_start)
train <- subset(feature,date <= test_start)

# M6. build the model by gbdt
train <- transform(train,value)