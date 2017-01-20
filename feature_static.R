#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: feature_static.R
#
# Description: generate static features for each shop
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-01-19 16:21:46
#
# Last   modified: 2017-01-19 16:21:54
#
#
#

rm(list = ls())
source('head.R')
load(file.path(dir_data,'shop_info.Rda'))
load(file.path(dir_data,'shop_pay.Rda'))
load(file.path(dir_data,'shop_view.Rda'))
shop_pay$shop_id <- factor(shop_pay$shop_id)
shop_view$shop_id <- factor(shop_view$shop_id)

# F1. coefficient of correlation between view and pay
shop <- merge(shop_pay,shop_view,by = c('shop_id','uni_time'),all = T)
shop <- subset(shop,uni_time >= as.p('2016-02-01'))
names(shop) <- c('shop_id','uni_time','numPay','numView')
shop$shop_id <- factor(shop$shop_id)
shop[is.na(shop)] <- 0
corr_pv <- data.frame(shop_id = levels(shop$shop_id),
                            corr = as.numeric(by(shop[,c('numPay','numView')],factor(shop$shop_id),function(df)cor(df$numPay,df$numView))))
corr_pv$corr[is.na(corr_pv$corr)] <- 0

# Plot view and pay
# shop_melt <- melt(shop,id.vars = c('shop_id','uni_time'))
# ggplot(subset(shop_melt,shop_id == '330'),aes(x = uni_time,y = value,group = variable,color = variable)) + geom_line()

# F2. mean view and pay in each day of week/month
shop_pay$wday <- as.POSIXlt(shop_pay$uni_time)$wday
shop_pay$mday <- as.POSIXlt(shop_pay$uni_time)$mday
shop_view$wday <- as.POSIXlt(shop_view$uni_time)$wday
shop_view$mday <- as.POSIXlt(shop_view$uni_time)$mday

pay_wday <- aggregate(shop_pay$value,by = list(shop_pay$shop_id,shop_pay$wday),mean)
pay_mday <- aggregate(shop_pay$value,by = list(shop_pay$shop_id,shop_pay$mday),mean)
view_wday <- aggregate(shop_view$value,by = list(shop_view$shop_id,shop_view$wday),mean)
view_mday <- aggregate(shop_view$value,by = list(shop_view$shop_id,shop_view$mday),mean)

pay_wday <- dcast(Group.1~Group.2,data = pay_wday,value.var = 'x')
pay_mday <- dcast(Group.1~Group.2,data = pay_mday,value.var = 'x')
view_wday <- dcast(Group.1~Group.2,data = view_wday,value.var = 'x')
view_mday <- dcast(Group.1~Group.2,data = view_mday,value.var = 'x')

revise_name <- function(df,prefix){
  len <- ncol(df)
  names(df) <- c('shop_id',paste(prefix,names(df)[2:len],sep=''))
}

names(pay_wday) <- revise_name(pay_wday,'pwday')
names(pay_mday) <- revise_name(pay_mday,'pmday')
names(view_wday) <- revise_name(view_wday,'vwday')
names(view_mday) <- revise_name(view_mday,'vmday')

# F. Add all static feature to shop_info
# shop_info$corr_pv <- corr_pay_view$corr[match(shop_info$shop_id,corr_pay_view$shop_id)]
shop_info$city_name <- as.numeric(shop_info$city_name)
shop_info$cate_1_name <- as.numeric(shop_info$cate_1_name)
shop_info$cate_2_name <- as.numeric(shop_info$cate_2_name)
shop_info$cate_3_name <- as.numeric(shop_info$cate_3_name)

shop_info[is.na(shop_info)] <- 0

save(shop_info,corr_pv,pay_wday,pay_mday,file = file.path(dir_data,'feature_static.Rda'))