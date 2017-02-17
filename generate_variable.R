#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: generate_variable.R
#
# Description: generate variables and save
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


# generate volt_limit_set
data_comp$shop_id <- factor(data_comp$shop_id)
aggr_ms <- data.frame(shop_id = levels(data_comp$shop_id),
                      mean = as.numeric(tapply(data_comp$ms,data_comp$shop_id,mean)))
aggr_ms$pa <- pa
return(aggr_ms)

r <- lapply(seq(0.01,2.3,0.01),main)
r1 <- do.call(rbind,r)
r2 <- dcast(shop_id~pa,data = r1,value.var = 'mean')
r2$shop_id <- fct2num(r2$shop_id)
r3 <- data.frame(shop_id = r2$shop_id,
                 vt = seq(0.01,3,0.01)[apply(r2,1,which.min) - 1])
r3 <- rbind(r3,c(1824,1))
r3 <- r3[order(r3$shop_id),]
volt_limit_set <- r3$vt
save(volt_limit_set,file = file.path(dir_data,'volt_limit_set.Rda'))

# generate smp_aggra
smp_aggra <- gen_smp_aggra(shop_pay,as.p('2016-11-01'),k)
save(smp_aggra,file = file.path(dir_data,'smp_aggra.Rda'))
