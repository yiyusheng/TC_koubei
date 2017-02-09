#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: optimize_last_week.R
#
# Description: optimize result of last-week model.
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

source('gen_result_last_week.R')

shop_error <- merge(r,shop_info)
shop_error$shop_id <- fct2num(shop_error$shop_id)
shop_error$city_name <- as.numeric(shop_error$city_name)
shop_error$cate_1_name <- as.numeric(shop_error$cate_1_name)
shop_error$cate_2_name <- as.numeric(shop_error$cate_2_name)
shop_error$cate_3_name <- as.numeric(shop_error$cate_3_name)
shop_error[is.na(shop_error)] <- 0

library(gbm)

n_trees <- 1000
idx_error_ftr <- 3
gbm1 <- gbm(sd~.,             # formula  
      data=shop_error[,c(idx_error_ftr,9:ncol(shop_error))],                         # dataset  
      # var.monotone=c(0,0,0,0,0,0),    # -1: monotone decrease, +1: monotone increase,  
      #  0: no monotone restrictions  
      distribution="gaussian",        # see the help for other choices  
      n.trees=n_trees,                     # number of trees  
      shrinkage=0.005,                   # shrinkage or learning rate, 0.001 to 0.1 usually work  
      interaction.depth=10,             # 1: additive model, 2: two-way interactions, etc.  
      bag.fraction = 0.5,              # subsampling fraction, 0.5 is probably best  
      train.fraction = 0.5,           # fraction of data for training, first train.fraction*N used for training  
      n.minobsinnode = 10,             # minimum total weight needed in each node  
      cv.folds = 5,                     # do 3-fold cross-validation  
      keep.data=TRUE,                  # keep a copy of the dataset with the object  
      verbose=T,                   # don't print out progress  
      n.cores=1)                        # use only a single core (detecting #cores is error-prone, so avoided here)  


# check performance using 5-fold cross-validation  
best.iter <- gbm.perf(gbm1,method="cv")  
print(best.iter)  
summary(gbm1,n.trees=best.iter) # based on the estimated best number of trees  

# M7. test
# pp <- data.frame(pred = predict(gbm1,test[,4:ncol(test)],n.trees=n_trees))
# out <- cbind(test[,c('shop_id','date','value')],pp)
# eval <- mean(abs((out$value - out$pred)/(out$value + out$pred)))