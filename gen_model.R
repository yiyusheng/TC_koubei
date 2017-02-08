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
load(file.path(dir_data,'shop_pay.Rda'))
load(file.path(dir_data,'shop_view.Rda'))
feature_start <- as.p('2016-05-01')
feature_end <- as.p('2016-11-01')
test_start <- feature_end - 86400 * 14

# M1. merge static features
feature_static <- merge(shop_info,corr_pv,by = 'shop_id')
feature_static <- merge(feature_static,pay_mday,by = 'shop_id')
feature_static <- merge(feature_static,pay_wday,by = 'shop_id')

# M2. filter dynamic features by time and etc.
feature_dynamic_wa <- subset(feature_dynamic_wa,date >= as.Date(feature_start))
feature_dynamic_rc <- subset(feature_dynamic_rc,date >= as.Date(feature_start))

# M3. merge dynamic features by shop_id and date
feature_dynamic <- feature_dynamic_wa
feature_dynamic <- merge(feature_dynamic,feature_dynamic_rc,by = c('shop_id','date'))

# M4. merge static features , dynamic features and the number of paid
feature_static$shop_id <- factor(feature_static$shop_id)
names(shop_pay)[2] <- 'date'
feature <- merge(feature_dynamic,feature_static,by = 'shop_id')
feature <- merge(shop_pay,feature,by = c('shop_id','date'))

# M5. generate train and test
test <- subset(feature,date > test_start)
train <- subset(feature,date <= test_start & date > feature_start + 86400 * 90)

# M6. build the model by gbdt
n_trees <- 10000
gbm1 <-  
  gbm(value~.,             # formula  
      data=train[,3:ncol(train)],                         # dataset  
      # var.monotone=c(0,0,0,0,0,0),    # -1: monotone decrease, +1: monotone increase,  
      #  0: no monotone restrictions  
      distribution="gaussian",        # see the help for other choices  
      n.trees=n_trees,                     # number of trees  
      shrinkage=0.01,                   # shrinkage or learning rate, 0.001 to 0.1 usually work  
      interaction.depth=3,             # 1: additive model, 2: two-way interactions, etc.  
      bag.fraction = 0.5,              # subsampling fraction, 0.5 is probably best  
      train.fraction = 0.5,           # fraction of data for training, first train.fraction*N used for training  
      n.minobsinnode = 10,             # minimum total weight needed in each node  
      cv.folds = 3,                     # do 3-fold cross-validation  
      keep.data=TRUE,                  # keep a copy of the dataset with the object  
      verbose=T,                   # don't print out progress  
      n.cores=1)                        # use only a single core (detecting #cores is error-prone, so avoided here)  


# check performance using 5-fold cross-validation  
best.iter <- gbm.perf(gbm1,method="cv")  
print(best.iter)  
summary(gbm1,n.trees=best.iter) # based on the estimated best number of trees  

# M7. test
pp <- data.frame(pred = predict(gbm1,test[,4:ncol(test)],n.trees=n_trees))
out <- cbind(test[,c('shop_id','date','value')],pp)
eval <- mean(abs((out$value - out$pred)/(out$value + out$pred)))
cat(sprintf('eval:%.4f',eval))

save(out,gbm1,train,test,file = file.path(dir_data,'gen_model.Rda'))
