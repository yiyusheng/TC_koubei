#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: base.R
#
# Description: commenly used function in this project
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

get_shop <- function(id,sp = shop_pay){
  subset(sp,shop_id %in% id)
}

mse <- function(x,y){
  mean((x-y)**2)
}

plot_week_aggra <- function(df){
  if(!is.data.frame(df))df <- smp_aggra[[df]]
  p <- ggplot(df, aes(x=1:nrow(df), y=smp_mean, group = 1)) + 
    geom_errorbar(aes(ymin=smp_mean-smp_sd, ymax=smp_mean+smp_sd), width=.1) +
    geom_line() + geom_point() + xlab('weeks') + ylab('mean') + ggtitle(df$shop_id[1])
  ggsave(p,file = file.path(dir_data,'figure','week_aggra',paste(df$shop_id[1],'.jpg',sep='')))
  return(p)
}


arimaX <- function(df,test_start){
  dfa <- subset(df,uni_time < test_start)
  dfb <- subset(df,uni_time >= test_start)
  
  fit_mean <- auto.arima(dfa$smp_mean,D = 1,trace = F,
                    max.P = 0,max.Q = 0,max.p = 20,max.q = 20,
                    ic = 'aic')
  fit_sd <- auto.arima(dfa$smp_sd,D = 1,trace = F,
                       max.P = 0,max.Q = 0,max.p = 20,max.q = 20,
                       ic = 'aic')
  pred_mean <- forecast.Arima(fit_mean,h = 2)$mean[1:2]
  pred_sd <- forecast.Arima(fit_sd,h = 2)$mean[1:2]
  real_mean <- dfb$smp_mean
  real_sd <- dfb$smp_sd
  # last_mean <- rep(dfa$smp_mean[which.max(dfa$uni_time)],2)
  # last_sd <- rep(dfa$smp_sd[which.max(dfa$uni_time)],2)
  # last_mean <- dfa$smp_mean[(nrow(dfa)-1):nrow(dfa)]
  # last_sd <- dfa$smp_sd[(nrow(dfa)-1):nrow(dfa)]
  # last_mean <- rep(mean(dfa$smp_mean[(nrow(dfa)-1):nrow(dfa)]),2)
  # last_sd <- rep(mean(dfa$smp_sd[(nrow(dfa)-1):nrow(dfa)]),2)
  last_mean <- predict(fit_mean,dfa$smp_mean[(nrow(dfa)-3):nrow(dfa)])
  last_sd <- predict(fit_mean,dfa$smp_sd[(nrow(dfa)-3):nrow(dfa)])
  

  error_mean1 <- mse(real_mean,pred_mean)
  error_mean2 <- mse(real_mean,last_mean)
  error_sd1 <- mse(real_sd,pred_sd)
  error_sd2 <- mse(real_sd,last_sd)
  
  cat(sprintf('%d\n',fct2num(df$shop_id[1])))
  data.frame(em1 = error_mean1,em2 = error_mean2,es1 = error_sd1,es2 = error_sd2)
}

# rescale x to a new mu and sigma
rescale <- function(x,mu,sigma){
  mu0 <- mean(x)
  sigma0 <- sd(x)
  (x-mu0)*sigma/sigma0 + mu 
}

# predict mean of next week with combination of last 4 weeks
gen_fit_glm <- function(){
  comb_start <- test_start - 90*86400
  last_weeks <- 4
  gen_last_Nweeks_train <- function(df,attr){
    # cat(sprintf('%d\n',fct2num(df$shop_id[1])))
    dfa <- subset(df,uni_time < test_start & uni_time >= comb_start)
    if(nrow(dfa) < 5)return(NULL)
    dfb <- subset(df,uni_time >= test_start)
    m <- matrix(0,nrow(dfa)-last_weeks,last_weeks+1)
    for(i in 1:nrow(m)){
      m[i,] <- dfa[[attr]][i:(i+4)]
    }
    m
  }
  train_mean <- data.frame(do.call(rbind,lapply(smp_aggra,gen_last_Nweeks_train,attr = 'smp_mean')))
  train_sd <- data.frame(do.call(rbind,lapply(smp_aggra,gen_last_Nweeks_train,attr = 'smp_sd')))
  fit_mean <- glm(X5~.,data =train_mean,family = 'gaussian')
  fit_sd <- glm(X5~.,data =train_sd,family = 'gaussian')
  list(fit_mean,fit_sd)
}

