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