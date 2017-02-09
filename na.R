sapply(seq(1.1,1.3,0.01),main)
data_comp_dcast <- dcast(shop_id~uni_time,data = data_comp[,c('shop_id','uni_time','ms')],value.var = 'ms')
data_comp_dcast$mean_ms <- apply(data_comp_dcast[,2:ncol(data_comp_dcast)],1,mean,na.rm = T)
shop_id_over20 <- data_comp_dcast$shop_id[data_comp_dcast$mean_ms > 0.2]
shop_info_over20 <- subset(shop_info,shop_id %in% shop_id_over20)
data_comp_over20 <- subset(data_comp,shop_id %in% shop_id_over20)