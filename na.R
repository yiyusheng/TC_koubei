sapply(seq(1.1,1.3,0.01),main)
data_comp_dcast <- dcast(shop_id~uni_time,data = data_comp[,c('shop_id','uni_time','ms')],value.var = 'ms')
data_comp_dcast$mean_ms <- apply(data_comp_dcast[,2:ncol(data_comp_dcast)],1,mean,na.rm = T)
shop_id_over20 <- data_comp_dcast$shop_id[data_comp_dcast$mean_ms > 0.2]
shop_info_over20 <- subset(shop_info,shop_id %in% shop_id_over20)
data_comp_over20 <- subset(data_comp,shop_id %in% shop_id_over20)

return(data.frame(test_start,error))
r <- lapply(seq.POSIXt(as.p('2016-05-01'),as.p('2016-11-01'),by = 'days'),main)
r1 <- lapply(r,'[[',1);r1 <- do.call(rbind,r1)
r2 <- lapply(r,'[[',2);r2 <- do.call(rbind,r2)
r3 <- data.frame(date = as.POSIXct(r1,tz = 'UTC',origin = '1970-01-01'),error = r2)
ggplot(r3,aes(x = date,y = error)) + geom_line() + geom_point()
readme <- "It's a data.frame including date(test_start) and error. I use different test_start"
save(r3,readme,file = file.path(dir_data,'test_start.Rda'))