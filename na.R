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

linMap <- function(x, from, to){
  mx <- mean(x)
  idx <- x > mx
  x[idx] <- (x[idx] - mean(x[idx])) / (max(x[idx]) - mean(x[idx])) * ((to - from)/2) + mean(x[idx])
  x[idx] <- (x[idx] - mean(x[idx])) / (mean(x[idx]) - min(x[idx])) * ((to - from)/2) + mean(x[idx])
  x
}

pa <- expand.grid(1:10,seq(0.7,1.3,0.05));
r <- mapply(main,pa[,1],pa[,2])
require(doParallel)
idx <- seq_len(nrow(pa))
ck <- makeCluster(min(floor(detectCores()*0.9),length(idx)),outfile = '')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = T,.packages = 'reshape2') %dopar% main(pa[i,1],pa[i,2])
stopCluster(ck)
r1 <- data.frame(matrix(unlist(r),byrow = T,nrow = nrow(pa)))
r1 <- dcast(X1~X2,data = r1,value.var = 'X3')

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