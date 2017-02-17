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

require(doParallel)
idx <- seq_len(length(smp_aggra))
ck <- makeCluster(min(floor(detectCores()*0.9),length(idx)),type = 'FORK',outfile = 'out_na')
registerDoParallel(ck)
r <- foreach(i = idx,.verbose = T,.packages = 'reshape2') %dopar% arimaX(smp_aggra[[i]],test_start = test_start)
stopCluster(ck)
r3 <- do.call(rbind,r)
# r2 <- do.call(rbind,r)
# r1 <- do.call(rbind,r)

colMeans(r1,na.rm = T);colMeans(r2,na.rm = T);colMeans(r3,na.rm = T)
