library(ggplot2)
library(dplyr)
library(reshape)

#setwd("/home/jcquiroz/Documents/patagonian_toothfish_utas/toy_model/DelayDiff")

rm(list = ls())
source("read.admb.R")
A <- read.admb("DDmod")


def.path <- paste(getwd(),'/saveRuns',sep='')
list.jcq <- list.files(path = def.path, pattern = "jcq")


## -------------- Porner una descripcion --------------

ref = grep("ref",list.jcq)
par(mfcol=c(1, 1), mar=c(4,4,0.5,0.5), oma=c(1,2,1,1))
for(k in 1:length(ref))
{
  tmp.ref = as.data.frame(t(read.table(list.jcq[ref[k]], header = FALSE)))
  tmp.ref[ tmp.ref==0 ] <- NA
  x.range = c(A$year[1]:(A$year[1]+dim(tmp.ref)[1]-1)) 
  if (k<=1) {tmp.ref =  tmp.ref/1000}
  
  matplot(x.range, tmp.ref, type='n', axes=TRUE, ann=TRUE, xlab = 'Year', ylab=list.jcq[ref[k]])
  matlines(x.range, tmp.ref, type='l', lty = "solid", col = "grey")  
  lines(x.range, t(apply(tmp.ref,1,quantile,c(0.05, 0.95),na.rm='TRUE'))[,2], col='red', lty = "dashed")
  lines(x.range, t(apply(tmp.ref,1,quantile,c(0.05, 0.95),na.rm='TRUE'))[,1], col='red', lty = "dashed")
  lines(x.range, apply(tmp.ref,1,median,na.rm='TRUE'), col = "black")
  abline(v = 2010, col = "red", lty = 3)
  
  h <- data.frame(yr = x.range, level = apply(tmp.ref,1,median,na.rm='TRUE'), 
                  th = t(apply(tmp.ref,1,quantile,c(0.4, 0.6),na.rm='TRUE')),
                  th = t(apply(tmp.ref,1,quantile,c(0.25, 0.75),na.rm='TRUE')),
                  th = t(apply(tmp.ref,1,quantile,c(0.05, 0.95),na.rm='TRUE')))
  h <- h 
  
  h1 <- ggplot(h, aes(x = yr))
  h1 <- h1 + geom_ribbon(aes(ymin=th.40., ymax=th.60.), color='red', alpha=0.3, fill='red')
  h1 <- h1 + geom_ribbon(aes(ymin=th.60., ymax=th.75.), color='blue', alpha=0.3, fill='blue')
  h1 <- h1 + geom_ribbon(aes(ymin=th.25., ymax=th.40.), color='blue', alpha=0.3, fill='blue')  
  h1 <- h1 + geom_ribbon(aes(ymin=th.75., ymax=th.95.), color='yellow', alpha=0.3, fill='yellow')
  h1 <- h1 + geom_ribbon(aes(ymin=th.5., ymax=th.25.), color='yellow', alpha=0.3, fill='yellow')
  h1 <- h1 + geom_line(aes(y=level), color='black', size=1)
  h1 <- h1 + theme_bw(15) + geom_vline(xintercept = 2010, colour="red", linetype = "longdash")
  h1 <- h1 + labs(x='Year', y=list.jcq[ref[k]])
  print(h1) 
  
}

## -------------- Porner una descripcion --------------

mse = grep("mse",list.jcq)
par(mfcol=c(1, 1), mar=c(4,4,0.5,0.5), oma=c(1.5,1,1,1))
for(k in 1:length(mse))
{
  tmp.mse = as.data.frame(t(read.table(list.jcq[mse[k]], header = FALSE)))
  tmp.mse[ tmp.mse==0 ] <- NA
  x.range = c(A$year[1]:(A$year[1]+dim(tmp.mse)[1]-1))
  
  if (k<=3) {tmp.mse = tmp.mse/1000}
  
  matplot(x.range, tmp.mse, type='n', axes=TRUE, ann=TRUE, xlab = 'Year', ylab=list.jcq[mse[k]])
  matlines(x.range, tmp.mse, type='l', lty = "solid", col = "grey")  
  lines(x.range, t(apply(tmp.mse,1,quantile,c(0.05, 0.95),na.rm='TRUE'))[,2], col='red', lty = "dashed")
  lines(x.range, t(apply(tmp.mse,1,quantile,c(0.05, 0.95),na.rm='TRUE'))[,1], col='red', lty = "dashed")
  lines(x.range, apply(tmp.mse,1,median,na.rm='TRUE'), col = "black")
  abline(v = 2010, col = "red", lty = 3)
  
  h <- data.frame(yr = x.range, level = apply(tmp.mse,1,median,na.rm='TRUE'), 
                  th = t(apply(tmp.mse,1,quantile,c(0.4, 0.6),na.rm='TRUE')),
                  th = t(apply(tmp.mse,1,quantile,c(0.25, 0.75),na.rm='TRUE')),
                  th = t(apply(tmp.mse,1,quantile,c(0.05, 0.95),na.rm='TRUE')))
  h <- h 
  
  h1 <- ggplot(h, aes(x = yr))
  h1 <- h1 + geom_ribbon(aes(ymin=th.40., ymax=th.60.), color='red', alpha=0.3, fill='red')
  h1 <- h1 + geom_ribbon(aes(ymin=th.60., ymax=th.75.), color='blue', alpha=0.3, fill='blue')
  h1 <- h1 + geom_ribbon(aes(ymin=th.25., ymax=th.40.), color='blue', alpha=0.3, fill='blue')  
  h1 <- h1 + geom_ribbon(aes(ymin=th.75., ymax=th.95.), color='yellow', alpha=0.3, fill='yellow')
  h1 <- h1 + geom_ribbon(aes(ymin=th.5., ymax=th.25.), color='yellow', alpha=0.3, fill='yellow')
  h1 <- h1 + geom_line(aes(y=level), color='black', size=1)
  h1 <- h1 + theme_bw(15) + geom_vline(xintercept = 2010, colour="red", linetype = "longdash")
  h1 <- h1 + labs(x='Year', y=list.jcq[mse[k]])
  print(h1) 

}

## -------------- Porner una descripcion --------------

tmp = grep("tmp",list.jcq)
tmp.op = read.table(list.jcq[tmp[2]], header = FALSE, col.names=c('bo','bt'))
tmp.sam = read.table(list.jcq[tmp[3]], header = FALSE, col.names=c('bo','bt'))

par(mfcol=c(1, 2), mar=c(4,4,0.5,0.5), oma=c(1.5,1,1,1))
lr = log2(tmp.sam/tmp.op)
boxplot(lr, xlab='log2 sam', boxwex = 0.65)
lr = (tmp.sam - tmp.op)/tmp.op
boxplot(lr, xlab='error sam', boxwex = 0.65)

tmp.bt.sam = read.table(list.jcq[tmp[1]], header = FALSE)[,1:dim(tmp.mse)[1]-1]
tmp.bt.ope = read.table(list.jcq[mse[1]], header = FALSE)[,1:dim(tmp.mse)[1]-1]
x.range = c(A$year[1]:(A$year[1]+dim(tmp.mse)[1]-2))

par(mfcol=c(2, 1), mar=c(4,4,0.5,0.5), oma=c(1.5,1,1,1))

matplot(x.range, t(tmp.bt.sam), type='n', axes=TRUE, ann=TRUE, xlab = 'Year', ylab=list.jcq[mse[k]])
matlines(x.range, t(tmp.bt.sam), type='l', lty = "solid", col = "grey")  
lines(x.range, t(apply(t(tmp.bt.sam),1,quantile,c(0.05, 0.95),na.rm='TRUE'))[,2], col='red', lty = "dashed")
lines(x.range, t(apply(t(tmp.bt.sam),1,quantile,c(0.05, 0.95),na.rm='TRUE'))[,1], col='red', lty = "dashed")
lines(x.range, apply(t(tmp.bt.sam),1,median,na.rm='TRUE'), col = "black")
abline(v = 2010, col = "red", lty = 3)

matplot(x.range, t(tmp.bt.ope), type='n', axes=TRUE, ann=TRUE, xlab = 'Year', ylab=list.jcq[mse[k]])
matlines(x.range, t(tmp.bt.ope), type='l', lty = "solid", col = "grey")  
lines(x.range, t(apply(t(tmp.bt.ope),1,quantile,c(0.05, 0.95),na.rm='TRUE'))[,2], col='red', lty = "dashed")
lines(x.range, t(apply(t(tmp.bt.ope),1,quantile,c(0.05, 0.95),na.rm='TRUE'))[,1], col='red', lty = "dashed")
lines(x.range, apply(t(tmp.bt.ope),1,median,na.rm='TRUE'), col = "black")
abline(v = 2010, col = "red", lty = 3)


lr = log2(tmp.bt.sam/tmp.bt.ope)
boxplot(lr, ylab='BT log2 sam', names=x.range, xlab='year', outline = FALSE)
abline(h = 0, v = which(x.range==2010), col = c("black","red"), lty = c(1,3))
lr = (tmp.bt.sam - tmp.bt.ope)/tmp.bt.ope
boxplot(lr, ylab='BT error sam', names=x.range, xlab='year', outline = FALSE)
abline(h = 0, v = which(x.range==2010), col = c("black","red"), lty = c(1,3))


# ## -------------- Porner una descripcion --------------
# 
# rnnd = grep("rnd",list.jcq)
# par(mfcol=c(2, 1), mar=c(4,4,0.5,0.5), oma=c(1.5,1,1,1))
# for(k in 1:length(rnnd))
# {
#   tmp.rnnd = as.data.frame(t(read.table(list.jcq[rnnd[k]], header = FALSE)))
#   tmp.rnnd[ tmp.rnnd==0 ] <- NA
#   x.range = c(A$year[1]:(A$year[1]+dim(tmp.rnnd)[1]-1))
#   matplot(x.range, tmp.rnnd, type='l', ylab=list.jcq[rnnd[k]], xlab='year', col=1)
#   lines(colMeans(tmp.rnnd, na.rm = TRUE), col='white', lwd=3)
#   abline(v = 2010, col = "red", lty = 3)
#   abline(h = 0, col = "red", lty = 3)
#   boxplot(t(tmp.rnnd), names=x.range, outline = FALSE,
#           ylab=list.jcq[rnnd[k]], xlab='year')  
#   abline(v = which(x.range==2010), col = "red", lty = 3)
#   abline(h = 0, col = "red", lty = 3)
# }






