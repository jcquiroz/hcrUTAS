

rm(list = ls())

library(ggplot2)
library(dplyr)
library(reshape2)


bas.path <- ("/home/jcquiroz/Documents/patagonian_toothfish/toy_model/DelayDiff/HR_eval_1/")
source("read.admb.R")
A <- read.admb("dataOM/DDmod")
def.path <- paste(getwd(),'/dataOM_017',sep='')
list.jcq <- list.files(path = def.path, pattern = "jcq")


## -------------- Outcomes --------------

setwd(def.path)
mse = grep("mse",list.jcq)
sam = grep("tmp",list.jcq)

tmp = read.table(list.jcq[sam[1]], header = FALSE)/1000
yrs <- c(A$year[1]:(A$year[1]+dim(tmp)[2]-1))
mle <- apply(tmp,2,mean,na.rm=TRUE)
prc <- apply(tmp,2,quantile,c(.1,.25,.5,.75,.9),na.rm=TRUE)
bst <- as.data.frame(cbind(yrs,mle,t(prc)))
bsm <-  data.frame(melt(bst,id=1),state='Total Biomass SAM')
bsm[bsm == 0] <- NA

tmp = read.table(list.jcq[mse[1]], header=FALSE)/1000
yrs <- c(A$year[1]:(A$year[1]+dim(tmp)[2]-1))
mle <- apply(tmp,2,mean,na.rm=TRUE)
prc <- apply(tmp,2,quantile,c(.1,.25,.5,.75,.9),na.rm=TRUE)
btt <- as.data.frame(cbind(yrs,mle,t(prc)))
btm <-  data.frame(melt(btt,id=1),state='Total Biomass MSE')

tmp = read.table(list.jcq[mse[2]], header=FALSE)/1e6
yrs <- c(A$year[1]:(A$year[1]+dim(tmp)[2]-1))
mle <- apply(tmp,2,mean,na.rm=TRUE)
prc <- apply(tmp,2,quantile,c(.1,.25,.5,.75,.9),na.rm=TRUE)
att <- as.data.frame(cbind(yrs,mle,t(prc)))
atm <-  data.frame(melt(att,id=1),state='Abundance')

tmp = read.table(list.jcq[mse[3]], header=FALSE)/1e6
yrs <- c(A$year[1]:(A$year[1]+dim(tmp)[2]-1))
mle <- apply(tmp,2,mean,na.rm=TRUE)
prc <- apply(tmp,2,quantile,c(.1,.25,.5,.75,.9),na.rm=TRUE)
rtt <- as.data.frame(cbind(yrs,mle,t(prc)))
rtm <-  data.frame(melt(rtt,id=1),state='Recruitment')

tmp = read.table(list.jcq[mse[4]], header=FALSE)
yrs <- c(A$year[1]:(A$year[1]+dim(tmp)[2]-1))
mle <- apply(tmp,2,mean,na.rm=TRUE)
prc <- apply(tmp,2,quantile,c(.1,.25,.5,.75,.9),na.rm=TRUE)
stt <- as.data.frame(cbind(yrs,mle,t(prc)))
stm <-  data.frame(melt(stt,id=1),state='Survival')
stm[stm == 0] <- NA

## --------------   Figure 1

ttm <- rbind(btm,bsm)
ttp <- dcast(ttm, state + yrs ~ variable)
colnames(ttp) <- c('state','yrs','mle','p10','p25','p50','p75','p90')
ttpL <- subset(ttp, yrs<=2010)
ttpU <- subset(ttp, yrs>=2010)

fig_mse <- ggplot(ttp, aes(yrs,mle))
fig_mse <- fig_mse + geom_blank() + facet_wrap(~state,ncol=2)
fig_mse <- fig_mse + geom_line(data=ttpL, aes(yrs,mle), colour=1)
fig_mse <- fig_mse + geom_pointrange(data=ttpL, mapping=aes(x=yrs, y=mle, 
                                                         ymin=p10, ymax=p90), 
                width=0.1, size=0.8, color="#9999CC", fill="white", shape=24)
fig_mse <- fig_mse + geom_ribbon(data=ttpU, aes(ymin=p25, ymax=p75), 
                                 color='red', alpha=0.3, fill='red')
fig_mse <- fig_mse + geom_ribbon(data=ttpU, aes(ymin=p75, ymax=p90), 
                                 color='blue', alpha=0.3, fill='blue')
fig_mse <- fig_mse + geom_ribbon(data=ttpU, aes(ymin=p10, ymax=p25), 
                                 color='blue', alpha=0.3, fill='blue')  
fig_mse <- fig_mse + geom_line(data=ttpU, aes(yrs,mle), color='black')
fig_mse <- fig_mse + theme_bw(15) + geom_vline(xintercept = 2010, colour="red", linetype = "longdash")
fig_mse <- fig_mse + labs(x='Year', y='')
print(fig_mse) 

## --------------   Figure 2

ttm <- rbind(atm,rtm,stm)
ttp <- dcast(ttm, state + yrs ~ variable)
colnames(ttp) <- c('state','yrs','mle','p10','p25','p50','p75','p90')
ttpL <- subset(ttp, yrs<=2010)
ttpU <- subset(ttp, yrs>=2010)

fig_mse <- ggplot(ttp, aes(yrs,mle))
fig_mse <- fig_mse + geom_blank() + facet_wrap(~state, scale='free',ncol=1)
fig_mse <- fig_mse + geom_line(data=ttpL, aes(yrs,mle), colour=1)
fig_mse <- fig_mse + geom_pointrange(data=ttpL, mapping=aes(x=yrs, y=mle, 
                                                            ymin=p10, ymax=p90), 
                                     width=0.1, size=0.8, color="#9999CC", fill="white", shape=24)
fig_mse <- fig_mse + geom_ribbon(data=ttpU, aes(ymin=p25, ymax=p75), 
                                 color='red', alpha=0.3, fill='red')
fig_mse <- fig_mse + geom_ribbon(data=ttpU, aes(ymin=p75, ymax=p90), 
                                 color='blue', alpha=0.3, fill='blue')
fig_mse <- fig_mse + geom_ribbon(data=ttpU, aes(ymin=p10, ymax=p25), 
                                 color='blue', alpha=0.3, fill='blue')  
fig_mse <- fig_mse + geom_line(data=ttpU, aes(yrs,mle), color='black')
fig_mse <- fig_mse + theme_bw(15) + geom_vline(xintercept = 2010, colour="red", linetype = "longdash")
fig_mse <- fig_mse + labs(x='Year', y='Biomass / Recruitment / Survival (MSE)')
fig_mse2 <- fig_mse
print(fig_mse) 

## --------------   Figure 3

tmp.sam = read.table(list.jcq[sam[1]], header = FALSE)/1000
tmp.sam[tmp.sam == 0] <- NA
tmp.mse = read.table(list.jcq[mse[1]], header=FALSE)/1000
tmp.mse[tmp.mse == 0] <- NA

tmp     = (tmp.sam - tmp.mse)/tmp.sam
yrs <- c(A$year[1]:(A$year[1]+dim(tmp)[2]-1))
mle <- apply(tmp,2,mean,na.rm=TRUE)
prc <- apply(tmp,2,quantile,c(.1,.25,.5,.75,.9),na.rm=TRUE)
ett <- as.data.frame(cbind(yrs,mle,t(prc)))
etm <-  data.frame(melt(ett,id=1),state='Error Total Biomass')

median.quartile <- function(x)
  {
  out <- c( quantile(x, probs = c(0.5,0.5,0.5)))
  names(out) <- c("ymin","y","ymax")
  return(out) 
}

ttpU <- subset(etm, yrs>2010)

p <- ggplot(etm, aes(factor(yrs), value))
p <- p + geom_violin(trim = FALSE, adjust = .5, scale = "count",
                     fill = "#ADFF2F", colour = "#BDB76B") # + coord_flip()
#p <- p + facet_wrap(~variable,ncol=2)
p <- p + geom_violin(data=ttpU, aes(factor(yrs), value), trim = FALSE, adjust = .5, scale = "count",
                     fill = "#FF4500", colour = "#CDCD00") 
p <- p + theme_bw(15) + geom_hline(yintercept = 0, colour="red", linetype = "longdash")
p <- p + labs(x='Year', y='Error Total Biomass') 
p <- p + stat_summary(fun.y=median.quartile,geom='point',colour='#8B4513')
p <- p + theme(text = element_text(size=12),
               axis.text.x = element_text(angle=90, vjust=1)) 
print(p) 

setwd(bas.path)
