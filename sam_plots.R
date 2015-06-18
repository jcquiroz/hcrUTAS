library(ggplot2)
library(dplyr)
library(reshape)

source("read.admb.R")
A <- read.admb("DDmod")


df <- data.frame(year=A$year,
                 bt = A$bt,
                 rt = A$rt,
                 nt = A$nt,
                 ct = A$ct,
                 chat = A$chat,
                 epsilon=A$epsilon,
                 nu = A$nu,
                 delta= A$delta,
                 psi  = A$psi,
                 wt = A$wt,
                 what=A$what,
                 ft = A$ft,
                 fdev = A$fdev,
                 cpue = A$cpue,
                 yt = A$yt,
                 psi = A$psi)

p <- ggplot(df,aes(year,bt/1000)) + geom_line()
p <- p + labs(x="Year",y="Biomass [t]")
print(p + theme_bw())

p <- ggplot(df,aes(year,cpue))+geom_point()
p <- p + geom_line(data=df,aes(year,yt))
p <- p + labs(x="Year",y="CPUE")
print(p + theme_bw())

p <- ggplot(df,aes(year,wt))+geom_point()
p <- p + geom_line(data=df,aes(year,what))
p <- p + labs(x="Year",y="Average Weight [Kg]")
print(p + theme_bw())

p <- ggplot(df,aes(year,ct/1000))+geom_point()
p <- p + geom_line(data=df,aes(year,chat/1000))
p <- p + labs(x="Year",y="CATCH [t]")
print(p + theme_bw())

p <- ggplot(df,aes(year,ft))+geom_line()
p <- p + labs(x="Year",y="Fishing mortality rate")
print(p + theme_bw())

p <- ggplot(df,aes(year,rt/1e6))+geom_line()
p <- p + labs(x="Year",y="Recruitment [mill]")
print(p + theme_bw())

mdf <- melt(df,id.var="year")
ssdf <- mdf %>% subset(variable %in% c("epsilon","nu","delta","psi"))
p <- ggplot(ssdf,aes(year,value)) + geom_line() + geom_hline(aes(yintercept=0))
p <- p + facet_wrap(~variable, scale='free')
print(p + theme_bw())

# runSim<- function(n=10)
# {
#         system("rm SimPars.rep")
#         for(iter in 1:n)
#         {
#                 arg = paste("./DDmod -nox -est -sim", iter)
#                 system(arg);
#         }
# }

# library(PBSmodelling)
# runSim()
# simula <- read.table('SimPars.rep')
# colnames(simula) <- c('iter','bo','reck','m','sigmaEps','sigmaMu')
# plotTrace(simula[,1:3],lwd=3)
# plotTrace(simula[,c(1,4:6)],lwd=3)




