library(tidyverse)
getwd()
da <- read_csv("time_corr.csv")
rtn.var1 <- ts(da[["var1"]],start=c(1976,2),frequency = 12)
rtn.var2 <- ts(da[["var2"]],start=c(1976,2),frequency = 12)
plot(
  rtn.var1,
  main="rtn of var1",
  major.ticks="years",minor.ticks=NULL,
  grid.ticks.on="years",
  col="red"
)
plot(
  rtn.var2,
  main="rtn of var2",
  major.ticks="years",minor.ticks=NULL,
  grid.ticks.on="years",
  col="red"
)
acf(rtn.var1,lag.max=30,main="")
acf(rtn.var2,lag.max=30,main="")
library(fGarch)
sigvar1 <- garchFit(~1+garch(1,1),data=rtn.var1,trace=FALSE)
summary(sigvar1)
sigvar2 <- garchFit(~1+garch(1,1),data=rtn.var2,trace=FALSE)
summary(sigvar2)
# Now calculate the time-varied covariance
plus <- garchFit(~1+garch(1,1),data=rtn.var1+rtn.var2,trace=FALSE)
minus <- garchFit(~1+garch(1,1),data=rtn.var1-rtn.var2,trace=FALSE)
cov <- 1/4*(volatility(plus)^2 - volatility(minus)^2)
rho <- cov / volatility(sigvar1) / volatility(sigvar2)
rho <- ts(rho,start=c(1976,2),frequency = 12)
plot(rho,type="l",xlab="year",ylab="correlation",col="red")
correlation <- cor(da["var1"],da["var2"])
abline(h=correlation,lty=4,col="blue")
abline(h=0,lty=4,col="black")
write.csv(x=rho,file="rho.csv")
