library(tidyverse)
library(forecast)

load("SeaIce.Rdata")

gp=autoplot(y.ts)+xlab("Year")+ylab("Sea Ice Extent (km^2)")
gp

#exploratory analysis: monthly profiles, yearly averages
min(y.ts)
max(y.ts)

data.monthly=aggregate(c(y.ts), list(month = cycle(y.ts)), mean) 
data.annual=aggregate(c(y.ts), list(year = floor(time(y.ts))), mean) 

data.monthly=ts(data.monthly[,2],start=1)
autoplot(data.monthly)+xlab("Month")+ylab("Sea Ice Extent (km^2)")

data.annual=ts(data.annual[,2],start=data.annual[1,1])
autoplot(data.annual)+xlab("data.annual")+ylab("Sea Ice Extent (km^2)")+xlab("Year")
year=time(data.annual)

# analyzing the trend: there is seasonality, we need the harmonics

X_1=fourier(y.ts, K=1)
X_3=fourier(y.ts, K=3)
X_4=fourier(y.ts, K=4)

year.ts=floor(time(y.ts))

mod.h1=tslm(y.ts~X_1+year.ts) 
mod.h3=tslm(y.ts~X_3+year.ts) 
mod.h4=tslm(y.ts~X_4+year.ts) 
summary(mod.h1)
summary(mod.h3)
summary(mod.h4)

res.ts=y.ts-mod.h3$fitted.values

# fitted values
df=data.frame(X=time(y.ts),Y=mod.h3$fitted.values)
gp+geom_line(data=df,aes(X,Y),color="red")

# residuals
autoplot(res.ts)+xlab("Year")+ylab("residual (km^2)")

# predicting for all 2021 and 2022, total 23 months 
# let's start with the trend

mn.hor=22

year.ts=c(kronecker(2021,rep(1,10)),kronecker(2022,rep(1,12)))
trend.predict=forecast(mod.h3,data.frame(fourier(y.ts, K = 3, h =mn.hor),year.ts))

# model selection and forecasting
fit=auto.arima(res.ts,d=0,seasonal=FALSE)
fit

ggAcf(fit$residuals)

forc=forecast(fit,h=mn.hor)

forc$x=forc$x+mod.h3$fitted.values
forc$mean=forc$mean+trend.predict$mean
forc$lower=forc$lower+trend.predict$mean
forc$upper=forc$upper+trend.predict$mean

# plotting the final results
autoplot(forc)+xlab("Year")+ylab("Sea Ice Extent (km^2)")+ggtitle("Forecast of Sea Ice Extent")
autoplot(forc,include=100)+xlab("Year")+ylab("Sea Ice Extent (km^2)")+ggtitle("Forecast of Sea Ice Extent")
autoplot(forc,include=mn.hor)+xlab("Year")+ylab("Sea Ice Extent (km^2)")+ggtitle("Forecast of Sea Ice Extent")


year.ts=floor(time(y.ts))
year.ts2=year.ts^2

mod.h=tslm(y.ts~X_3+year.ts+year.ts2) 
summary(mod.h)

res.ts1=y.ts-mod.h$fitted.values

# fitted values
df=data.frame(X=time(y.ts),Y=mod.h$fitted.values)
gp+geom_line(data=df,aes(X,Y),color="red")

# residuals
autoplot(res.ts1)+xlab("Year")+ylab("residual (km^2)")

# predicting for all 2021 and 2022, total 23 months 
# let's start with the trend

mn.hor=22

year.ts=c(kronecker(2021,rep(1,10)),kronecker(2022,rep(1,12)))
year.ts2=c(kronecker(2021^2,rep(1,10)),kronecker(2022^2,rep(1,12)))
trend.predict1=forecast(mod.h,data.frame(fourier(y.ts, K = 3, h =mn.hor),year.ts, year.ts2))

# model selection and forecasting
fit1=auto.arima(res.ts,d=0,seasonal=FALSE)
fit1

ggAcf(fit1$residuals)

forc1=forecast(fit1,h=mn.hor)

forc1$x=forc1$x+mod.h$fitted.values
forc1$mean=forc1$mean+trend.predict1$mean
forc1$lower=forc1$lower+trend.predict1$mean
forc1$upper=forc1$upper+trend.predict1$mean

# plotting the final results
autoplot(forc1)+xlab("Year")+ylab("Sea Ice Extent (km^2)")+ggtitle("Forecast of Sea Ice Extent")
autoplot(forc1,include=100)+xlab("Year")+ylab("Sea Ice Extent (km^2)")+ggtitle("Forecast of Sea Ice Extent")
autoplot(forc1,include=mn.hor)+xlab("Year")+ylab("Sea Ice Extent (km^2)")+ggtitle("Forecast of Sea Ice Extent")


ar.par=c(0.6)
min(Mod(polyroot(c(1,-ar.par))))

pred.isin=matrix(NaN,nrow=nsim)
p = 1
q = 0

nsim = 500
pred.isin=matrix(NaN,nrow=nsim)
means1 <- c()
n = seq(6, 101, 5)
for(j in 1:length(n)){
for (i in 1:nsim){
  x=arima.sim(list(ar = ar.par),sd = sqrt(1),n = n[j])
  xpres=x[n[j]]
  x=x[1:(n[j]-1)]
  mod=arima(x,order=c(p,0,q),include.mean = FALSE,method="ML")
  forc=forecast(mod,h=1)
  pred.isin[i]=(forc$lower[2] <= xpres & forc$upper[2] >= xpres )
}
  means1[j] <- mean(pred.isin)
}


ar.par=c(0.6,0.2)
ma.par=c(0.8,-0.1)
p=length(ar.par)
q=length(ma.par)
c(min(Mod(polyroot(c(1,-ar.par)))),
  min(Mod(polyroot(c(1,ma.par)))))

nsim=100
pred.isin=matrix(NaN,nrow=nsim)
means2 <- c()
for(j in 1:length(n)){
for (i in 1:nsim){
  x=arima.sim(list(ar = ar.par,ma=ma.par),sd = sqrt(1),n = n[j])
  xpres=x[n[j]]
  x=x[1:(n[j]-1)]
  mod=arima(x,order=c(p,0,q),include.mean = FALSE,method="ML")
  forc=forecast(mod,h=1)
  pred.isin[i]=(forc$lower[2] <= xpres & forc$upper[2] >= xpres )
}
  means2[j] <- mean(pred.isin)
}


ggplot(data = NULL, aes(x=n, y=means1))+
  geom_point()+
  geom_line()
  
ggplot(data = NULL, aes(x=n, y=means2))+
  geom_point()+
  geom_line()




