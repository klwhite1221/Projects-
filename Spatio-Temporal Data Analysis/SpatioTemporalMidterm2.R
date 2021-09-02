library(geostatsp)
library(geoR)
library(fields)
library(tidyverse)
library(sp)

data("wolfcamp")

load("borders.Rdata")

geodata <- wolfcamp

z <- geodata$data
x <- geodata$coords[, 1]
y <- geodata$coords[, 2]


df=data.frame(lat=y,long=x, Z=z)
p=ggplot()+geom_point( data=df, aes(x=x, y=y, size = Z, color=Z))
p=p + labs(x = "Longitude",y = "Latitude",size = "pressure head (m)",colour = "pressure head (m)")+ coord_fixed()
p=p + theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())
p

df=data.frame(X=z)
df2=data.frame(X=log(z))
ggplot()+geom_histogram(data=df, aes(x=X,y=..density..), binwidth=75,color="black", fill="white")+xlab("pressure head (m)")
ggplot()+geom_histogram(data=df2, aes(x=X,y=..density..),binwidth=0.25,color="black", fill="white")+xlab("log pressure head (log m)")


df=data.frame(X=x,Y=y,Z=z)
ggplot(df,aes(X, Z))+geom_point()+ylab("pressure head (m)")+xlab("longitude")
ggplot(df,aes(Y, Z))+geom_point()+ylab("pressure head (m)")+xlab("latitude")



mod.log.xy=lm(log(z)~x+y)
summary(mod.log.xy)


## storing the data
geodata$data.orig=geodata$data
geodata$data=residuals(mod.log.xy)



## setting up the prediction grid
gr = pred_grid(borders,by=3)
gr.bor=locations.inside(loc=gr, borders=borders)


gr.pred=data.frame(x=gr.bor[,1],y=gr.bor[,2])

pred=predict(mod.log.xy,newdata=gr.pred,interval="prediction",level=0.95)
geodata$trend=pred[,1]
sd_pred=(pred[,3]-pred[,1])/1.96



## plotting residuals


df=data.frame(lat=geodata$coords[, 2],long=geodata$coords[, 1],Z=geodata$data)
p=ggplot()+geom_point( data=df, aes(x=long, y=lat, size = Z))
p=p + labs(x = "Longitude",y = "Latitude",size = "residuals (m)")+ coord_fixed()
p


df=data.frame(X=geodata$data)
ggplot()+geom_histogram(data=df, aes(x=X,y=..density..),color="black", binwidth= 0.05, fill="white")+xlab("residuals")


max(dist(geodata$coords))
md = 437
lz.v = variog(geodata,max.dist=md)
lz.v.a0 = variog(geodata,max.dist=md,direction=0)
lz.v.a45 = variog(geodata,max.dist=md,direction=pi/4)
lz.v.a90 = variog(geodata,max.dist=md,direction=pi/2)
lz.v.a135 = variog(geodata,max.dist=md,direction=3/4*pi)

plot(lz.v,pch=19)
lines(lz.v.a0,col="red",lwd=2)
lines(lz.v.a45,col="blue",lwd=2)
lines(lz.v.a90,col="red",lty=2,lwd=2)
lines(lz.v.a135,col="blue",lty=2,lwd=2)
legend(100,lz.v$v[2],col=c("red","blue","red","blue"),lty=c(1,1,2,2),c(0,45,90,135),lwd=c(2,2,2,2),cex=0.8)


xy = cbind(x,y)

u =  seq(0, 250, 50)

theta = 0

t = 0.393

C <- function(u, xy, z, theta, t){
  
  n <- length(u)
  
  c <- rep(0, n-1)
  
  Nk <- rep(0, n-1)
  
  for(j in 1:n-1){
    
    for(i in 1:length(z)){
      
      for (m in 1:length(z)){
        d = sqrt(((xy[i, 1]-xy[m, 1])^2)+((xy[i, 2]-xy[m, 2])^2))
        if(i != m){
          a = acos((c(xy[i,1], xy[i,2])%*%c(xy[m,2],xy[m,2]))/(norm(xy[i, ])*norm(xy[m, ])))
        }
        flag = (d > u[j]) && (d < u[j+1]) && (a > (theta-t)) && (a <= (theta+t))
        
        if(flag){
          c[j] <- c[j] + sum(z[i]*z[m])
          Nk[j] <- Nk[j] + sum(flag)
        }
      }
    }
  }
  
  return(c/Nk)
  
}

dcv1 <- C(u, xy, z, theta, t)
dcv2 <- C(u, xy, z, pi/4, t)
dcv3 <- C(u, xy, z, pi/2, t)
dcv4 <- C(u, xy, z, 3*pi/4, t)

plot(dcv1, pch = 19, ylab = "Directional Covariance") 
lines(dcv1,col="red",lwd=2)
plot(dcv2, pch = 19, ylab = "Directional Covariance") 
lines(dcv2,col="blue",lwd=2)
plot(dcv3, pch = 19, ylab = "Directional Covariance")
lines(dcv3,col="red",lty=2,lwd=2)
plot(dcv4, pch = 19, ylab = "Directional Covariance")
lines(dcv4,col="blue",lty=2,lwd=2)




geodata.vgm = variog(geodata,max.dist=390)
geodata.vgm$n
plot(geodata.vgm, pch = 19)


u =  c(0, geodata.vgm$u)


C <- function(u, xy, z){
n_u=length(u)
C=rep(0,n_u-1)
Nk=rep(0,n_u-1)
n = length(z)

for (j in 1:(n_u-1)){
  for (i in 1:n){
    d=sqrt((xy[i,1]-xy[i:n,1])^2+(xy[i,2]-xy[i:n,2])^2)  
    flag=(d>=u[j])&(d<u[j+1]) 
    if (sum(flag)>0){
      C[j]=C[j]+sum(z[i]*z[i:n][flag])
      Nk[j]=Nk[j]+sum(flag)
    }
  }
}
return(C/Nk)
}

C(u, xy, z)
plot(C(u, xy, z), pch=19)


geodata.vgm = variog(geodata,max.dist=390)
geodata.vgm$n
plot(geodata.vgm, pch = 19)

### 2) Fit variogram model, with nugget
geodata.fit.mat = variofit(geodata.vgm,ini=c(var(geodata$data),120),cov.model="matern",fix.nugget = F,kap=0.5,fix.kappa = T)
geodata.fit.sph = variofit(geodata.vgm,ini=c(var(geodata$data),120),cov.model="spherical")
geodata.fit.lin = variofit(geodata.vgm,ini=c(var(geodata$data),120),cov.model="linear")


### 3) Plot for visual inspection
plot(geodata.vgm,pch=19)
lines(geodata.fit.mat,col="red",lwd=2)
lines(geodata.fit.sph,col="blue",lwd=2)
lines(geodata.fit.lin,col="green",lwd=2)
legend(100,geodata.vgm$v[2],lty=1,col=c("red","blue","green"),c("mat","sph","lin"))


## kriging commands
KC = krige.control(type="sk",obj.model = geodata.fit.mat)
sk =  krige.conv(geodata, loc = gr, borders = borders,krige=KC)

############ residual plots


## plotting kriging point estimates
## remember these are the RESIDUALS!
quilt.plot(gr.bor[,1],gr.bor[,2], sk$predict)
points(geodata,pch=,add=T)

## plotting kriging standard deviation
## remember these are the RESIDUALS!
quilt.plot(gr.bor[,1],gr.bor[,2], sqrt(sk$krige.var))
points(geodata,pch=,add=T)



## Confidence intervals
Z_CI_l=exp(geodata$trend+sk$predict-1.96*sqrt(sk$krige.var))
Z_CI_u=exp(geodata$trend+sk$predict+1.96*sqrt(sk$krige.var))
zlim_l=min(Z_CI_l)
zlim_u=max(Z_CI_u)


## plotting kriging lower and upper bound
quilt.plot(gr.bor[,1],gr.bor[,2], Z_CI_l,zlim=c(zlim_l,zlim_u))
points(geodata,pch=,add=T)

## plotting kriging lower and upper bound
quilt.plot(gr.bor[,1],gr.bor[,2], Z_CI_u,zlim=c(zlim_l,zlim_u))
points(geodata,pch=,add=T)

## point estimates 
prect.pred=exp(geodata$trend+sk$predict+sk$krige.var/2)

quilt.plot(gr.bor[,1],gr.bor[,2], prect.pred)
points(geodata,pch=,add=T)


# plotting the variance in the original scale
mu=geodata$trend+sk$predict
sigma=sqrt(sd_pred^2+sk$krige.var)
or.var=(exp(sigma^2)-1)*exp(2*mu+sigma^2)

quilt.plot(gr.bor[,1],gr.bor[,2], sqrt(or.var))
points(geodata,pch=,add=T)

d1 <- c ()
d2 <- c()
for(i in 1:length(gr.bor[,1])){
  d1[i] <- sqrt(((xy[15, 1]-gr.bor[i, 1])^2)+((xy[15, 2]-gr.bor[i, 2])^2))
  d2[i] <- sqrt(((xy[59, 1]-gr.bor[i, 1])^2)+((xy[59, 2]-gr.bor[i, 2])^2))
}

which.min(d1)
which.min(d2)

est = (geodata$trend[8621]+sk$predict[8621])-(geodata$trend[9327]+sk$predict[9327])
d = sqrt(((gr.bor[8621, 1]-gr.bor[9327, 1])^2)+((gr.bor[8621, 2]-gr.bor[9327, 2])^2))
uvec = seq(0, 405.5534, 57.93962)
geodata.vgm2 = variog(geodata,option = "bin",uvec=uvec)
geodata.fit.mat2 = variofit(geodata.vgm2,ini=c(var(geodata$data),50),cov.model="matern",fix.nugget = F,kap=0.5,fix.kappa = T)
#var = geodata.fit.mat2$cov.pars[1]
var = geodata.vgm2$v[2]
Z_CI_l=exp(est-1.96*sqrt(var))
Z_CI_u=exp(est+1.96*sqrt(var))
c(Z_CI_l, Z_CI_u)
 

