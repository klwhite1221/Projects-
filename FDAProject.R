library(fda)

data(lip)
head(lip)

time = 1:51
rng = range(time)
knots  <- time
norder <- 4
nbasis <- length(knots) + norder - 2
bbasis <- create.bspline.basis(rng, nbasis, norder, knots)

par = fdPar(bbasis,Lfdobj=int2Lfd(2),lambda=1e-12)
fd = smooth.basis(time, lip, par)$fd

matplot (time, lip, type="l", lty=1, cex=2,
         lwd=1,  xlab = "Time", ylab="Lip Position")
lines(fd, col="blue")

landmark = rep(NA,dim(lip)[2])
for(i in 1:dim(lip)[2]){
  landmark[i] = which.min(lip[, i])
  points(time[landmark[i]], lip[landmark[i],i])
}
landmark

ldmk = landmarkreg(fd, landmark, mean(landmark), par)

r.lip = eval.fd(time, ldmk$regfd)
matplot(time, r.lip, type="l", lty=1,
        xlab="Time", ylab="Lip Position")
mr.lip = apply(r.lip, 1, mean)
lines(time, mr.lip, type="l", lty=1, lwd=3)

w.lip = eval.fd(time, ldmk$warpfd)
matplot(time, w.lip, type="l", lty=1, xlab="time", ylab="warping")

cfd = center.fd(fd)
c.lip = eval.fd(time, cfd)
matplot(time, c.lip, type="l", lty=1,
        xlab="Time", ylab="Lip Position")

rcfd = center.fd(ldmk$regfd)
rc.lip = eval.fd(time, rcfd)
matplot(time, rc.lip, type="l", lty=1,
        xlab="time", ylab="Lip Position")

varprop = sum(diag(inprod(rcfd,rcfd)))/sum(diag(inprod(cfd,cfd)))
varprop


par = fdPar(bbasis,Lfdobj=int2Lfd(2),lambda=1e-12)
fd = smooth.basis(time, lip[, 1:5], par)$fd

matplot (time, lip[, 1:5], type="l", lty=1, cex=2,
         lwd=1,  xlab = "Time", ylab="Lip Position")
lines(fd, col="blue")

landmark = rep(NA,5)
for(i in 1:5){
  landmark[i] = which.min(lip[, i])
  points(time[landmark[i]], lip[landmark[i],i])
}
landmark

ldmk = landmarkreg(fd, landmark, mean(landmark), par)

r.lip = eval.fd(time, ldmk$regfd)
matplot(time, r.lip, type="l", lty=1,
        xlab="Time", ylab="Lip Position")
mr.lip = apply(r.lip, 1, mean)
lines(time, mr.lip, type="l", lty=1, lwd=3)

w.lip = eval.fd(time, ldmk$warpfd)
matplot(time, w.lip, type="l", lty=1, xlab="time", ylab="warping")

cfd = center.fd(fd)
c.lip = eval.fd(time, cfd)
matplot(time, c.lip, type="l", lty=1,
        xlab="Time", ylab="Lip Position")

rcfd = center.fd(ldmk$regfd)
rc.lip = eval.fd(time, rcfd)
matplot(time, rc.lip, type="l", lty=1,
        xlab="time", ylab="Lip Position")

varprop = sum(diag(inprod(rcfd,rcfd)))/sum(diag(inprod(cfd,cfd)))
varprop


