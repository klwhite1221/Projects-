library(sp)
library(spdep)
library(splines)
library(grid)
library(INLA)
library(tidyverse)
library(RColorBrewer)

load("lipscotland.Rdata")

summary(lipscotland$observed) 

lipscotland$observed.plot=lipscotland$observed
brks.observed=c(0,5,10,15,20,25,30,35,40)
spplot(lipscotland, "observed.plot", at = brks.observed)
grid.text("Observed Cases", x=unit(0.8, "npc"), y=unit(0.50, "npc"), rot=90)

summary(lipscotland$expected) 

lipscotland$expected.plot=lipscotland$expected
spplot(lipscotland, "expected.plot")
grid.text("Expected Cases", x=unit(0.8, "npc"), y=unit(0.50, "npc"), rot=90)

summary(lipscotland$pcaff)
lipscotland$pcaff.plot=lipscotland$pcaff
spplot(lipscotland, "pcaff.plot")
#grid.text("Percentage of the Population Employed in Agriculutre, Fishing , or Forestry", x=unit(0.8, "npc"), y=unit(0.50, "npc"), rot=90)

### Analysis
# nonspatial model

form <- observed ~ offset(log(expected))+pcaff
model <- glm(formula=form, data=lipscotland, family = poisson)
summary(model)

# Moran's I and Geary's C

resid.model <- residuals(model)
moran.mc(x=resid.model, listw=W.list, nsim=1000)
geary.mc(x=resid.model, listw=W.list, nsim=1000)

############ spatial model (with INLA)

# first we need to standardize W so that each entry is not 1, but 1/N_i, N_i being the total number of entries for each row
Wsd=matrix(0,nrow=dim(W)[1],ncol=dim(W)[1])
for (i in 1:dim(W)[1]){
  Wsd[i,]=W[i,]/sum(W[i,])
} 
# then we multiply by the maximum eigenvalue, so that generic1 allows the usual CAR representation
Wsd=Wsd*max(eigen(Wsd)$value)

data.inla=lipscotland@data
data.inla$ID=1:dim(W)[1]

mod.car <- inla(update(form, . ~. +
                         f(ID, model = "generic1", Cmatrix = Wsd)), 
                family="poisson",data = data.inla,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(compute = TRUE))
summary(mod.car)


mod.car$summary.fixed
#let's get the marginals posteriors for the fixed effects (the betas)
#marginal posterior intercept 
df=data.frame(X=mod.car$marginals.fixed[[1]][,1],Y=mod.car$marginals.fixed[[1]][,2])
gp=ggplot()+geom_line(data=df,aes(X,Y),color=c("red"),size=1.2)+xlab("Intercept")+ylab("density")
gp


df=data.frame(X=mod.car$marginals.fixed[[2]][,1],Y=mod.car$marginals.fixed[[2]][,2])
gp=ggplot()+geom_line(data=df,aes(X,Y),color=c("red"),size=1.2)+xlab("Percentage of the Population Employed in Agriculutre, Fishing , or Forestry")+ylab("density")
gp

mod.car$summary.hyperpar



# spatial variance
post.tau2 = inla.tmarginal(function (x) exp(-x), mod.car$internal.marginals.hyperpar[[1]])
inla.zmarginal(post.tau2)

# posterior plot for tau2
df=data.frame(X=post.tau2[,1],Y=post.tau2[,2])
gp=ggplot()+geom_line(data=df,aes(X,Y),color=c("red"),size=1.2)+xlab("tau2")+ylab("density")
gp

# rho of the CAR for the spatial effect
df=data.frame(X=mod.car$marginals.hyperpar[[2]][,1],Y=mod.car$marginals.hyperpar[[2]][,2])
gp=ggplot()+geom_line(data=df,aes(X,Y),color=c("red"),size=1.2)+xlab("rho")+ylab("density")
gp


# summary of the posterior means for the random effects

summary(mod.car$summary.random$ID[,"mean"])
#they are really small compared to the mean, indicating again small spatial dependence

# plotting the posterior means for the fitted values now

lipscotland$fitted.car = mod.car$summary.fitted.values[,"mean"]
brks.observed=c(0,5,10,15,20,25,30,35,40)
spplot(lipscotland, "fitted.car",  at = brks.observed, col.regions = rev(brewer.pal(8,"RdBu")))


