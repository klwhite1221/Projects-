View(`2019pitching`)

#Create scatterplots for each of the five predictors vs. Wins 
plot(`2019pitching`$ERA,`2019pitching`$W)
plot(`2019pitching`$R,`2019pitching`$W)
plot(`2019pitching`$SO,`2019pitching`$W)
plot(`2019pitching`$AVG,`2019pitching`$W)
plot(`2019pitching`$WHIP,`2019pitching`$W)

#Test for multicollinearity using the correlation matrix
cor(cbind(`2019pitching`$ERA,`2019pitching`$R,`2019pitching`$SO,`2019pitching`$AVG,`2019pitching`$WHIP))

#Create linear model with all five predictors
mod.pitch<-lm(W~ERA+R+SO+AVG+WHIP,data=`2019pitching`)
summary(mod.pitch)

#Create reduced model with two predictors 
mod.pitch_2<-lm(W~ERA+WHIP,data=`2019pitching`)
summary(mod.pitch_2)

#Run F-test for nested models
F_Stat<-anova(mod.pitch_2,mod.pitch)
F_Stat

#Use stepAIC() to find best-fitting model
library(MASS)
stepAIC(mod.pitch)

#Test for outliers 
rstandard(mod.pitch)
which(abs(rstandard(mod.pitch))>3)

#Test for influential observations
F_thresh<-qf(0.50,6,24)
cooks.distance(mod.pitch)
which(cooks.distance(mod.pitch) > F_thresh)

#Create final model
mod.pitch_final<-lm(W~ERA+SO+AVG,data=`2019pitching`)
summary(mod.pitch_final)

#Create residual plot
plot(mod.pitch_final$fitted.values,mod.pitch_final$residuals)
#Create normal QQ plot
qqnorm(mod.pitch_final$residuals)

#Find critical value for CI's
t<-qt(0.975,26)
#Determine CI's for each slope in final model
CI_ERA_UB<-(-27.81359)+t*(6.23016)
CI_ERA_LB<-(-27.81359)-t*(6.23016)
CI_SO_UB<-(0.03154)+t*(0.01997)
CI_SO_LB<-(0.03154)-t*(0.01997)
CI_AVG_UB<-(410.22615)+t*(273.49593) 
CI_AVG_LB<-(410.22615)-t*(273.49593)
