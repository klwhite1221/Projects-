ca <- read_csv("ca.csv")
View(ca)

summary(polr(as.factor(DAST_CAT) ~ FENT_USE+BUP_USE, data=ca, Hess=TRUE))

p <- polr(as.factor(DAST_CAT) ~ FENT_USE+BUP_USE, data=ca, Hess=TRUE)
exp(coef(p))

p <- vglm(as.factor(DAST_CAT) ~ FENT_USE+BUP_USE, family = propodds, data=ca)

summary(p)

NMU <- polr(as.factor(DAST_CAT) ~ FENT_NMU+BUP_NMU+METH_NMU+MORPH_NMU+OXY_NMU+OXYM_NMU+TRAM_NMU+TAP_NMU+COD_NMU+HYD_NMU+HYDM_NMU+SUF_NMU, data=ca, Hess=TRUE)
USE <- polr(as.factor(DAST_CAT) ~ FENT_USE+BUP_USE+METH_USE+MORPH_USE+OXY_USE+OXYM_USE+TRAM_USE+TAP_USE+COD_USE+HYD_USE+HYDM_USE+SUF_USE, data=ca, Hess=TRUE)
USE_AIC <- stepAIC(USE)
USE2 <- polr(as.factor(DAST_CAT) ~ FENT_USE+BUP_USE+METH_USE+MORPH_USE+OXY_USE+OXYM_USE+TAP_USE+COD_USE+HYD_USE+HYDM_USE, data=ca, Hess=TRUE)

ggplot(data = ca, )

mUSE <- multinom(as.factor(DAST_CAT) ~ FENT_USE+BUP_USE+METH_USE+MORPH_USE+OXY_USE+OXYM_USE+TRAM_USE+TAP_USE+COD_USE+HYD_USE+HYDM_USE+SUF_USE, data=ca, HESS=TRUE)


M1 <- logLik(USE)
M2 <- logLik(mUSE)
G <- -2*(M1[1] - M2[1])
pchisq(G,3,lower.tail = FALSE)


medication <- ca %>% 
  select(FENT_USE, BUP_USE, METH_USE, MORPH_USE, OXY_USE, OXYM_USE, TRAM_USE, TAP_USE, COD_USE, HYD_USE, HYDM_USE, SUF_USE)


predict(USE2,newdata = data.frame(FENT_USE = 0, BUP_USE = 0, METH_USE = 0, MORPH_USE = 1, OXY_USE =0 , OXYM_USE = 0, TAP_USE = 0, COD_USE = 0, HYD_USE = 0, HYDM_USE = 1),type="p")
predict(USE2,newdata = data.frame(FENT_USE = 0, BUP_USE = 0, METH_USE = 0, MORPH_USE = 0, OXY_USE =0 , OXYM_USE = 0, TAP_USE = 0, COD_USE = 0, HYD_USE = 0, HYDM_USE = 0),type="p")
predict(USE2,newdata = data.frame(FENT_USE = 1, BUP_USE = 1, METH_USE = 1, MORPH_USE = 1, OXY_USE =1 , OXYM_USE = 1, TAP_USE = 1, COD_USE = 1, HYD_USE = 1, HYDM_USE = 1),type="p")
predict(USE2,newdata = data.frame(FENT_USE = 1, BUP_USE = 0, METH_USE = 0, MORPH_USE = 0, OXY_USE =0 , OXYM_USE = 0, TAP_USE = 0, COD_USE = 0, HYD_USE = 0, HYDM_USE = 0),type="p")


USE2_fv <- USE2$fitted.values

ggplot(data=NULL, aes(USE2_fv[,1]))+
  geom_histogram(binwidth = 0.05, boundary = 0)+
  labs(x = "Probability of Having DAST_CAT=1")

ggplot(data=NULL, aes(USE2_fv[,2]))+
  geom_histogram(binwidth = 0.05, boundary = 0)+
  labs(x = "Probability of Having DAST_CAT=2")

ggplot(data=NULL, aes(USE2_fv[,3]))+
  geom_histogram(binwidth = 0.01, boundary = 0)+
  labs(x = "Probability of Having DAST_CAT=3")

ggplot(data=NULL, aes(USE2_fv[,4]))+
  geom_histogram(binwidth = 0.01, boundary = 0)+
  labs(x = "Probability of Having DAST_CAT=4")

ggplot(data=NULL, aes(USE2_fv[,5]))+
  geom_histogram(binwidth = 0.01, boundary = 0)+
  labs(x = "Probability of Having DAST_CAT=5")


bins <- seq(0, 1, 0.05)
dc1 <- cut(USE2_fv[, 1], bins, include.lowest = TRUE)
transform(table(dc1))
dc2 <- cut(USE2_fv[, 2], bins, include.lowest = TRUE)
transform(table(dc2))


dc3 <- cut(USE2_fv[, 3], bins, include.lowest = TRUE)
transform(table(dc3))
dc4 <- cut(USE2_fv[, 4], bins, include.lowest = TRUE)
transform(table(dc4))
dc5 <- cut(USE2_fv[, 5], bins, include.lowest = TRUE)
transform(table(dc5))


