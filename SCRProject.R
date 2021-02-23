heart <- read.csv("heart.csv")
head(heart)
dim(heart)

n = dim(heart)[1]
X = as.matrix(heart[,c("ï..age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", 
"exang", "oldpeak", "slope", "ca", "thal")])
X = cbind(rep(1,n), X)
Y = as.vector(heart[,"target"])

iter = 10
beta = matrix(NA, iter, dim(X)[2])
W = matrix(0, n, n)

i = 1
beta[i,] = rep(0.0,dim(X)[2])

for (i in 1:(iter-1)) {
  pi = exp(X %*% beta[i,])/(1 + exp(X %*% beta[i,]))
  for (j in 1:n) {
    W[j,j] = pi[j] * (1 - pi[j]) 
  }
  cov.matrix = solve(t(X) %*% W %*% X)
  beta[i+1,] = beta[i,] + cov.matrix %*% t(X) %*% (Y - pi)
}
beta

logitall <- glm(target ~ ., data=heart, family="binomial")
summary(logitall)
logit <- glm(target ~ ï..age + trestbps + chol + thalach + oldpeak, data = heart, family = "binomial")
summary(logit)


X = as.matrix(heart[,c("ï..age", "trestbps", "chol", "thalach", 
                       "oldpeak")])
X = cbind(rep(1,n), X)
Y = as.vector(heart[,"target"])

iter = 10
beta = matrix(NA, iter, dim(X)[2])
W = matrix(0, n, n)

i = 1
beta[i,] = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

for (i in 1:(iter-1)) {
  pi = exp(X %*% beta[i,])/(1 + exp(X %*% beta[i,]))
  for (j in 1:n) {
    W[j,j] = pi[j] * (1 - pi[j]) 
  }
  cov.matrix = solve(t(X) %*% W %*% X)
  beta[i+1,] = beta[i,] + cov.matrix %*% t(X) %*% (Y - pi)
}
beta




n = dim(heart)[1]
X = as.matrix(heart[,c("ï..age", "sex", "cp", "trestbps", "chol", "fbs", "restecg")])
X = cbind(rep(1,n), X)
Y = as.vector(heart[,"target"])

iter = 10
beta = matrix(NA, iter, dim(X)[2])
W = matrix(0, n, n)

i = 1
beta[i,] = rep(0.0,dim(X)[2])

for (i in 1:(iter-1)) {
  pi = exp(X %*% beta[i,])/(1 + exp(X %*% beta[i,]))
  for (j in 1:n) {
    W[j,j] = pi[j] * (1 - pi[j]) 
  }
  cov.matrix = solve(t(X) %*% W %*% X)
  beta[i+1,] = beta[i,] + cov.matrix %*% t(X) %*% (Y - pi)
}
beta


logit <- glm(target ~ ï..age + sex + cp + trestbps + chol + fbs + restecg, data = heart, family = "binomial")
summary(logit)
