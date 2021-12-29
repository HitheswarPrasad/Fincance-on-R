n.sim <- 100
x.1 <- rgamma(n.sim, 0.5, 0.2)
x.2 <- rlnorm(n.sim, 0.15, 0.25)
hist(x.1)
hist(x.2)
x<-cbind(x.1,x.2)
#Response vector y as
y <- 1.5 * x.1 + 0.8 * x.2 + rnorm(n.sim,4.2, 5.03)
#Here we have to fins th ecolumn vector Beta a 2by 1 matrix to find the model parameter
hist(y)
#Solving for Beta
X <- cbind(x.1, x.2)
XTX.inverse <- solve(t(X) %*% X)
(beta.hat <- XTX.inverse %*% t(X) %*% y)