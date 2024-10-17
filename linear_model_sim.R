#What are the assumptions of a linear model.  
#Let's see how violations of these assumptions affect results of linear model.  

nsim <- 4070
beta0int <- beta1int <- data.frame()
betahat <- matrix(NA, ncol = 2, nrow = nsim)
set.seed(1234)
for (i in 1:nsim){
n <- 200 
x <- rnorm(n, 0, 1)
beta0 <- 3
beta1 <- 4
sigma <- 7
y <- beta0 + beta1*x + rnorm(n,0,sigma)

mod <- lm(y ~ x)
betahat[i,] <- summary(mod)$coef[,1]


beta0int <- rbind(beta0int,confint(mod)[1,])
beta1int <- rbind(beta1int,confint(mod)[2,])
}

mean(beta0int[,1] < 3 & beta0int[,2] > 3)
mean(beta1int[,1] < 4 & beta1int[,2] > 4)


colMeans(betahat)
apply(betahat,2,mean)

hist(betahat[,1])
hist(betahat[,2])




#######################################################
#Now we are breaking the normality assumption
#######################################################


nsim <- 10000
beta0int <- beta1int <- data.frame()
betahat <- matrix(NA, ncol = 2, nrow = nsim)
set.seed(1234)
for (i in 1:nsim){print(i)
  n <- 200 
  x <- rnorm(n, 0, 1)
  beta0 <- 3
  beta1 <- 4
  sigma <- 7
  y <- beta0 + beta1*x + rt(n,5)
  
  mod <- lm(y ~ x)
  betahat[i,] <- summary(mod)$coef[,1]
  
  
  beta0int <- rbind(beta0int,confint(mod)[1,])
  beta1int <- rbind(beta1int,confint(mod)[2,])
}

colMeans(betahat)
apply(betahat,2,mean)

mean(beta0int[,1] < 3 & beta0int[,2] > 3)
mean(beta1int[,1] < 4 & beta1int[,2] > 4)


colMeans(betahat)
apply(betahat,2,mean)

hist(betahat[,1])
hist(betahat[,2])



#######################################################
#Now we are breaking the normality assumption
#######################################################
nsim <- 10000
beta0int <- beta1int <- data.frame()
betahat <- matrix(NA, ncol = 2, nrow = nsim)
set.seed(1234)
for (i in 1:nsim){print(i)
  n <- 200 
  x <- abs(rnorm(n, 0, 1))
  beta0 <- 3
  beta1 <- 4
  sigma <- 7
  y <- beta0 + beta1*x + rnorm(n,0,sigma*(1+abs(3*x)))
  
  mod <- lm(y ~ x)
  betahat[i,] <- summary(mod)$coef[,1]
  
  
  beta0int <- rbind(beta0int,confint(mod)[1,])
  beta1int <- rbind(beta1int,confint(mod)[2,])
}

colMeans(betahat)
apply(betahat,2,mean)

mean(beta0int[,1] < 3 & beta0int[,2] > 3)
mean(beta1int[,1] < 4 & beta1int[,2] > 4)


colMeans(betahat)
apply(betahat,2,mean)

hist(betahat[,1])
hist(betahat[,2])