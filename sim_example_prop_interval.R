#Simulation study.  
#Create a population
p <- 0.5
n <- 30
alpha <- 0.05
nsim <- 100000


dat <- data.frame()
nvec <- c(5, 10, 20, 30, 50, 100, 500)
pvec <- c(0.01, seq(0.05,0.95,0.05),0.99)

for (n in nvec){print(n)
  for (p in pvec){print(p)
init <- rep(NA,nsim)
for (i in 1:nsim){
x <- rbinom(n,1,p)
phat <- mean(x)
ub <- phat + qnorm(1-alpha/2)*sqrt(phat*(1-phat)/n)
lb <- phat - qnorm(1-alpha/2)*sqrt(phat*(1-phat)/n)

init[i] <- (p > lb & p < ub)
}


dat <- rbind(dat,data.frame(n, p , cp = mean(init)))


  }
}


ggplot(aes(x = n, y = cp, color = as.factor(p)), data = dat) + 
  geom_path() + 
  geom_point() + geom_hline(yintercept = .95, color = "hotpink")



ggplot(aes(x = p, y = cp, color = as.factor(n)), data = dat) + 
  geom_path() + 
  geom_point() + geom_hline(yintercept = .95, color = "hotpink") + ylim(0.9,1)
