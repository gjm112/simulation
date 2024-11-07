data <- list(y = 5200, n = 10000)

library(rstan)
fit_hector <- stan(
  file = "example_hector.stan",  # Stan program
  data = data,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)


library(bayesplot)
mcmc_areas(fit_hector,
           pars = c("theta"),
           prob = 0.8) 

mean(fit_hector@sim$samples[[1]]$theta > 0.5)

#Nevada Prob
0.63125

#PA prob
0.5715

n <- 1000
1.96*sqrt((0.5*0.5)/n)





#Pennsylvania
alpha <- 3458229		
beta <- 3377674
alpha + beta 

#Nevada
alpha <- 703486	
beta <- 669890
alpha /(alpha + beta)
alpha + beta

x <- seq(0,1,0.001)
y <- dbeta(x,alpha,beta)
plot(x,y, type = "l", col = "red")
