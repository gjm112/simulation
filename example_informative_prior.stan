//
// This Stan program defines a beta-binomial model
//
//

// The input data is y and n.  
data {
  int<lower = 0> y; 
  int<lower = 0> n; 
}

// Parameters
parameters {
  real<lower=0,upper=1> theta;
}

//The model
model {
  y ~ binomial(n, theta);
  //Optional prior would go here. 
  theta ~ beta(20,20);
}

