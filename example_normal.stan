//
// This Stan program defines a beta-binomial model
//
//

// The input data is a vector y
data {
  int<lower = 0> n;
  vector[n] y; 
   
}

// Parameters
parameters {
  real<lower=0> sigma;
  real mu;
}

//The model
model {
  y ~ normal(mu, sigma);
  //Optional prior would go here. 
  //Default prior is uniform
}

