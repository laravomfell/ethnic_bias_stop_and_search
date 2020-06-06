// Here we estimate an officer- and ethnicity-specific AR(1) model 
// of the form y_ite = a_ie + b_ie * y_i(t-1)e + epsilon_ite
// For better identification (few t per officer), we model a_ie and b_ie as draws
// from an ethnicity-specific distribution over officers with mean b_loc_e and sd s_b_e

// Since our data structure is ragged (unequal number of time points per officer)
// the easiest modeling approach we identified is providing y without t = 1
// similarly giving y_lag to the model directly
data {
  // sample size (I officers * (T_i - 1 timepoints))
  int<lower=0> N;
  // number of officers
  int<lower=0> I;
  // which officer each observation belongs to
  int id[N];
  // number of ethnic groups
  int E;
  // our outcome: a vector of size E with N observations of log risk
  // y without the first observation already
  // y_lag without the last observation
  vector[E] y[N];
  vector[E] y_lag[N];

}

parameters {
  // for all parameters we use a hierarchical setting which means that 
  // we have (param)_loc plus the actual officer-specific parameters
  // for simplification, we do not also estimate the variances of the 
  // hierarchical terms
  
  
  // the intercept in AR(1)
  real a_loc[E];
  vector[I] a[E];
  
  // we use a non-centered param of b, the AR(1) coefficient
  vector<lower=0,upper=1>[E] b_loc_raw;
  vector[I] b[E];
  
  // the variance is also hierarchical
  real<lower=0> sigma_y_loc[E];
  real<lower=0> sigma_y[E,I];
  
}

transformed parameters {
  // the non centered parametrization of b
  // this ensures that b in [-1,1]
  vector[E] b_loc = 2 * b_loc_raw - 1;

}

model {
  
  // priors for AR(1)
  
  // N(0,1) for the intercept
  a_loc ~ normal(0, 1);
  // beta(5,5) prior on b_loc_raw corresponds to 
  // prior centered on 0 for b_loc
  b_loc_raw ~ beta(5, 5);
  // prior on variance of sigma_y
  sigma_y_loc ~ normal(0, 1);
  
  
  // sample from hierchical prior
  for (e in 1:E){
    b[e] ~ normal(b_loc[e], 0.25);
    a[e] ~ normal(a_loc[e], 0.25);
    sigma_y[e] ~ normal(0, sigma_y_loc[e]);
  }
  
  // estimate y = a + b * y_t-1
  for (e in 1:E){
    log(y[, e]) ~ normal(a[e,id] + b[e,id] .* to_vector(y_lag[, e]), sigma_y[e,id]);
    target += -log(to_vector(y[, e]));
  }
}

