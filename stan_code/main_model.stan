// We estimate a vector theta = [theta1, theta2, 0]'
// giving the multinomial allocation of search counts y into E bins.
// We model theta as a function of covariates and 
// two latent shares, rho and zeta.
// Those are in turn also vector of multinomial allocation of 
// crime suspects and residents.

data {
  // sample size
  int<lower=1> N;
  // number of ethnic groups to model
  int<lower=1> E;
  
  // outcome y: an array of search counts
  int y[N,E];
  // summed outcome y: the total number of search counts by each officer
  int y_sum[N];

  // number of regressors w/o intercept
  int K;
  // array of E-1 holding NxK covariate matrices 
  // (E - 1) because we set theta3 to 0.
  matrix[N,K] X[E-1];
  
  // we also have a hierarchical team effect:
  // number of teams
  int J;
  // team id for hierarchical effect
  int<lower=1,upper=J> team[N];
  
  // and a hierarchical team effect
  vector[N] white_share;
  
  // Next, we give arrays of counts used to model 
  // the latent shares:
  // crime suspect counts for modeling latent shares
  int S[N,E];
  // patrolling counts for modeling latent census shares
  int P[N,E];
}

transformed data {
  // multinomial_rng in the generated quantities doesn't like zero counts.
  // To circumvent errors, we add +1 to zero entries
  int y_sum1[N] = y_sum;

  for (n in 1:N){
    if (y_sum1[n] == 0) y_sum1[n] = 1;
  }
}

parameters {
  // regression coefficients
  vector[K] beta[E - 1];
  
  // coefficients for latent crime shares
  vector[E - 1] gamma;
  // coefficients for latent patrolling shares
  vector[E - 1] delta;
  
  // now we model the team effects as hierarchical intercepts 
  // plus white share for each ethnic group
  vector[J] alpha[E - 1];
  // coef for white share
  real w[E-1];
  
  // scale param for mu (hierarchical term)
  real<lower=0> s_alpha[E - 1];
  real m_alpha[E - 1];
  
  // latent suspect share vector
  simplex[E] zeta[N];
  
  // latent residential population share vector
  simplex[E] rho[N];
  
}

transformed parameters{
  // matrix holding the theta we actually estimate
  matrix[N, E - 1] theta_raw;
  // matrix holding theta_raw + a vector of 0s
  matrix[N, E] theta;
  
  
  // model theta_raw as a function of intercept_j, Xb, 
  // gamma * crime, delta * patrol, w * white_share
  for (e in 1:(E - 1)){
    // alpha[e] +
    theta_raw[, e] =  alpha[e, team] + X[e] * beta[e] + gamma[e] * to_vector(zeta[, e]) + delta[e] * to_vector(rho[, e]) + w[e] * white_share;
  }
  
  // set theta3 to zero
  theta = append_col(theta_raw, rep_vector(0, N));
}

model {
  // Priors
  
  // prior on hierarchical intercepts
  s_alpha ~ normal(0, 1);
  m_alpha ~ normal(0, 1);

  for (e in 1:(E - 1)){
    alpha[e] ~ normal(m_alpha[e], s_alpha[e]);
    beta[e] ~ normal(0, 2);
  }
  
  gamma ~ normal(0, 2);
  delta ~ normal(0, 2);
  w ~ normal(0, 2);
  
  // weakly informative prior on zeta using England-wide data on shares of 
  // ethnicities in crime
  for (n in 1:N){
    // dirichlet(0.43, 0.61, 5.00) corresponds to p = [0.07, 0.10, 0.82]
    zeta[n] ~ dirichlet([0.43, 0.61, 5.00]');
  }
  
  // weakly informative prior on eta using UK-wide data on 
  // population shares
  for (n in 1:N){
    // dirichlet(0.39, 0.21, 5.00) corresponds to p = [0.07, 0.04, 0.89]
    rho[n] ~ dirichlet([0.39, 0.21, 5.00]');
  }
  
  
  // Model estimation
  
  for (n in 1:N){
    // lik of the latent suspect share zeta
    S[n] ~ multinomial(zeta[n]);
    // lik of the latent patrolling share rho
    P[n] ~ multinomial(rho[n]);
    // lik of the final model
    y[n] ~ multinomial(softmax(to_vector(theta[n])));
  }

}

generated quantities {
  // I'm interested in four quantities:
  // 1. the vector of multinomial probabilities
  // 2. DS = search share/suspect share
  // 3. DP = search share/patrol share
  // 4. posterior predicted search counts

  // 1. holds the fitted thetas
  simplex[E] p_theta[N];

  // 2. holds the log ratio wrt crime suspect
  vector[E] DS[N];

  // 3. holds the log ratio wrt patrol
  vector[E] DP[N];

  // 4. predicted counts
  int counts[N,E];

  // 1.
  for (n in 1:N){
    p_theta[n] = softmax(to_vector(theta[n]));
  }

  // 2.
  for (n in 1:N){
    DS[n] = exp(log(p_theta[n]) - log(zeta[n]));
  }

  // 3.
  for (n in 1:N){
    DP[n] = exp(log(p_theta[n]) - log(rho[n]));
  }

  // 4.
  for (n in 1:N){
    counts[n] = multinomial_rng(p_theta[n], y_sum1[n]);
  }

}
