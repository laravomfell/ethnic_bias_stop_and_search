# In this file we take the data loaded in data_prep.R
# and make it 'stan-ready', i.e. put everything in a list
# that Stan can deal with and then run the model

# Note that the model takes around 35h to run

# Author: Lara Vomfell
# Date: 31/05/2020

# -----------------------------------------------------------------------------

# First, create the formula used to create the model matrices
f <- formula(~ officer_gender + officer_age + officer_service)

# we use dummies for officer ethnicity
fs <- list(
  update(f, ~ . + officer_asian),
  update(f, ~ . + officer_black)
)

# create model matrices, dropping the intercept because 
# that's already in the stan model
X <- map(fs, function(formula){
  model.matrix(formula, semester)[, -1]
})


S <- semester[, .(incidents_asian, incidents_black, incidents_white)]
P <- semester[, .(asian, black, white)]


# Stan-ready list -------------------------------------------------------------

data_list <- list(
  N = nrow(semester),
  E = 3,
  y = semester[, .(stops_asian,
                   stops_black,
                   stops_white)],
  y_sum = rowSums(semester[, .(stops_asian,
                               stops_black,
                               stops_white)]),
  K = unique(unlist(map(X, ncol))),
  X = X,
  J = uniqueN(semester$team_id),
  team = semester$team_id,
  S = S,
  P = P,
  white_share = semester$white_share
)

# Stan model ------------------------------------------------------------------

# begin sampling
m <- stan(file = "stan_code/main_model.stan",
          data = data_list,
          # don't store theta
          pars = "theta",
          include = FALSE,
          iter = 2000,
          warmup = 1000,
          cores = 4,
          chains = 4,
          save_warmup = FALSE,
          control = list(adapt_delta = 0.9))
