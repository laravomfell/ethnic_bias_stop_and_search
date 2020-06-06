# In this file we run AR(1) models on the officer timeseries 
# of D^S and D^P. 
# Note that we run these models are various points of aggregation of 
# D^S and D^P: at the median and at the upper and lower 90% intervals

# Author: Lara Vomfell
# Date: 31/05/2020

# -----------------------------------------------------------------------------

# depends on 'fig_3_4_disp.R' or just uncomment this line
# disp_posterior <- gather_draws(m, c(DS, DP)[i,j])
# setDT(disp_posterior)
d_posterior <- disp_posterior[, median_qi(.value, 
										  .width = c(0, .9), 
                                          .simple_names = T), 
							  by = .(i, j, .variable)]

d_posterior <- merge(d_posterior, id, by = "i")

# now I want to estimate the AR parameters at various points in the distribution
# so I define the following data.table to create the conditions
# (CJ is the same as expand.grid w/o the stupid factor conversion)
conditions <- CJ(var = c("DS", "DP"),
                 width = 0.9,
                 endpoint = c(".lower", ".upper"))

# the median is still missing so I manually add that in
conditions <- rbind(data.table(var = c("DS", "DP"),
                               width = 0,
                               endpoint = ".value"),
                    conditions)
# give each condition a name
conditions[, name := paste0(tolower(var), "_", 
                            gsub("\\.", "", endpoint), width * 100)]

# pmap here means: for each row in conditions, get the corresponding
# subset from disp_posterior and put it in the list
# the ... absorbs unused components 
d_quant <- pmap(conditions, function(var, width, endpoint, ...) 
									 d_posterior[.variable == var &
												 .width == width,
												 .(.value = get(endpoint), 
												    i, j,
													officer_id, 
													sem_year)])

names(d_quant) <- conditions$name

# now we need to make a few transformations to this data to make it
# ready for the AR(1) model

# first, reshape the long format into wide format in terms of ethnicity
# ie put the ethnicity information into separate columns

d_quant <- map(d_quant,
               function(x) dcast(x, i + officer_id + sem_year ~ paste0("e", j),
                                 value.var = ".value"))

# next, we need all time series to start at 1 to play nice with Stan
d_quant <- map(d_quant, function(x) x[, tid := seq_len(.N), by = officer_id])

# Next, we need an indicator when the officer series end
d_quant <- map(d_quant, function(x) x[, last_t := as.numeric(tid == max(tid)), by = officer_id])

# Finally, create a list where each element is a list containing
# the data needed to run the stan model
# by excluding the first observation for y and dropping the last for 
# y_lag we automatically supplied a lagged y array
l_d_quant <- map(d_quant, function(x) 
  list(
    # y exclusing the first time point (tid == 1)
    y = x[tid != 1, .(e1, e2, e3)],
    # lagged y, excluding the last observation
    y_lag = x[last_t == 0L, .(e1, e2, e3)],
    E = 3,
    N = nrow(x[tid != 1]),
    I = uniqueN(x$officer_id),
    id = x[tid != 1, officer_id]
))

# load stan model only once
ar_model <- stan_model("stan_code/ar1.stan")

# list to hold the results
ar_results <- vector(mode = "list", length = length(l_d_quant))
names(ar_results) <- names(l_d_quant)

# Ideally, one wouldn't loop here and use map instead
# but the loop has the advantage that you can kill the loop at any time
# without losing the information
# Hence, the loop is quite chatty and tells you what it's up to
for (i in seq_along(l_d_quant)){
  tic("this loop took")
  ar_results[[i]] = sampling(ar_model, 
                             data = l_d_quant[[i]],
                             iter = 2000,
                             warmup = 1000,
                             cores = 4,
                             chains = 4, save_warmup = F,
                             control = list(adapt_delta = .9))
  print(paste("Finished model", i))
  toc()
}

# the loop has the disadvantage that warning messages get printed at the end
ar_nuts <- map(ar_results, nuts_params)
# count divergent transitions and return vector
div_trans <- map_dbl(ar_nuts, function(x) sum(x[x$Parameter == "divergent__",]$Value))
if (any(div_trans > 0)){
  print(paste0("Caution! Some model(s) didn't converge. Check nr. ", which(div_trans > 0)))
}
