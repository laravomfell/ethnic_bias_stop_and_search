# This script reproduces Figure A.4 in appendix A,
# showing the posterior densities of the AR(1) coefficients

# Author: Lara Vomfell
# Date: 31/05/2020

# This script assumes that you have succesfully run 'apx_a4_ar_model.R'
# and have a list 'ar_results' in your environment

# -----------------------------------------------------------------------------

# gather AR coefficients
ar_b <- map(ar_results, 
            function(x) gather_draws(x, b[j,i]))

ar_b <- rbindlist(ar_b, idcol = "location")

# set ethnic group
ar_b[, y_ticks := j_to_e(j)]
# stat_dots can't handle axis reordering inside ggplot2
ar_b[, y_ticks := forcats::fct_rev(y_ticks)]

# create nice facet labels
# first, split location into D plus distr location
ar_b[, c("which_d", "where") := tstrsplit(location, "_")]
ar_b[, nice_d := ifelse(which_d == "rd", "D^S", "D^P")]
# now get 'location' to parse nicely and set ordering
ar_b[, nice_location := factor(
  location,
  levels = c("ds_lower90",
             "ds_value0",
             "ds_upper90",
             "dp_lower90",
             "dp_value0",
             "dp_upper90"),
  labels = c("D^S~at~lower~'90%'~interval",
             "D^S~at~median",
             "D^S~at~upper~'90%'~interval",
             "D^P~at~lower~'90%'~interval",
             "D^P~at~median",
             "D^P~at~upper~'90%'~interval")
)]


p <- ggplot(ar_b, aes(.value, y_ticks)) +
  geom_vline(xintercept = 0, color = "grey35") +
  stat_halfeyeh(slab_fill = "#90A4AE", 
                .width = c(0.5, 0.8), 
                fatten_point = 1.1, 
                normalize = "xy") +
  facet_wrap(~ nice_location, labeller = label_parsed, ncol = 3) +
  scale_y_discrete(labels = expression(e==White, e==Black,e==Asian)) +
  labs(y = "", x = expression(b[ie])) +
  coord_cartesian(xlim = c(-1, 1))

dsave("ar_coef_all.png", plot = p, width = 7.5, height = 4.5)
