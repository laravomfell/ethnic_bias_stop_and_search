# This file creates Fig 2 in our paper, the one with the posterior 
# quantile intervals of relevant coefficients

# Author: Lara Vomfell
# Date: 31/05/2020

# -----------------------------------------------------------------------------

# helper function to rename the variables
fct_coefs <- function(x){
  levels <- c("m_alpha",
              "beta1",
              "beta2",
              "beta3",
              "beta4",
              "w",
              "gamma",
              "delta")
  labels <- c("Global intercept",
              "Female officer",
              "Officer age",
              "Officer experience",
              "Officer of same ethnicity",
              "White share in team",
              "Officer-level suspect share",
              "Officer-level patrol share")
  forcats::fct_rev(factor(x, levels, labels))
}

# -----------------------------------------------------------------------------

# first, collect the posterior draws of the relevant coefficients
coef_posterior <- gather_draws(m, m_alpha[j], beta[j,i], gamma[j], delta[j], w[j])

# and format the variable column a little
setDT(coef_posterior)
coef_posterior[, i := as.character(i)]
coef_posterior[is.na(i), i := ""]
coef_posterior[, plot_var := paste0(.variable, i)]

# rename and order coefficients 
coef_posterior[, plot_var := fct_coefs(plot_var)]

# rename j
coef_posterior[, facet_lab := ifelse(j == 1, "theta[Asian]", "theta[Black]")]

# create the zoomed in plot
# (there's technically a version of ggforce::facet_zoom but I couldn't
# get it to work. The grey "zoom" is done in photoshop)
p1 <- ggplot(coef_posterior[.variable != "beta"], aes(.value, plot_var)) + 
  geom_rect(aes(xmin = -0.25, xmax = 0.25, ymin = -Inf, ymax = Inf), fill = "grey90") +
  geom_vline(xintercept = 0, color = "grey55") +
  stat_halfeyeh(fill = "#90A4AE", fatten_point = 1.1, .width = c(.5, .9)) + 
  facet_wrap(~ facet_lab, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(-4, 12, by = 2)) +
  labs(x = "Effect size", y = "") +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, -10), "pt"))
p2 <- ggplot(coef_posterior[.variable == "beta"], aes(.value, plot_var)) + 
  geom_rect(aes(xmin = -0.25, xmax = 0.25, ymin = -Inf, ymax = Inf), fill = "grey90") +
  geom_vline(xintercept = 0, color = "grey55") +
  stat_halfeyeh(fill = "#90A4AE", fatten_point = 1.1, .width = c(.5, .9)) + 
  facet_wrap(~ facet_lab, labeller = label_parsed) +
  scale_x_continuous(breaks = seq(-4, 12, by = 0.25)) +
  labs(x = "Effect size", y = "") +
  coord_cartesian(xlim = c(-.25, .25)) +
  theme(plot.margin = unit(c(5.5, 5.5, 5.5, -10), "pt"))

p <- egg::ggarrange(p1, p2, nrow = 2, draw = F)
dsave("coef_sep.png", plot = p, height = 5, width = 7.5)
