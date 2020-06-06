# In this file we create Figure 1 showing the search share distributions.

# Author: Lara Vomfell
# Date: 31/05/2020

# -----------------------------------------------------------------------------

# get posterior draws of p_theta
p_theta <- gather_draws(m, p_theta[i,j])
setDT(p_theta, key = "i")

# merge id info so we can plot by time as well for appendix
p_theta <- merge(p_theta, id)

# map j to Asian etc
p_theta[, ethnic_group := j_to_e_lab(j)]
# stat_dots can't handle axis reordering inside ggplot2
p_theta[, y_ticks := forcats::fct_rev(ethnic_group)]

# create density plot
p <- ggplot(p_theta, aes(.value, y_ticks)) +
  stat_halfeyeh(slab_fill = "#90A4AE", 
                slab_color = NA, 
                .width = c(0.5, 0.9),
                fatten_point = 1.1) +
  labs(x = expression(Search~share~p[ite]), y = "") +
  scale_y_discrete(labels = expression(e==White, e==Black,e==Asian)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25))

dsave("p_theta.png", plot = p, width = 4.5, height = 2.5)
