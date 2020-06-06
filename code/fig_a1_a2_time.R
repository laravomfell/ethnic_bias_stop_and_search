# This file creates Figures A.1 and A.2 from Appendix A,
# the time-separated versions of Fig 1 and 3

# Author: Lara Vomfell
# Date: 31/05/2020

# Note that these files depend on the posterior draws
# 'p_theta' from file 'fig_1_p_theta.R' and
# 'disp_posterior' from file 'fig_3_log_disp.R'


# A.1: Time-separated p_theta -------------------------------------------------

p <- ggplot(p_theta, aes(factor(sem_num), .value)) +
  stat_halfeye(slab_fill = "#90A4AE", 
               slab_color = NA, 
               .width = c(0.5, 0.9), 
               fatten_point = 1.1)+
  facet_wrap(~ ethnic_group, labeller = label_parsed) +
  labs(x = "Time in half-years", 
       y = expression(Search~share~p[ite])) +
  ylim(c(0,1))

dsave("p_theta_time.png", plot = p, width = 9, height = 3.5)

# A.2: Time-separated D^S and D^P ---------------------------------------------

vars <- c("DS", "DP")
y_labs <- c("italic(D)[ite]^{italic(S)}", "italic(D)[ite]^{italic(P)}")
y_labs <- purrr::set_names(y_labs, vars)

p <- map(vars, function(x){
  ggplot(disp_posterior[.variable == x],
         aes(factor(sem_num), .value)) +
    # make line at 1 slightly darker
    geom_hline(yintercept = 1,  color = "grey35") +
    # draw densities + intervals
    stat_halfeye(slab_fill = "#90A4AE", slab_color = NA, 
                 .width = c(0.5, 0.9), fatten_point = 1.25) +
    facet_wrap(~ ethnic_group, labeller = label_parsed) +
    scale_y_log10(minor_breaks = FALSE) +
    # breaks = c(0.5, 1, 1.5, 2, 3, 4),
    labs(x = "Time in half-years", y = parse(text = y_labs[x])) +
    # cut out some extreme values
    coord_cartesian(ylim = c(0.3, 13))
})

p <- egg::ggarrange(plots = p, nrow = 2, draw = F)
dsave("log_disp_time.png", plot = p, width = 8, height = 5)
