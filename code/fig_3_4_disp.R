# In this file we create Figure 3 in the main text,
# showing the densities of D^S and D^P,
# and Figure 4, showing posterior probability mass above 1

# Author: Lara Vomfell
# Date: 31/05/2020

# Note that in Figure 3 we log-transform D to get a nice 
# symmetrical distribution

# Figure 3: Density of D^S and D^P --------------------------------------------

# get posterior draws
disp_posterior <- gather_draws(m, c(DS, DP)[i,j])
setDT(disp_posterior, key = c("i", "j"))

# merge id info in
disp_posterior <- merge(disp_posterior, id, by = "i")
# format j
disp_posterior[, ethnic_group := j_to_e_lab(j)]
disp_posterior[, y_ticks := forcats::fct_rev(ethnic_group)]

# use plot math to get nice thetas on facet labels
disp_posterior[, var_lab := ifelse(.variable == "DS", 
                                   "italic(D)[ite]^{italic(S)}", 
                                   "italic(D)[ite]^{italic(P)}")]

disp_posterior[, var_lab := factor(var_lab, 
                                   levels = c("italic(D)[ite]^{italic(S)}", 
                                              "italic(D)[ite]^{italic(P)}"))]
# log-transformed D
p <- ggplot(disp_posterior, aes(.value, y_ticks)) +
  # vertical line at 1
  geom_vline(xintercept = 1, color = "grey35") +
  stat_halfeyeh(slab_fill = "#90A4AE", slab_color = NA, 
                .width = c(0.5, 0.9), fatten_point = 1.25) +
  facet_wrap(~ var_lab, labeller = label_parsed) +
  scale_y_discrete(labels = expression(e==White, e==Black,e==Asian)) +
  scale_x_log10(breaks = c(0.3, 0.5, 1, 2, 5, 10)) +
  coord_cartesian(xlim = c(.3, 13)) +
  labs(x = "Disparity", y = "")

dsave("log_disp.png", plot = p, width = 6.5, height = 3)


# the summaries reported in the figure caption

# which percentage is outside [0.3, 13]?
disp_posterior[, (sum(.value < 0.3 | .value > 13)/.N) * 100]


# Figure 4: Posterior probability > 1 -----------------------------------------

# calculate % over 1 for each officer x ethnicity
# so I need to calculate how many draws I have 
# per officer x ethnicity x variable
disp_posterior[, n_officer := .N, by = .(officer_id, j, .variable)]

above1 <- disp_posterior[, .(share1 = sum(.value > 1)/n_officer), 
                         by = .(officer_id, j, .variable)]
above1 <- unique(above1)

above1[, y_facet := j_to_e_lab(j)]

above1[, x_facet := ifelse(.variable == "DS",
                           "italic(D)[ite]^{italic(S)}", 
                           "italic(D)[ite]^{italic(P)}")]
above1[, x_facet := forcats::fct_rev(x_facet)]

p <- ggplot(above1, aes(share1)) + 
  geom_vline(xintercept = 0.5, color = "grey50") +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "#90A4AE") + 
  facet_grid(y_facet ~ x_facet, labeller = label_parsed, switch = "y") +
  scale_y_continuous(position = "right") +
  labs(x = expression(Probability~mass~of~italic(D)~above~1), 
       y = "Number of officers\n")

dsave("perc_biased.png", plot = p, height = 4, width = 5.5)

# Data summaries reported in the text -----------------------------------------

# medians of the distributions
x = disp_posterior[, median_qi(.value, .width = .9), by = .(j, .variable)]
x[, lapply(.SD, round, 2), by = .(j, .variable, .width, .point, .interval)]

# percentage above 95% for Black
above1[.variable == "DP" & j == 2, 
       .(number = sum(share1 >= .95), 
         share = sum(share1 >= .95)/.N)]

