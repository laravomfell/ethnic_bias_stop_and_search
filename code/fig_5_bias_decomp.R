# This script reproduces Figure 5 in the main text,
# showing the overall bias decomposition as violin plots

# Author: Lara Vomfell
# Date: 31/05/2020

# Our original script includes the construction of the force-level disparities
# file based on our data. Since we cannot make those files available, 
# we instead provide the final numbers in the 'force_level.csv'
# file loaded at the beginning.

# We then obtain the posterior draws for p_theta (search shares) and 
# rho (patrol shares) and then calculate the 3 terms (officer bias, 
# patrol ratio, search ratio) for each draw

# Lastly, we calculate the medians labeled on the figure.

# -----------------------------------------------------------------------------

# load file holding force-level disparities
force_level <- fread("data/force_level.csv")
# reverse j_to_e for fast merging
force_level[, j := ifelse(ethnic_group == "Asian", 1, 
                          ifelse(ethnic_group == "Black", 2, 3))]

# Now get the officer-level disparities
officer_level <- gather_draws(m, c(p_theta, rho)[i,j])
setDT(officer_level)

# put into wide format for easy matching with force level data 
# and calculation of the three terms
officer_level <- dcast(officer_level, i + j + .draw ~ .variable, 
                       value.var = ".value")

# combine with force level data
officer_level <- merge(officer_level, force_level, by = "j")

# calculate the three terms
officer_level[, ":="(
  overall = searches_share/pop_share,
  officer_bias = p_theta/rho,
  patrol = rho/pop_share,
  aggr = searches_share/p_theta
)]


# melt into long for faster plotting
bias_long <- melt(officer_level, 
                  id.vars = c("i", "j", ".draw", "ethnic_group"),
                  measure.vars = c("overall", "officer_bias", "patrol", "aggr"),
                  value.name = "value", variable.name = "type")

# recode the term labels
bias_long[, type_lab := factor(type, 
                               levels = c("overall", "officer_bias", 
                                          "patrol", "aggr"),
                               labels = c("Overall\nover-\nsearching", "Officer\nbias", 
                                          "Over-\npatrolling", "Aggre-\ngation"))]

# get median shares
medians <- officer_level[, .(searches_share = searches_share[1],
                             pop_share = pop_share[1],
                             p_theta = median(p_theta),
                             rho = median(rho)), 
                         by = ethnic_group]

# calculate terms based on medians
medians[, ":=" (
  overall = searches_share/pop_share,
  officer_bias = p_theta/rho,
  patrol = rho/pop_share,
  aggr = searches_share/p_theta
)]

# melt into long format to play nice with ggplot
medians <- melt(medians, 
                id.vars = "ethnic_group", 
                measure.vars = c("overall", "officer_bias", "patrol", "aggr"))
medians[, value_print := round(value, 2)]
# position on xaxis
medians[, x := rep(seq(1, 4, by = 1), each = 3)]

# I need to manually fiddle in the equal and multipl signs
signs <- CJ(ethnic_group = c("Asian", "Black", "White"), 
            x = c(1.5, 2.5, 3.5))
signs[, value_print := rep(c("=", "x", "x"), 3)]

# combine
medians <- rbind(medians, signs, use.names = T, fill = T)
# position on y axis
medians[, y := 0.25]

# the extra labeling
medians <- rbind(medians, 
                 data.table(ethnic_group = c("Asian", "Black", "White"),
                            x = 2, y = 0.31, value_print = "Median:"),
                 use.names = T, fill = T)

p <- ggplot(bias_long, aes(type_lab, value)) + 
  geom_hline(yintercept = 1, color = "grey55") +
  stat_eye(.width = c(.5, .9), fill = "#90A4AE", scale = 15) + 
  coord_cartesian(ylim = c(0.25, 5.5)) + 
  facet_wrap(~ ethnic_group) +
  scale_y_log10(breaks = c(0.3, 0.5, 1, 2, 3, 5)) +
  labs(x = "", y = "Value") +
  geom_text(data = medians, aes(x,y, label = value_print))

dsave("bias_decomp.png", plot = p, width = 10, height = 3.5)
