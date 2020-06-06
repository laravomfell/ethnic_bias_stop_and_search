# This file creates Table A.1 from Appendix A,
# summarizing the coefficients in table format

# Author: Lara Vomfell
# Date: 31/05/2020

# Note that this file assumes that you have already run 'fig_coef.R'
# and obtained the posterior summaries of the coefficients

# -----------------------------------------------------------------------------

# coef_summarized gives the median and 90% intervals
# then dcast by ethnicity
coef_posterior[, ethnic_group := j_to_e(j)]
coef_summarized <- coef_posterior[, median_qi(.value, 
                                              .width = .9, 
                                              .simple_names = T), 
                                  by = .(plot_var, ethnic_group)]

coef_wide <- dcast(coef_summarized,
                   plot_var ~ ethnic_group, 
                   value.var = c(".value", ".lower", ".upper"))

# round the values to two decimal points
coef_wide <- coef_wide[, lapply(.SD, round, digits = 2), by = plot_var]

# Some table formatting nonsense to get latex to align everything

# which is the widest any number gets?
n_max <- max(nchar(unlist(coef_wide)))
# now pad the columns to match n_max
coef_wide <- coef_wide[, lapply(.SD, formatC, digits = 2, format = "f", width = n_max),
                       by = plot_var]
# now collapse the intervals
# because latex doesn't respect double whitespace, do it with tilde
coef_wide[, q90_black := paste0("[", .lower_Black, ",~", .upper_Black, "]")]
coef_wide[, q90_asian := paste0("[", .lower_Asian, ",~", .upper_Asian, "]")]

# I had to reverse the plot_var factor for ggplot to behave, 
# now undo that
setorder(coef_wide, -plot_var)
setnames(coef_wide, gsub(".value", "Median", colnames(coef_wide)))

# keep ony the columns I need
coef_wide <- coef_wide[, .(plot_var, Median_Asian, q90_asian, Median_Black, q90_black)]

cap <- "Estimates and 90\\% uncertainty intervals (UI) for model parameters in \\eqnref{eq:theta}. The estimates are also displayed graphically in \\figref{fig:coefs}. Officer age and experience are standardised.\\label{tbl:regr}"

# turn into latex code
print(xtable::xtable(coef_wide,
                     caption = cap,
                     align = c("l", "l", rep("r", ncol(coef_wide) - 1))),
      include.rownames = FALSE,
      file = "results/ugly_regression_table.tex")
