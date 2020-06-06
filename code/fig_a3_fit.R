# This script reproduces Figure A.3 in appendix A,
# showing observed vs predicted search counts

# Author: Lara Vomfell
# Date: 31/05/2020

# -----------------------------------------------------------------------------


# grab the ground truth search counts
truth <- semester[, .(i = 1:.N, officer_id, team_id, sem_year,
                      stops_asian, stops_white, stops_black)]
truth[, zero := (stops_asian + stops_white + stops_black) == 0L]

# melt into long format
truth <- melt(truth,
              measure.vars = c("stops_asian", "stops_black", "stops_white"), 
              value.name = "count", 
              variable.name = "stops")
truth[, j := .GRP, by = stops]
# set key to speed up merge
setkey(truth, "i", "j")

# get the posterior predicted counts
counts_posterior <- gather_draws(m, counts[i,j])
# set key
setDT(counts_posterior, key = c("i", "j"))

# merge
counts_posterior <- merge(counts_posterior, truth, by = c("i", "j"))

# create ethnic group col
counts_posterior[, facet_lab := j_to_e_lab(j)]

# fix zero counts
counts_posterior[zero == T, .value := 0]

# prediction error
counts_posterior[, error := count - .value]

# pre jitter the data to speed up ggplot2 plotting
counts_posterior[, error_jittered := jitter(error, amount = 0.8)]
counts_posterior[, count_jittered := jitter(count, amount = 0.8)]


# add median predictions
med_counts <- counts_posterior[, .(.value = median(.value)), 
                               by = .(count, i, j, facet_lab)]
med_counts[, med_error := count - .value]

p <- ggplot(counts_posterior,
            aes(count_jittered, error_jittered)) +
  geom_hline(yintercept = 0, color = "grey35") +
  geom_point(shape = 21,
             fill = NA,
             color = "#78909C", 
             size = .4,
             alpha = .5) +
  geom_point(data = med_counts, 
             aes(count, med_error), 
             shape = 21, fill = NA, color = "black", size = .7) +
  facet_wrap(~ facet_lab, labeller = label_parsed) +
  labs(x = expression(paste("Observed number of searches ", y[ite])), 
       y = "Prediction error") +
  scale_y_continuous(breaks = seq(-100, 100, by = 20), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(0, 180, by = 20), minor_breaks = NULL)

dsave("error_counts_med.png", plot = p, width = 8.5, height = 3)
