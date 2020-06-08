# This script reproduces Figure A.5 in appendix A,
# showing search volume relative to crime and diversity
# on LSOA level

# Author: Lara Vomfell
# Date: 31/05/2020

# This is based on file 'lsoa.csv' giving the number of searches and crimes
# per LSOA. This is based on confidential data given to us so we 
# anonymized the 2011 ONS LSOA name with a generic id

# -----------------------------------------------------------------------------

lsoa <- fread("data/lsoa.csv")

# calculate relative contribution of each area
lsoa[, rel_size := n_searches/sum(n_searches)]

p <- ggplot(lsoa, aes(1-white_share,n_searches/n_crimes * 1000, size = rel_size)) + 
  geom_point(shape = 21, alpha = .5, fill = "#90A4AE") +
  scale_size_continuous(name = "Share of searches on total search volume") +
  labs(x = "Non-White share", y = "Number of searches per\n1,000 reported crimes") +
  theme(legend.position = "bottom") +
  guides(size = guide_legend(title.position = "top"))

dsave("lsoa.png", width = 6, height = 3.5)
