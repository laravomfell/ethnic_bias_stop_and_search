# This is a run script for all of the analyses for our
# revision of "Officer bias..." for Nature Human Behaviour.

# Author: Lara Vomfell
# Date: 05/06/2020

# The run script first loads and prepares the data, 
# runs the Stan model and then creates all figures and tables in the paper
# in the order in which they appear

# Unfortunately, we cannot share the original data on which our analysis
# based. Instead, we generate synthetic data closely resembling the
# original data. This data needs to be generated only once.

# Note that there are some dependencies between files 
# ie 'tbl_a1_coef_table.R' depends on posterior draws 
# collected in 'fig_2_coef.R'
# The run script ensures that everything is executed in the correct order
# but we note those dependencies in the files.

# Preliminaries ---------------------------------------------------------------

# required libraries
library(data.table)
library(purrr)
library(rstan)
library(bayesplot)
library(forcats)
library(tidybayes)
library(magrittr)
library(ggplot2)
library(tictoc)
library(scales)
library(egg)
library(xtable)
options(bitmapType='cairo')
# recommended options for rstan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# A few plot settings

# a default ggplot2 theme
theme_print <- function(){
  theme_light() +
  theme(text = element_text(size = 14),
        plot.background = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(),
        strip.text = element_text(colour = 'black'),
        strip.background = element_rect(fill = "lightgray"),
        panel.spacing = unit(1.2, "lines"),
        panel.grid = element_blank())
}
theme_set(theme_print())

# default path for saving plots using cairo-png for optimal resolution
dsave <- function(file_name, file_path = "results/", ...){
  ggsave(filename = paste0(file_path, file_name), type = "cairo-png", 
         bg = "transparent", ...)
}

# two helper functions to translate Stan's numerical 'j' column
# into ethnic group labels
j_to_e <- function(x){
  # are you sure this is the right column?
  if (!all(x %in% 1:3)){
    stop("All values must be between 1 and 3")
  }
  ifelse(x == 1, "Asian", ifelse(x == 2, "Black", "White"))
}

j_to_e_lab <- function(x){
  paste0("e==", j_to_e(x))
}

# Synthetic data generation ---------------------------------------------------
source("code/generate_synthetic_data.R")

# Data preparation ------------------------------------------------------------
source("code/data_prep.R")

# Running the model -----------------------------------------------------------
source("code/main_model.R")

# Creating figures and table --------------------------------------------------
# Note that all of these depend on model 'm' 
# (from 'main_model.R') being in the environment

# Figure 1: Density of p = softmax(theta)
source("code/fig_1_p_theta.R")

# Figure 2: Coefficient densities 
source("code/fig_2_coef.R")

# Figures 3 and 4: Density and probability mass of D^S and D^P
source("code/fig_3_4_disp.R")

# Figure 5: Bias decomposition into violin plot
source("code/fig_5_bias_decomp.R")

# Table 1: Summary of the variables used
source("code/tbl_1_summary_table.R")

# Appendix A (additional results) ---------------------------------------------

# Table A.1: Median + 90% intervals of coefficients
# (depends on 'code/fig_2_coef.R'!)
source("code/tbl_a1_coef_table.R")

# Figure A.1: p = softmax(theta) by time
# and Figure A.2: D^S and D^P by time
source("code/fig_a1_a2_time.R")

# Figure A.3: Observed search counts by ethnicity 
# against predicted search counts by ethnicity based on softmax(theta)
source("code/fig_a3_fit.R")

# Appendix A.4 presents the results of running an AR(1) model on each
# officer time series of D^S and D^P
# (depends on 'code/fig_3_4_disp.R')
source("code/apx_a4_ar_model.R")
# Figure A.4: AR(1) coefficients from the AR(1) models
source("code/fig_a4_ar_coef.R")

# Figure A.5: Search volume by crime incidents against minority share
source("code/fig_a5_search_volume_crime.R")

