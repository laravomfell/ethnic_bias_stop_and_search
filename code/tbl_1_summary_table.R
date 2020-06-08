# In this file we create Table 1 in the main text,
# summarizing the variable in our data

# Author: Lara Vomfell
# Date: 31/05/2020

# This file relies on skimr as a quick summary tool of our data.table.
# Since we also want to report pre-standardized officer age and experience,
# we load the data from scratch to make this explicit

# -----------------------------------------------------------------------------

library(skimr)

# load the data
sem_data <- fread("data/wide_semester_counts.csv")
setorder(sem_data, collar_id, sem_year, sem_team)

# drop the same officers as before
sem_data <- sem_data[collar_id %in% keep]

# set consistent collar_id
sem_data[, officer_id := .GRP, by = collar_id]
# create id column for the teams
sem_data[, team_id := .GRP, by = sem_team]

# scale age + experience
# but keep a copy of the originals
std_cols <- c("officer_age", "officer_service")
sem_data[, paste0(std_cols, "_std") := lapply(.SD, scale), .SDcols = std_cols]

# recode gender
sem_data[, officer_gender := as.numeric(officer_gender == "Female")]

# create officer ethnicity indicator
sem_data[, officer_asian := as.numeric(officer_ethnic_group == "Asian")]
sem_data[, officer_black := as.numeric(officer_ethnic_group == "Black")]

# create a custom skimr basically just reporting mean, sd, min and max
custom_skim <- skim_with(numeric = sfl(p25 = NULL,
                                       p50 = NULL,
                                       p75 = NULL,
                                       hist = NULL),
                         base = NULL)

# first, summarize the data that varies with time
time_cols <- c("stops_asian", "stops_black", "stops_white",
               "incidents_asian", "incidents_black", "incidents_white",
               "asian", "black", "white",
               "officer_age", "officer_age_std",
               "officer_service", "officer_service_std")

tbl_time <- custom_skim(sem_data, all_of(time_cols)) %>%	
  as.data.table()	
	
	
	
	
# now the fixed data (gender etc)	
	
# drop everything that doesn't vary by time	
fixed_data <- sem_data[, .(time = uniqueN(sem_year)), 	
					   by = .(officer_id, officer_gender, 	
							  officer_asian, officer_black)]	
	
fixed_cols <- c("officer_gender", "officer_asian", 	
				"officer_black",	
				"time")	
	
tbl_fixed <- custom_skim(fixed_data, all_of(fixed_cols)) %>%	
  as.data.table()	

# we also want the team white share	
tbl_team <- custom_skim(team_share, "white_share") %>%	
  as.data.table()	
	
# combine all three results	
tbl <- rbind(tbl_time, tbl_fixed, tbl_team)	
# do some light rounding	
tbl <- tbl[, -"skim_type"][, lapply(.SD, round, 2), by = skim_variable]

# rename the variables
tbl[, skim_variable := dplyr::recode(skim_variable,
  "stops_asian" = "Asian",
  "stops_black" = "Black",
  "stops_white" = "White",
  "incidents_asian" = "Asian",
  "incidents_black" = "Black",
  "incidents_white" = "White",
  "asian" = "Asian",
  "black" = "Black",
  "white" = "White",
  "officer_age" = "Officer age in years",
  "officer_age_std" = "Standardized officer age",
  "officer_service" = "Officer experience",
  "officer_service_std" = "Standardized officer experience",
  "officer_gender" = "Female officer",
  "officer_asian" = "Asian officer",
  "officer_black" = "Black officer",
  "time" = "Number of observed half-years per officer",
  "white_share" = "Share of White officers in team"
)]


# rename columns
setnames(tbl, c("Variable",
                "Mean", "SD", "Min", "Max"))

# export to file
print(xtable::xtable(tbl,
                     digits = c(0, 0, 2, 2, 2, 2),
                     caption = "Variable summaries in the data file used for model estimation."),
      include.rownames = FALSE,
      file = "results/ugly_summary_table.tex")
