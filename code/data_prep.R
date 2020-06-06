# This file prepares the data for model estimation.
# Here, we drop officers with too few observation points 
# and prepare the data

# Author: Lara Vomfell
# Date: 31/05/2020

# Officer filtering: As described in the Data& Methods section,
# we drop all officers who search in fewer than 5 half-years

# -----------------------------------------------------------------------------

# load .csv with semester-level search/suspect/patrol counts
# + all officer-level information
semester <- fread("data/wide_semester_counts.csv")
setorder(semester, collar_id, sem_year, sem_team)

# Officer filtering -----------------------------------------------------------

# first, calculate number of observation points per officer
semester[, n_year := uniqueN(sem_year), by = collar_id]
# then calculate how many of those semesters had any searches
semester[, sum_searches := stops_asian + stops_black + stops_white, 
         by = .(collar_id, sem_year)]
semester[, n_any_search := sum(sum_searches > 0), by = collar_id]

keep <- semester[n_year > 4 & n_any_search/n_year > .5, unique(collar_id)]
semester <- semester[collar_id %in% keep]

# Variable formatting ---------------------------------------------------------

# set consistent collar_id from 1:N
semester[, officer_id := .GRP, by = collar_id]
# create id column for the teams
semester[, team_id := .GRP, by = sem_team]

# scale age + experience
std_cols <- c("officer_age", "officer_service")
semester[, (std_cols) := lapply(.SD, scale), .SDcols = std_cols]

# order binary gender
semester[, officer_gender := factor(officer_gender, 
                                    levels = c("Male", "Female"))]

# create officer ethnicity indicator
semester[, officer_asian := as.numeric(officer_ethnic_group == "Asian")]
semester[, officer_black := as.numeric(officer_ethnic_group == "Black")]

# calculate white share in team
team_share <- unique(semester[, .(collar_id, officer_ethnic_group), 
                              by = team_id])

team_share <- team_share[, .(white_share = sum(officer_ethnic_group == "White")/.N),
                         by = team_id]

semester <- merge(semester, team_share, by = "team_id")

# ID lookup -------------------------------------------------------------------

# Now our data is in panel format (N x T) and we have uneven t_i,
# it's nice to have a lookup table telling us to what each row 'i' corresponds

# create table with i = 1:nrow index
id <- semester[, .(i = 1:.N, officer_id, team_id, sem_year)]
id[, sem_num := .GRP, by = sem_year]
setkey(id, i)

