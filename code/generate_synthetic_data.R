library(data.table)
set.seed(1)

# This is the file generating the synthetic data for 
# the code submission of our paper.
# We cannot provide the original data so we generate synthetic data for 
# n_officers (100 by default) officers.

# Generally, all sample means will match the original data. 
# Since the original data cannot be provided, all
# sample means are hard-coded

# The structure of the provided data is a row for each 
# officer x semester
# where the columns give search counts, suspect and patrol counts
# as well as the officer information

# generation arguments --------------------------------------------------------

# number of unique officers
n_officers <- 100

# generate team strings
teams <- paste0("team", 1:10)

ethnic <- purrr::set_names(c("Asian", "Black", "White"))

# generate officer data -------------------------------------------------------

# for each officer, sample "intial conditions" 
# of age, gender, experience, ethnicity and team
# Sample means match original data

data <- data.table(collar_id = 1:n_officers,
                   sem_team = sample(teams, 
                                     size = n_officers, 
                                     replace = TRUE),
                   officer_gender = sample(c("Female", "Male"), 
                                           size = n_officers, 
                                           replace = T, 
                                           prob = c(0.12, 0.88)),
                   officer_ethnic_group = sample(c(ethnic, "Other"), 
                                                 size = n_officers, 
                                                 prob = c(.06, .01, .89, .03),
                                                 replace = T),
                   officer_age = rnorm(n = n_officers, 
                                       mean = 37, 
                                       sd = 7),
                   officer_service = rnorm(n = n_officers, 
                                           mean = 10.8, 
                                           sd = 5.2))

# transform into panel --------------------------------------------------------

# define the semesters
semesters <- CJ(2014:2018, 1:2)[-.N, paste(V1, V2, sep = ".")]

data <- data[rep(1:nrow(data), each = length(semesters))]
# create row identifier
data[, i := 1:.N]
# semester id + numerical semester id
data[, sem_year := rep(semesters, times = n_officers)]
data[, sem_num := rep(1:9, times = n_officers)]
# age the officers and make them more experienced
# add six months (0.5) every six months
data[, officer_age := officer_age + 0.5 * (sem_num-1)]
data[, officer_service := officer_service + 0.5 * (sem_num - 1)]

# add search counts -----------------------------------------------------------

data[, paste0("stops_", tolower(ethnic)) := as.list(
  rpois(3, c(2.41, 1.72, 5.39))
  ), by = i]

# add suspect counts ----------------------------------------------------------

data[, paste0("incidents_", tolower(ethnic)) := as.list(
  rpois(3, c(10.61, 8.29, 43.41))
  ), by = i]

# add patrol counts -----------------------------------------------------------

data[, tolower(ethnic) := as.list(
  rpois(3, c(85.56, 32.30, 200.84))
  ), by = i]

# save result -----------------------------------------------------------------

fwrite(data, file = "data/wide_semester_counts.csv")
