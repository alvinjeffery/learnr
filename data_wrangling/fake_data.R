
# this file can be used to source() for automatically creating data for R sessions
# last updated: 3/13/17

set.seed(123)
n <- 20
demographics <- data.frame(id = seq(1:n), 
                           age = rnorm(n = n, mean = 60, sd = 15), 
                           gender = sample(c('male', 'female'), size = n, 
                                           replace = TRUE, prob = rep(0.5, 2))
                           )

# create random set of times over a couple years
random_time <- seq(as.POSIXct('2010/01/01 00:00:00'), 
                   as.POSIXct('2012/01/01 00:00:00'), 
                   by = 'min')

n <- 3*n # more than the number of patients above
labs <- data.frame(lab_id = seq(1:n), 
                   patient_id = sample(c(1357, 8342, demographics$id), # includes extra patients
                                       size = n, replace = TRUE), 
                   lab_ts = sample(random_time, size = n),
                   wbc = round(rnorm(n = n, mean = 6, sd = 2), 1), 
                   plt = as.integer(rnorm(n = n, mean = 250, sd = 75))
                   )

# remove a few patients' labs
labs <- labs[-c(sample(unique(labs$patient_id), size = 2)), ]

# update intended sample size
n <- nrow(labs)
vitals <- data.frame(vitals_id = seq(1:n), 
                     patient_id_vs = labs$patient_id, 
                     # make vitals close to labs but not exact
                     vitals_ts = as.POSIXct(jitter(as.numeric(labs$lab_ts)), origin = '1970-01-01'), 
                     pulse = round(rnorm(n = n, mean = 80, sd = 10), 0), 
                     sbp = round(rnorm(n = n, mean = 120, sd = 15), 0)
                     )

# make vital signs in long format to practice shaping
vitals_long <- vitals %>%
  select(-vitals_id) %>% 
  gather(key = type, value = value, # name the new columns
         pulse, sbp) %>% # specify which old columns are moved to the new ones
  # change some instances of 'pulse' to 'hr' to practice if-else statements
  mutate(type = ifelse(type == 'pulse', 
                       sample(c('pulse', 'hr'), size = 2*nrow(vitals), replace = T),
                       type)) %>% 
  # change timestamp to factor to assist with data manipulation practice
  mutate(vitals_ts = as.factor(vitals_ts)) %>%
  arrange(patient_id_vs)


# clean-up after sourcing
rm(n, random_time)