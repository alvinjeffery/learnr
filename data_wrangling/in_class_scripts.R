
# load the R file to create the fake data (try using 'tab' to fill in path)
source('/Users/AlvinMBA/Desktop/VAQS/Learning R/data_wrangling/fake_data.R')

library(dplyr); library(tidyr); library(Hmisc); library(lubridate)

#### Session 1 ####

# rename variable to keep consistent between data frames
labs <- rename(labs, id = patient_id)

# add labs onto demographics
df <- left_join(demographics, labs, by = 'id')

length(unique(demographics$id)) == length(unique(df$id))
length(unique(df$id)) == length(unique(labs$id))

# from data.table
#uniqueN(df$id) 

str(df$lab_ts)
hour(df$lab_ts) # using lubridate package

# create new variable to add hour of timestamp
df$ts_hour <- hour(df$lab_ts)

# create chain to both rename id variable & add vital signs onto new dataframe
all <- vitals %>%
  rename(id = patient_id_vs) %>% 
  mutate(ts_hour = hour(vitals_ts)) %>% 
  right_join(df, by = c('id', 'ts_hour'))
  
#### Session 2 (with new working code for function as of 4/4/17) ####

# check out patient id #6 - what things are wrong with this merge?  
# fix the inconsistency in the new hour variable by also referring to the minute variable
# fix the date/year inconsistency by adding more arguments
df <- rename(df, ts = lab_ts)

parse_ts <- function(data) { 
  x <- data %>%
    mutate(ts_year = year(data[,'ts'])) %>%
    mutate(ts_month = month(data[,'ts'])) %>%
    mutate(ts_day = day(data[,'ts'])) %>%
    mutate(ts_hour = hour(data[,'ts'])) %>%
    mutate(ts_min = minute(data[,'ts'])) 
}

df <- parse_ts(df)

all <- vitals %>%
  rename(id = patient_id_vs) %>%
  rename(ts = vitals_ts) %>%
  parse_ts() %>% # new application of function as of 4/4/17
  # mutate(ts_year = year(vitals_ts)) %>%
  # mutate(ts_month = month(vitals_ts)) %>%
  # mutate(ts_day = day(vitals_ts)) %>%
  # mutate(ts_hour = hour(vitals_ts)) %>%
  # mutate(ts_min = minute(vitals_ts)) %>% 
  full_join(df, by = c('id', 'ts_year', 'ts_month', 'ts_day')) %>%
  select(id, age, gender, ts.x, pulse, sbp, wbc, plt) %>%
  select(-contains('ts_'))
# consider time/system burden of creating new variables vs. calling functions within chain

#### Session 3 ####

### explore the new vitals_long data set - what stands out as potential problems?  
source('/Users/AlvinMBA/Desktop/VAQS/Learning R/data_wrangling/fake_data.R')


### start building a chain where the final df will be names 'vitals_wide'
vitals_wide <- vitals_long %>%

### rename the id variable so it's simply called 'id'
  rename(id = patient_id_vs) %>% 

### change "pulse" and "hr" values to simply "pulse" in the 'type' variable'
  mutate(type = ifelse(type == 'hr', # condition to test
                       'pulse', # if TRUE, do this
                       type)) %>% # otherwise, do this

### convert the 'timestamp' and 'type' variables to an appropriate data type
  mutate(vitals_ts = ymd_hms(as.character(vitals_ts))) %>% 
  mutate(type = as.factor(type)) %>%

### use a tidyr:: function to convert the dataframe
  spread(key = type, 
         value = value) %>% 

### arrange in DESCENDING order of ids but ASCENDING order of times
  arrange(desc(id), vitals_ts)


### optional: convert vitals_wide back to vitals_long

