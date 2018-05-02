# Developed by Alvin D. Jeffery, PhD, RN in 2017 to 
# facilitate string manipulation learning in R for Clinicians

library(dplyr); library(tidyr); library(stringr)

my_data <- c("Aspirin 81 mg daily", 
             "aspirin 81mg daily", 
             "Asprin 81mg daily",
             "asa 365mg daily",
             "Therapy asan outpatient")

str_detect(my_data, "Aspirin")
str_detect(my_data, "[Aa]spirin")
str_detect(my_data, "[Aa]spi*rin")
str_detect(my_data, "[Aa]spi*rin | *asa")
str_detect(my_data, "[Aa]spi*rin | *asa[ ]") # winner, winner, chicken dinner
str_detect(my_data, "[AaSs]{3}")

str_detect(my_data, "81|365")
str_detect(my_data, "[0-9]{2,3}")
str_detect(my_data, "[1-9][0-9]{1,2}")

str_detect(my_data, "mg")

str_detect(my_data, "([Aa]spi*rin | *asa[ ])([0-9]{2,3})( *mg)")

new <- str_match(my_data, "([Aa]spi*rin | *asa[ ])([0-9]{2,3})( *mg)")
#new <- str_trim(new)

new <- as.data.frame(new)
new <- as.data.frame(apply(new, 2, function(x) str_trim(x)))

#apply(new, 2, function(x) ifelse(is.na(x), median(x, na.rm=T), x))

str(new)




df <- data.frame(id = seq(1, 5), 
                 meds = my_data)

df <- df %>%
  mutate(asa = ifelse(str_detect(my_data, "[Aa]spi*rin | *asa[ ]"), 
                      1, 
                      0)) %>%
  filter(asa == 1)




####









# start basic
str_detect(my_data, "Aspirin")

# add regex to allow different capitalization
str_detect(my_data, "[Aa]spirin")

# add some regex to account for missing letters
str_detect(my_data, "[Aa]spi*rin")

# add in the other category
str_detect(my_data, "[Aa]spi*rin |asa") 
# note the 'or' inside the quotes & no space; could also try...
str_detect(my_data, "[Aa]spi*rin | *asa")

# get ensure a space after "asa"
str_detect(my_data, "asa[ ]")
str_detect(my_data, "[Aa][Ss][Aa][ ]")


# capture dose
str_detect(my_data, "81|365")

# capture all 2 or 3-digit doses
str_detect(my_data, "[0-9]{1,3}")


# capture units
str_detect(my_data, "mg")


# put it all together
str_detect(my_data, "([Aa]spi*rin | *asa)([0-9]{1,3})( *mg)")

new <- str_match(my_data, "([Aa]spi*rin | *asa)([0-9]{1,3})( *mg)")

# review
new
str(new)

# note the weird spaces
str_trim(new)

# use apply() to move quickly through the columns
new <- apply(new, 2, function(x) str_trim(x))

# coerce to data frame
new <- as.data.frame(new)



