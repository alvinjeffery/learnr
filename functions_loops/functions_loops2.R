library(Hmisc); library(dplyr)

set.seed(123)
n <- 1e3 # set sample size
w <- data.frame(id = seq(1:n), 
                age = rnorm(n = n, mean = 65, sd = 10), 
                gender = factor(c(rep('male', 0.5*n), 
                                  rep('female', 0.5*n))))

w$hgb <- ifelse(w$gender == 'male', 
                rnorm(n = n, mean = 13, sd = 2), 
                rnorm(n = n, mean = 11, sd = 1.5))

# calculate overall mean & by gender
mean(w$hgb)

# calculate each gender's mean hgb without specifying male or female
for (g in unique(w$gender)) {
  x <- filter(w, gender == g)
  print(mean(x[,'hgb']))
}

# create a function to do the same but requiring you to specify both male & female

calc_mean_hgb <- function(data = w, sex) { 
  x <- filter(w, gender == sex)
  print(mean(x[,'hgb']))
}

calc_mean_hgb(sex = 'male')
calc_mean_hgb(sex = 'female')

# additional variables to demonstrate benefit of functions
w$pain <- sample(Cs(none, mild, moderate, severe), 
                 size = nrow(w), 
                 replace = TRUE, 
                 prob = c(0.6, 0.2, 0.1, 0.1))
table(w$pain)

calc_mean_hgb <- function(data = w, iv) {
  for (i in unique(data[,iv])) {
    x <- filter(data, data[,iv] == i)
    print(paste("Mean Hemoglobin for", i, "is"))
    print(mean(x[, 'hgb']))
  }
}

calc_mean_hgb(iv = 'pain')

# add within a gender loop

for (j in unique(w$gender)) {
  for (i in unique(w$pain)) {
    x <- filter(w, pain == i & gender == j)
    print(paste("Mean Hemoglobin for", j, "with", i, "pain is"))
    print(mean(x[, 'hgb']))
  }
}


