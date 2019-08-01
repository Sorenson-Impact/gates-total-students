library(tidycensus)
library(siverse)
library(tidyverse)
library(tidyselect)
library(fuzzyjoin)
library(purrr)
library(furrr)

census_api_key("e3a7c1da5af60c9c770f4e9ab62807e6cef7683a", install = TRUE)
readRenviron("~/.Renviron")

vthing <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = c("B15001_006", "B15001_007", "B15001_008", 
                                             "B15001_009", "B15001_010"), 
          state = x)
})


#Time Series Play



vars <- load_variables(2009, "acs5", cache = TRUE)

us <- unique(fips_codes$state)[1:51]

thing <- map_df(us, function(x) {
  get_acs(geography = "state", variables = c("B23001_062"), 
          state = x)
})

test <- c()
for (i in 0:9){
  for (j in 0:2){
    if (i == 0){
      test <- c(test, paste("00",as.character(6+7*i+j), sep= ""))
    } else {
      test <- c(test, paste("0",as.character(6+7*i+j), sep= ""))
    }
  }
}
for (i in 1:3){
  for (j in 0:2){
    test <- c(test, paste("0",as.character(69+5*i+j), sep= ""))
  }
}
test

test2 <- c()
for (i in 0:9){
  for (j in 0:2){
    temp <- as.character(92+7*i+j)
    if (str_length(temp)<3){
      test2 <- c(test2, paste("0", temp, sep= ""))
    } else {
      test2 <- c(test2, temp)
    }
  }
}
for (i in 1:3){
  for (j in 0:2){
    test2 <- c(test2, as.character(155+5*i+j))
  }
}
test2

relevant_cols <- c()
for (i in 1:length(test)){
  relevant_cols <- c(relevant_cols, paste("B23001_", test[i], sep = ""))
}
relevant_cols

relevant_cols2 <- c()
for (i in 1:length(test2)){
  relevant_cols2 <- c(relevant_cols2, paste("B23001_", test2[i], sep = ""))
}
relevant_cols2

male_employ <- map_df(us, function(x) {
  get_acs(geography = "state", variables = relevant_cols, 
          state = x)
})

female_employ <- map_df(us, function(x) {
  get_acs(geography = "state", variables = relevant_cols2, 
          state = x)
})

write_rds(male_employ, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/census data/male_employment.rds")
write_rds(female_employ, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/census data/female_employment.rds")
