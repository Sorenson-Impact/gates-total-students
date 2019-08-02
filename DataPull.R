library(tidycensus)
library(siverse)
library(tidyverse)
library(tidyselect)
library(fuzzyjoin)
library(purrr)
library(furrr)

# institutions file from gates foundation
# random forrest
# highlight by variability
# try a linear model
# slope from the years in the past
# color continuously by average total students data per institute
# correlate birth rate by state, see if it is correlated with student populations
# use grawe ELS data ?
# compare model to:
# - linear model (log data)
# - time series with random walks

# census_api_key("e3a7c1da5af60c9c770f4e9ab62807e6cef7683a", install = TRUE)
readRenviron("~/.Renviron")

# vthing <- map_df(us, function(x) {
#   get_acs(geography = "tract", variables = c("B15001_006", "B15001_007", "B15001_008", 
#                                              "B15001_009", "B15001_010"), 
#           state = x)
# })

us <- unique(fips_codes$state)[1:51]

acsvars <- load_variables(year = 2012, "acs5", cache = T) %>%
  mutate(level = str_count(label, pattern = "!!")) %>%
  rowwise() %>%
  mutate(levlab = str_split(label, pattern = "!!") %>% unlist() %>% .[level + 1]) %>%
  ungroup() %>%
  mutate(concept = str_to_title(concept)) %>%
  rename(variable = name)

si_acs <- function(table, county = NULL, state = NULL, summary_var = "universe total", geography = NULL, survey = "acs5", geometry = FALSE, year = 2017) {
  # cat(yellow(bold("Reminder: You must stay within the same level for any summary to be valid!\n")))
  
  if(summary_var == "universe total") summary_var = paste0(table, "_001")
  summary_label = acsvars %>% filter(variable == summary_var) %>% pull(levlab)
  
  get_acs(geography = geography,
          table = table,
          county = county,
          state = state,
          output = "tidy",
          year = year,
          cache_table = T,
          summary_var = summary_var, 
          geometry = geometry, 
          cb = T, 
          survey = survey) %>%
    clean_names() %>%
    left_join(acsvars) %>%
    select(-summary_moe, -variable) %>%
    select(geoid, county = name, level, levlab, estimate, everything()) %>%
    rename(!!summary_label := summary_est)
}

plan(multisession)

employment_test_2009 <- future_map_dfr(us, function(x){
  si_acs("B23001", year = 2009, geography = "state", state = x)
}, .progress = T)

employment_test_2010 <- future_map_dfr(us, function(x){
  si_acs("B23001", year = 2010, geography = "state", state = x)
}, .progress = T)


employment_test_2012 <- future_map_dfr(us, function(x){
  si_acs("B23001", year = 2012, geography = "state", state = x)
}, .progress = T)

employment_spread <- employment_test_2010 %>% 
  select(c(county, estimate, label)) %>% 
  spread(key = label, value = estimate) %>% 
  mutate(employed = reduce(select(., ends_with("Employed")), `+`),
         unemployed = reduce(select(., ends_with("Unemployed")), `+`),
         total = employed + unemployed,
         unemployment_rate = unemployed/total)

write_rds(employment_spread, "G:/My Drive/SI/DataScience/data/gates/census data/employment_data2010.rds")


#Birth Data Cleaning-----
fips <- fips_codes %>% 
  select(state, state_code) %>% 
  unique()

?fips_codes
#1991
births91 <- read_csv("G:/My Drive/SI/DataScience/data/gates/BirthData/natl1991.csv")

births91.clean <- births91 %>% 
  select(datayear, state_code = stoccfip) %>% 
  group_by(state_code) %>% 
  count() %>% mutate(datayear = 1991) %>% 
  left_join(fips)

write_rds(births91.clean, "G:/My Drive/SI/DataScience/data/gates/BirthData/births91.rds")

#1992
births92 <- read_csv("G:/My Drive/SI/DataScience/data/gates/BirthData/natl1992.csv")

births92.clean <- births92 %>% 
  select(datayear, state_code = stoccfip) %>% 
  group_by(state_code) %>% 
  count() %>% mutate(datayear = 1992) %>% 
  left_join(fips)

write_rds(births92.clean, "G:/My Drive/SI/DataScience/data/gates/BirthData/births92.rds")

#1993
births93 <- read_csv("G:/My Drive/SI/DataScience/data/gates/BirthData/natl1993 2.csv")

births93.clean <- births91 %>% 
  select(datayear, state_code = stoccfip) %>% 
  group_by(state_code) %>% 
  count() %>% mutate(datayear = 1993) %>% 
  left_join(fips)

write_rds(births93.clean, "G:/My Drive/SI/DataScience/data/gates/BirthData/births93.rds")

#Load in all data
births88 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births88.rds") 
births89 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births89.rds") 
births90 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births90.rds") 
births91 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births91.rds")
births92 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births92.rds") 
births93 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births93.rds") 
births94 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births94.rds")
births95 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births95.rds") 
births96 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births96.rds") 
births97 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births97.rds") 
births98 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births98.rds") 
births99 <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/births99.rds") 

birthdata <- births88 %>% 
  bind_rows(births89) %>% 
  bind_rows(births90) %>% 
  bind_rows(births91) %>% 
  bind_rows(births92) %>% 
  bind_rows(births93) %>% 
  bind_rows(births94) %>% 
  bind_rows(births95) %>% 
  bind_rows(births96) %>% 
  bind_rows(births97)  

write_rds(birthdata, "G:/My Drive/SI/DataScience/data/gates/BirthData/birthdata.rds")

  

  


