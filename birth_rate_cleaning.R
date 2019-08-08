# birth rate data -- from Jon Pritzker community assessment tool

library(janitor)
library(readxl)
library(fs)
library(scales)
library(sorensonimpact)
library(tidyverse)
library(purrr)
library(tidycensus)

natl1988 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1988.csv")
natl1989 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1989.csv")
natl1990 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1990.csv")
natl1997 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1997.csv")
natl1996 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1996.csv")
natl1995 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1995.csv")
natl1994 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1994.csv")
natl1993 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1993.csv")
natl1992 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1992.csv")
natl1998 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1998.csv")
natl1999 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1999.csv")

fips <- fips_codes %>% 
  select(state, state_code) %>% 
  unique()

births88 <- natl1988 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1988, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births89 <- natl1989 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1989, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births90 <- natl1990 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1990, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births97 <- natl1997 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1997, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births96 <- natl1996 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1996, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births95 <- natl1995 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1995, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births94 <- natl1994 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1994, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births93 <- natl1993 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1993, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births92 <- natl1992 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1992, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births98 <- natl1998 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1998, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births99 <- natl1999 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 1999, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

write_rds(births88, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births88.rds")
write_rds(births89, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births89.rds")
write_rds(births90, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births90.rds")
write_rds(births97, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births97.rds")
write_rds(births96, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births96.rds")
write_rds(births95, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births95.rds")
write_rds(births94, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births94.rds")
write_rds(births93, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births93.rds")
write_rds(births92, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births92.rds")
write_rds(births98, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births98.rds")
write_rds(births99, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births99.rds")


