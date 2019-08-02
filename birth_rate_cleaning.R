# birth rate data -- from Jon Pritzker community assessment tool

library(janitor)
library(readxl)
library(fs)
library(scales)
library(sorensonimpact)
library(tidyverse)
library(purrr)
library(tidycensus)

si_ggplot_theme_update()
si_knitr_settings()
gdrive_dir <- "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData"

load("~/Google Drive/SI/DataScience/Pritzker/ACS Data/geoids lookup table.RData")

# Get wondr data
# Settings: select years individually
# use bridged race because it has fewer categories / fewer suppressions

files <- dir_ls(path(gdrive_dir),glob = "*.txt")
wonder <- files %>% 
  map(function(x) {
    suppressWarnings(read_tsv(x)) %>% #na = c("", "NA", "Suppressed"), col_types = cols(Births = "i")
      clean_names() %>% 
      filter(!is.na(county)) %>% 
      select(-notes) %>% 
      separate(col = county, into = c("county", "state"), sep = ", ") %>% 
      
      rename(geoid = county_code) %>% 
      
      select(-contains("_code")) %>% 
      add_column(source = str_sub(basename(x), end = -5)) %>% 
      separate(source, into = c("data", "subgroup"), sep = " X ")
  } 
  ) %>% set_names(str_sub(basename(files), end = -5))

#Number of births without race/ethnicity
all_births <- wonder %>% 
  reduce(bind_rows) %>% 
  filter(year <= 1997)

#   select(-subgroup) %>% 
#   left_join(filter(., data == "all births") %>%
#               select(geoid, year, all_births = births)) %>%
#   mutate() %>% 
#   full_join(geoids$county %>% select(-state_name, -name_county), by = c("geoid" = "GEOID", "county", "state")) %>% 
#   complete(year, nesting(county, state, geoid), data) %>% 
#   filter(!is.na(year), !is.na(data)) %>% 
#   mutate_all(funs(replace_na(., "Data Unavailable")))

natl1988 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1988.csv")
natl1989 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1989.csv")
natl1990 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1990.csv")
natl1997 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1997.csv")
natl1996 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1996.csv")
natl1995 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1995.csv")
natl1994 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1994.csv")
natl1993 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1993.csv")
natl1992 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/natl1992.csv")

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

write_rds(births88, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births88.rds")
write_rds(births89, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births89.rds")
write_rds(births90, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births90.rds")
write_rds(births97, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births97.rds")
write_rds(births96, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births96.rds")
write_rds(births95, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births95.rds")
write_rds(births94, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births94.rds")
write_rds(births93, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births93.rds")
write_rds(births92, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births92.rds")

