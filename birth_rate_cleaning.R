library(janitor)
library(readxl)
library(fs)
library(scales)
library(sorensonimpact)
library(tidyverse)
library(purrr)
library(tidycensus)

# i am so sorry, i couldnt figure out how to automate ... :( -- Lily
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
natl2000 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2000.csv")
natl2001 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2001.csv")
natl2002 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2002.csv")
natl2003 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2003.csv")
natl2004 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2004.csv")
natl2005 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2005.csv")
natl2006 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2006.csv")
natl2007 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2007.csv") ##
natl2008 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2008.csv")
natl2009 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2009.csv")
natl2010 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2010.csv")
natl2011 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2011.csv")
natl2012 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2012.csv")
natl2013 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2013.csv")
natl2014 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2014.csv")
natl2015 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2015.csv")
natl2016 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2016.csv")
natl2017 <- read_csv("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/future birth rate data/natl2017.csv")

fips <- fips_codes %>% 
  select(state_name, state) %>% 
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

births00 <- natl2000 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2000, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births01 <- natl2001 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2001, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births02 <- natl2002 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2002, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births03 <- natl2003 %>% 
  group_by(ostate) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2003, state_code = ostate) %>% 
  select(-c(ostate))

births04 <- natl2004 %>% 
  group_by(ostate) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2004, state_code = ostate) %>% 
  select(-c(ostate))

births05 <- natl2005 %>% 
  group_by(xostate) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2005, state_code = xostate) %>% 
  select(-c(xostate))

births06 <- natl2006 %>% 
  group_by(ostate) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2006, state_code = ostate) %>% 
  select(-c(ostate))

births07 <- natl2007 %>% 
  group_by(ostate) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2007, state_code = ostate) %>% 
  select(-c(ostate))

births08 <- natl2008 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2008, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births09 <- natl2009 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2009, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births10 <- natl2010 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2010, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births11 <- natl2011 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2011, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births12 <- natl2012 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2012, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births13 <- natl2013 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2013, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births14 <- natl2014 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2014, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births15 <- natl2015 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2015, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births15 <- natl2015 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2015, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births16 <- natl2016 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2016, state_code = stoccfip) %>% 
  select(-c(stoccfip)) %>% 
  left_join(fips)

births17 <- natl2017 %>% 
  group_by(stoccfip) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(datayear = 2017, state_code = stoccfip) %>% 
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
write_rds(births00, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births00.rds")
write_rds(births01, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births01.rds")
write_rds(births02, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births02.rds")
write_rds(births03, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births03.rds")
write_rds(births04, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births04.rds")
write_rds(births05, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births05.rds")
write_rds(births06, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births06.rds")
write_rds(births07, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births07.rds")
write_rds(births08, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births08.rds")
write_rds(births09, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births09.rds")
write_rds(births10, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births10.rds")
write_rds(births11, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births11.rds")
write_rds(births12, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births12.rds")
write_rds(births13, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births13.rds")
write_rds(births14, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births14.rds")
write_rds(births15, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births15.rds")
write_rds(births16, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births16.rds")
write_rds(births17, "/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/BirthData/births17.rds")

# 2005-2017, state data not available on the national dataset, have to go to the PDF description, copy, paste, and scrape... URGH NBER AND CDC URGH

births_2005 = "Alabama 59,300 60,453
Alaska 10,365 10,459
Arizona 95,687 96,199
Arkansas 38,381 39,208
California 549,100 548,882
Colorado 69,205 68,944
Connecticut 42,133 41,718
Delaware 12,265 11,643
District of Columbia 14,311 7,971
Florida 226,415 226,240
Georgia 143,476 142,200
Hawaii 17,911 17,924
Idaho 22,522 23,062
Illinois 175,714 179,020
Indiana 87,843 87,193
Iowa 39,337 39,311
Kansas 40,737 39,888
Kentucky 54,590 56,444
Louisiana 60,461 60,937
Maine 13,975 14,112
Maryland 71,292 74,980
Massachusetts 77,820 76,865
Michigan 126,498 127,706
Minnesota 70,933 70,919
Mississippi 41,175 42,395
Missouri 79,523 78,618
Montana 11,551 11,583
Nebraska 26,350 26,145
Nevada 36,950 37,268
New Hampshire 13,968 14,420
New Jersey 110,800 113,776
New Mexico 28,291 28,835
New York 247,901 246,351
North Carolina 123,943 123,096
North Dakota 9,621 8,390
Ohio 148,876 148,388
Oklahoma 50,656 51,801
Oregon 46,712 45,922
Pennsylvania 144,908 145,383
Rhode Island 13,481 12,697
South Carolina 55,321 57,711
South Dakota 11,957 11,462
Tennessee 87,072 81,747
Texas 387,856 385,915
Utah 52,555 51,556
Vermont 5,932 6,295
Virginia 102,646 104,555
Washington 82,336 82,703
West Virginia 21,150 20,836
Wisconsin 69,769 70,984
Wyoming 6,778 7,239"

births05 <- str_split(births_2005, "\\n", simplify = TRUE) %>%
  as_tibble() %>%
  gather(key = "key", value = "value") %>%
  select(-c(key)) %>%
  extract(value, into = c("state_name", "n", "residence"), regex = "([A-z\\s]*)\\s([\\d\\,]+)\\s([\\d\\,]+)") %>% 
  mutate(n = as.integer(gsub(",","",n))) %>% 
  left_join(fips) %>% 
  select(-c(residence, state_name)) %>% 
  rename("state_code" = "state") %>% 
  mutate(datayear = 2005)

