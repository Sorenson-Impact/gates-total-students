# Created by Gwen Reynolds. trying to get totalstudents for as far back as i can :D

library(siverse)
library(tidyverse)
if(!require(ipeds)) devtools::install_github("jbryer/ipeds")


efa2008 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2008a.csv")
efa2009 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2009a.csv")
efa2010 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2010a.csv")
efa2011 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2011a.csv")
efa2012 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2012a.csv")
efa2013 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2013a.csv")
efa2014 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2014a.csv")
efa2015 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2015a.csv")
efa2016 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2016a.csv")
efa2017 <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2017a.csv")

# # this is code Gwen wrote to get the right enrollment
# efa2016 <- efa2016 %>% 
#   filter(LINE==1 | LINE==15 | LINE==29) %>% 
#   mutate(LINE = case_when(LINE==1 ~ "FTFT", 
#                           LINE==15 ~ "FTPT",
#                           LINE==29 ~ "totalstudents")) %>% 
#   rename(grand_total_students = EFTOTLT) %>% 
#   select(UNITID, LINE, grand_total_students) %>% 
#   spread(key= LINE, value = grand_total_students) %>%
#   mutate(FTFT = replace_na(FTFT, 0),
#          FTPT = replace_na(FTPT, 0)) %>% 
#   mutate(total_FT_PT_FT_student = FTFT + FTPT) 

cleanefa <- function(dataset){
  dataset %>% 
    filter(LINE==1 | LINE==15 | LINE==29) %>% 
    mutate(LINE = case_when(LINE==1 ~ "FTFT", 
                            LINE==15 ~ "FTPT",
                            LINE==29 ~ "totalstudents")) %>% 
    rename(grand_total_students = EFTOTLT) %>% 
    select(UNITID, LINE, grand_total_students) %>% 
    spread(key= LINE, value = grand_total_students) %>%
    mutate(FTFT = replace_na(FTFT, 0),
           FTPT = replace_na(FTPT, 0)) %>% 
    mutate(total_FT_PT_FT_student = FTFT + FTPT) 
}


efa2008 <- efa2008 %>%  cleanefa() %>% mutate(Year = 2008)
efa2009 <- efa2009 %>%  cleanefa() %>% mutate(Year = 2009)
efa2010 <- efa2010 %>%  cleanefa() %>% mutate(Year = 2010)
efa2011 <- efa2011 %>%  cleanefa() %>% mutate(Year = 2011)
efa2012 <- efa2012 %>%  cleanefa() %>% mutate(Year = 2012)
efa2013 <- efa2013 %>%  cleanefa() %>% mutate(Year = 2013)
efa2014 <- efa2014 %>%  cleanefa() %>% mutate(Year = 2014)
efa2015 <- efa2015 %>%  cleanefa() %>% mutate(Year = 2015)
efa2016 <- efa2016 %>%  cleanefa() %>% mutate(Year = 2016)
efa2017 <- efa2017 %>%  cleanefa() %>% mutate(Year = 2017)

efa08to17 <- efa2008 %>% 
  bind_rows(efa2009) %>% 
  bind_rows(efa2010) %>% 
  bind_rows(efa2011) %>% 
  bind_rows(efa2012) %>% 
  bind_rows(efa2013) %>% 
  bind_rows(efa2014) %>% 
  bind_rows(efa2015) %>% 
  bind_rows(efa2016) %>% 
  bind_rows(efa2017) 
  
  

institutions <- read_csv("~/Google Drive/SI/DataScience/data/gates/IPEDS/Full Gates Download/IPEDS Data/Institutions.csv") %>% 
  filter(`Degree Granting` == "Degree-granting") %>% 
  mutate(Level = case_when(Level == "At least 2 but less than 4 years" ~ "2-Yr",
                           Level == "Four or more years" ~ "4-Yr")) %>% 
  filter(!is.na(Level)) %>% 
  mutate(UNITID = `Institution ID`) %>% 
  filter(Sector!= "Administrative Unit")

states <- institutions %>% 
  select(UNITID, Year, State, `State Name`, `County Name`, `County Code`, `Institution Name`) 

efastate <- efa08to17 %>% 
  left_join(states)

