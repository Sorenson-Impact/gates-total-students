# Created by Gwen Reynolds. trying to get totalstudents for as far back as i can :D

library(siverse)
library(tidyverse)
if(!require(ipeds)) devtools::install_github("jbryer/ipeds")
install.packages("fUnitRoots")
install.packages("forecast")
install.packages("FitAR")
install.packages("lmtest")


efa2008 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2008a.csv")
efa2009 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2009a.csv")
efa2010 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2010a.csv")
efa2011 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2011a.csv")
efa2012 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2012a.csv")
efa2013 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2013a.csv")
efa2014 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2014a.csv")
efa2015 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2015a.csv")
efa2016 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2016a.csv")
efa2017 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/API Pulls/data/ef2017a.csv")

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
  
  

institutions <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/Full Gates Download/IPEDS Data/Institutions.csv") %>% 
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

t <- efastate %>%
  group_by(State) %>%
  count(is.na(`County Code`))


efastate_stagg <- efastate %>%
  select(Year, `County Code`, totalstudents) %>% 
  group_by(Year, `County Code`) %>%
  mutate(totalstudents_stsum = sum(totalstudents)) %>%
  select(-c(totalstudents)) %>%
  unique()


#ARIMA Model Test ----

efaMa <- efastate %>%
  filter(State == "MA") %>%
  select(Year, totalstudents) %>%
  group_by(Year) %>% 
  mutate(TS_sum = sum(totalstudents)) %>%
  ungroup() %>% 
  select(-c(totalstudents)) %>% 
  unique()

efaMa_dupes <- efaMa[rep(row.names(efaMa), 2), 1:2] 
  

plot(x = efaMass$Year, y = efaMass$TS_sum)

library(stats)
library(fUnitRoots)
library(forecast)
library(FitAR)
library(lmtest)

#make time series
ts_efaMass <- stats::ts(efaMass, start = c(2008,1), frequency = 2)
plot(ts_efaMass)


#decompose
components <- decompose(ts_efaMass)
plot(components)





# Mixed model play ------------------------------------------------------------------------------------------------

# Setup initial data



df %>%
    group_by(Year, `State Name`) %>%
    mutate(sum = sum(totalstudents)) %>%
    select(Year, `State Name`, sum) %>% 
    unique() %>%
    filter(`State Name` == "South Dakota") %>%
  ggplot(aes(x = Year, y = sum)) + geom_point() + facet_wrap(~`State Name`)
  
  #+ facet_wrap(~`State Name`)

test  <- df %>%
    group_by(Year, `Insitution`) %>% 
    mutate(pop_change = totalstudents - lag(totalstudents))


#######################################################################################

df <- efastate %>%
  select(Year, State, `State Name`,`County Name`, `County Code`, totalstudents, `Institution Name`) %>%
  filter(is.na(`County Code`) == FALSE & is.na(totalstudents) == FALSE)

library(lme4)
library(lmerTest)

hist(log(df$totalstudents))


df <- df %>%
  mutate(log_TS = log(totalstudents))

df.sub <- df %>% 
  filter(Year < 2013)


m0 <- lmer(log_TS ~ poly(Year,3) + (1 | `State Name`), data = df.sub, REML = TRUE)

m1 <- glmer(totalstudents ~ poly(Year,3) + (1 + poly(Year,3)| `State Name`), data = df.sub,
            family = poisson(link = "log"))

anova(m0,m1)


summary(rePCA(m1))
summary(m1)

base.model <- coef(m1)$`State Name`

save(base.model, file = "base_model.rds")
