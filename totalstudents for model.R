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


efb2006.17 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/ef2017a.csv")
efc2006.17 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/ef2017a.csv")
efd2017 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/ef2017a.csv")
efia2017 <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/ef2017a.csv")

sensitivity <- read_rds("/Volumes/GoogleDrive/My Drive/SI/DataScience/data/gates/sensitivity/sensitivity.rds")
sensitivity <- sensitivity %>% select(UNITID = unitid, Level, acceptance_rate, tuition_rev) %>%
  unique()


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

efastate %>% 
  group_by(`Institution Name`) %>% 
  count()

df <- efastate %>%
  select(Year, State, `State Name`,`County Name`, `County Code`, totalstudents, `Institution Name`) %>%
  filter(is.na(`County Code`) == FALSE & is.na(totalstudents) == FALSE)


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



remove.packages("lme4")

install.packages("lme4")
library(lme4)
library(lmerTest)

hist(log(df$totalstudents))


df <- df %>%
  mutate(log_TS = log(totalstudents))

df.sub <- df %>% 
  filter(Year < 2014)


# model using state as a grouping variable
m0 <- lmer(log_TS ~ poly(Year,3) + (1 | `State Name`), data = df.sub, REML = TRUE)

m1 <- glmer(totalstudents ~ poly(Year,3) + (1 + poly(Year,2)| `State Name`), data = df.sub,
            family = poisson(link = "log"))

anova(m0,m1)


summary(rePCA(m1))
summary(m1)

STATE.basemodel <- coef(m1)$`State Name`

save(base.model, file = "base_model.rds")

# model using UNITID as a grouping variable

#checks
hist(efastate$totalstudents, nclass = 200)

efastate_restricted <- efastate %>% 
  filter(Year < 2014) %>% 
  group_by(UNITID) %>% 
  mutate(cumsum = cumsum(UNITID)/UNITID) %>% 
  mutate(datacount = max(cumsum)) %>% 
  filter(datacount > 2)

t <- efastate_restricted %>% 
  group_by(UNITID) %>% 
  count() %>% unique()

hist(t$n)

#model
m2 <- glmer(totalstudents ~ poly(Year,3) + (1 | UNITID), data = efastate_restricted,
            family = poisson(link = "log"), control=glmerControl(optimizer="",optCtrl=list(maxfun=2e5)))

summary(rePCA(m2)) #cubic term not helping the model
plot(m2)

UNITID.basemodel <- coef(m2)$UNITID


# try and apply the coefficients to the years that were left out, group_by state
# amount of error in predicted vs observed
# predicted/observed
# mean of those predictions

# function to run the polynomial with the coefficients


#Predicted estimates vs. Observed

UNITID.basemodel <- UNITID.basemodel %>% 
  mutate(UNITID = as.numeric(rownames(.))) %>% 
  arrange(UNITID)

UNITID.modelmerge <- efastate %>% 
  select(UNITID, Year, totalstudents) %>% 
  filter(Year >= 2013) %>% 
  left_join(UNITID.basemodel) %>% 
  filter(UNITID == 100654) %>%
  rename(b0 = 6, 
         b1 = 7, 
         b2 = 8, 
         b3 = 9, 
         re1 = 4, 
         re2 = 5) %>% 
  group_by(UNITID) %>% 
  mutate(base.ts = totalstudents[1], x = cumsum(UNITID)/UNITID - 1) %>% ungroup() %>% 
  mutate(predVal = base.ts*exp(b0 + b1*x + b2*x + b3*x + re1*x + re2*x)) %>% 
  mutate(test = predVal[2]) %>% 
  mutate(othertest = test*exp(b0 + b1*x + b2*x + b3*x + re1*x + re2*x))

predict(m2)


futuredata <- efastate %>%
  select(UNITID, Year, totalstudents) %>% 
  filter(Year >= 2014)


futuredata$predvals <- exp(predict(m2, newdata = futuredata, allow.new.levels = TRUE))


futuredata. <- futuredata %>% 
  mutate(diff = (predvals - totalstudents), diffratio = predvals/totalstudents) %>% 
  group_by(UNITID) %>% 
  mutate(meanrawerror = mean(diff), meanratioerror = mean(diffratio)) %>% 
  select(UNITID, meanrawerror, meanratioerror, totalstudents) %>% 
  mutate(sd_ts = sd(totalstudents),
         sd_ratio = sd_ts/mean(totalstudents),
         med = median(totalstudents)) %>%
  unique() %>%
  #filter(meanratioerror < 1.5 & meanratioerror > 0.5) %>% 
  ungroup() %>% 
  select(UNITID, meanratioerror, meanrawerror, sd_ts, sd_ratio) %>% 
  unique()

futuredata. %>% ggplot(aes(x = sd_ratio, y = meanratioerror)) + geom_point()
  
summary(futuredata.$sd_ts)
summary(futuredata.$sd_ratio)
hist(futuredata.$meanratioerror)
hist(futuredata.$meanrawerror)


statecount <- futuredata. %>%
  select(UNITID,State) %>% unique() %>% 
  group_by(State) %>% 
  count() %>%
  ggplot(aes(x = reorder(factor(State),n), y = n)) + geom_bar(stat = "identity")


# M3 - include FT student count
#model

efastate_restricted. <- efastate_restricted %>% 
  group_by(UNITID) %>% 
  mutate(FT_mean = mean(FTFT), Year_mean = mean(Year)) %>% 
  mutate(FT_c = scale(FTFT - FT_mean), Year_c = scale(Year - Year_mean), TS_s = scale(totalstudents)) %>%
  filter(FTFT > 0)
  
m3 <- glmer(totalstudents ~ poly(Year_c,2) + FT_c + (1 + poly(Year_c,1) | UNITID), data = efastate_restricted.,
            family = poisson(link = "log"), control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=4e5)))
summary(rePCA(m3))

tt <- getME(m3, "theta")
ll <- getME(m3, "lower")
min(tt[ll==0])

derivs1 <- m3@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))


packageVersion("lme4")


#New data - only select institutions
ids <- as.vector(sensitivity[,1])

df <- sensitivity %>%
  left_join(efastate) %>% 
  filter(Year < 2014) %>% 
  group_by(UNITID) %>% 
  mutate(FT_mean = mean(FTFT), Year_mean = mean(Year)) %>% 
  mutate(FT_c = scale(FTFT - FT_mean), Year_c = scale(Year - Year_mean), TS_s = scale(totalstudents)) 
  
#new baseline model
new.m2.inst <- glmer(totalstudents ~ poly(Year,3) + (1 + poly(Year,3) | UNITID) , data = df,
                family = poisson(link = "log"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) #seems like cubic re is important
summary(rePCA(new.m2.inst))


new.m2.inst_state <- glmer(totalstudents ~ poly(Year,3) + (1 + poly(Year,3) | UNITID) + (1|State), data = df,
                     family = poisson(link = "log"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) 
summary(rePCA(new.m2.inst_state))

anova(new.m2,new.m2.)

new.m3.inst_state <- glmer(totalstudents ~ poly(Year,3) + FT_c + 
                             (1 + poly(Year,3) + FT_c | UNITID) + 
                             (1 |State), 
                           data = df,family = poisson(link = "log"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) 

new.m3 <- glmer(totalstudents ~ poly(Year,3) + FT_c + (1 + poly(Year,3) + FT_c | UNITID) + (1 + poly(Year,2)|State), data = df,
                  family = poisson(link = "log"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) #seems like cubic re is important




