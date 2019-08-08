library(siverse)
library(tidyverse)
library(lme4) #for mixed models

# Read in Data -------
  # Load in efastate.rds from project files

birthdata <- read_rds("G:/My Drive/SI/DataScience/data/gates/BirthData/birthdata_for_model.rds")
birthdata <- birthdata %>% 
  rename(Year = realyear, State = state)

institutions <- read_csv("G:/My Drive/SI/DataScience/data/gates/IPEDS/Full Gates Download/IPEDS Data/Institutions.csv")
institutions <- institutions %>% 
  select(UNITID = `Institution ID`, State,`Degree Granting`) %>% 
  unique()

sensitivity <- read_rds("G:/My Drive/SI/DataScience/data/gates/sensitivity/sensitivity.rds")
sensitivity <- sensitivity %>% select(UNITID = unitid, Level, acceptance_rate, tuition_rev) %>%
  unique()


# Make total and state wrapped datasets ----
df.total <- sensitivity %>%
  left_join(efastate) %>% 
  group_by(UNITID) %>% 
  mutate(count = max(cumsum(UNITID)/UNITID)) %>% ungroup()

df.total. <- df.total %>% 
  left_join(institutions) %>% 
  left_join(birthdata) 

df.total_st <- df.total. %>% 
  filter(count == 10) %>% 
  select(State, totalstudents, FTFT, Year, Level, count_t18) %>% 
  group_by(State, Year,Level) %>% 
  mutate(TS_st = sum(totalstudents), FTFT_sum = sum(FTFT)) %>% ungroup() %>% 
  select(-c(totalstudents, FTFT)) %>% 
  unique() %>% 
  mutate(count_t18_gc = scale(count_t18 - mean(count_t18, na.rm = TRUE)),
         Inst_Type = factor(Level))

cor.test(df.total_st$TS_st, df.total_st$count_t18_gc) #Yay! The .99 correlation is not real.  Most likely due to repeated rows!!!


#Generate data frame for model ----
df.model_st <- df.total_st %>% 
  filter(Year < 2014 & is.na(count_t18) == FALSE)

df.model_st %>% 
  select(State) %>% 
  unique() %>% count() #Data on all 50 states is included

#Generate data frame for model predictions
df.future_st <- df.total_st %>% 
  filter(Year >= 2014 & is.na(State) == FALSE)

#MODELS-----

# Run model with t18 grand centered interacting with Institution type and with all random effects ----
m_full <- glmer(TS_st ~ count_t18_gc*Inst_Type +
                          (1 + count_t18_gc + Inst_Type | State), 
                        data = df.model_st,
                        family = poisson(link = "log"), 
                        control=glmerControl(optCtrl=list(maxfun=3e5)))

summary(rePCA(m_full))

m_full_pred <- df.future_st %>% 
  mutate(m_full_predvals = exp(predict(m_full, newdata = df.future_st, allow.new.levels = TRUE, 
                                    re.form = ~(1 + count_t18_gc + Inst_Type| State)))) %>% 
  mutate(m_full_error = m_full_predvals - TS_st) 

#Plot predicted vs. actual
m_full_plot <- m_full_pred %>%
  ggplot(aes(x = TS_st, y = m_full_predvals, color = Inst_Type)) + geom_point() + geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Predicted Vs. Observed: Birthrate Model & Institution Type, Full model, All RE's") +
  scale_y_continuous(limits = c(0,1.5e6)) +
  facet_wrap(~Year)

    #TEXAS ----
    TX <- df.total_st %>% 
      filter(State == "TX" & Inst_Type == "2-Yr" | State == "TX" & Inst_Type == "4-Yr") %>% 
      select(Year, TS_st, count_t18) %>% 
      group_by(Year) %>% 
      mutate(totalstudents = sum(TS_st)) %>% 
      select(-c(TS_st)) %>% unique()
    
    TX.8to13 <- TX %>% 
      filter(Year < 2014)
    TX.14to17 <- TX %>%
      filter(Year >= 2014)
        #Corrleation between birthrate and total students for 2008 - 2013
        cor.test(TX.8to13$count_t18, TX.8to13$totalstudents)
        
        #Corrleation between birthrate and total students for 2014 - 2017
        cor.test(TX.14to17$count_t18, TX.14to17$totalstudents) #What we would need to show that the relationship changes is a YearXbirthrate interaction

#Run model without random effect on birthrate----
m_2 <- glmer(TS_st ~ count_t18_gc*Inst_Type +
                  (1 + Inst_Type | State), 
                data = df.model_st,
                family = poisson(link = "log"), 
                control=glmerControl(optCtrl=list(maxfun=3e5)))

summary(rePCA(m_2))

m_2_pred <- df.future_st %>% 
  mutate(m_2_predvals = exp(predict(m_2, newdata = df.future_st, allow.new.levels = TRUE, 
                                       re.form = ~(1 + Inst_Type| State)))) %>% 
  mutate(m_2_error = m_2_predvals - TS_st) 

#Plot predicted vs. actual
m_2_plot <- m_2_pred %>%
  ggplot(aes(x = TS_st, y = m_2_predvals, color = Inst_Type)) + geom_point() + geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Predicted Vs. Observed: Birthrate Model & Institution Type, Full model, All RE's") +
  scale_y_continuous(limits = c(0,1.5e6)) +
  facet_wrap(~Year)

#Descriptives on reduced model
median(m_2_pred$m_2_error)
mean(m_2_pred$m_2_error)
sum(abs(m_2_pred$m_2_error))
sum(abs(m_2_pred$m_2_error))/sum(m_2_pred$TS_st)
cor.test(m_2_pred$m_2_predvals, m_2_pred$TS_st)


#Compare full model to reduced model
m_compare_full_m2 <- m_full_pred %>% #density plot
  left_join(m_2_pred %>% select(Year,State,Inst_Type,m_2_predvals,m_2_error)) %>%
  select(Year, State, m_full_predvals, m_2_predvals,TS_st) %>% 
  gather(m_full_predvals, m_2_predvals, key = "model", value = "prediction") %>%
  mutate(error_ratio = (prediction - TS_st)/TS_st) %>% 
  ggplot(aes(x = error_ratio, color = model, group = model, fill = model, alpha = 0.5, linetype = model)) + 
  geom_density() + geom_vline(aes(group = model, xintercept=0)) +
  ggtitle("Predicted Vs. Observed: Birthrate Only vs. Birthrate & Year") + 
  scale_x_continuous(limits = c(-0.8,0.8))

m_comparebx_full_m2 <- m_full_pred %>% #boxplot
  left_join(m_2_pred %>% select(Year,State,Inst_Type,m_2_predvals,m_2_error)) %>%
  select(Year, State, m_full_predvals, m_2_predvals,TS_st) %>% 
  gather(m_full_predvals, m_2_predvals, key = "model", value = "prediction") %>%
  mutate(error_ratio = (prediction - TS_st)/TS_st) %>% 
  ggplot(aes(x = model, y = error_ratio, color = model, group = model, fill = model, alpha = 0.5, linetype = model)) + 
  geom_boxplot() +
  ggtitle("Predicted Vs. Observed: Birthrate Only vs. Birthrate & Year") 

anova(m_full,m_2)

#Run model on FTFT students
m.FTFT <- glmer(FTFT_sum ~ count_t18_gc*Inst_Type +
                  (1 + Inst_Type | State), 
                data = df.model_st,
                family = poisson(link = "log"), 
                control=glmerControl(optCtrl=list(maxfun=3e5)))

summary(m.FTFT)
m.FTFT_pred <- df.future_st %>% 
  mutate(m.FTFT_predvals = exp(predict(m.FTFT, newdata = df.future_st, allow.new.levels = TRUE, 
                                    re.form = ~(1 + Inst_Type | State)))) %>% 
  mutate(m.FTFT_error = m.FTFT_predvals - FTFT_sum) %>% 
  mutate(m.FTFT_erroratio = (m.FTFT_predvals - FTFT_sum)/FTFT_sum)

#Plot predicted vs. actual
m.FTFT_plot <- m.FTFT_pred %>%
  ggplot(aes(x = FTFT_sum, y = m.FTFT_predvals, color = Inst_Type)) + geom_point() + geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Predicted Vs. Observed: Birthrate Model & Institution Type, Full model, All RE's") +
  facet_wrap(~Year)

#Descriptives on reduced model
median(m.FTFT_pred$m.FTFT_error)
mean(m.FTFT_pred$m.FTFT_error)
sum(abs(m.FTFT_pred$m.FTFT_error))
sum(abs(m.FTFT_pred$m.FTFT_error))/sum(m.FTFT_pred$FTFT_sum)



m_compare_FT_TS <- m_2_pred %>% #boxplot
  left_join(m.FTFT_pred %>% select(Year,State,Inst_Type,m.FTFT_erroratio)) %>%
  select(Year, Inst_Type,State, m_2_predvals, m.FTFT_erroratio,TS_st) %>% 
  mutate(mTS_error_ratio = (m_2_predvals - TS_st)/TS_st) %>%
  gather(mTS_error_ratio, m.FTFT_erroratio, key = "model", value = "error_ratio") %>%
  ggplot(aes(x = error_ratio, color = model, group = model, fill = model, alpha = 0.5, linetype = model)) + 
  geom_density() + geom_vline(xintercept = 0) +
  ggtitle("Density plot of error ratio for best model on total students and FTFT") + facet_wrap(~Inst_Type)
