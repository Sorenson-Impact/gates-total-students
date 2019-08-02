#Only by state
new.m2.state <- glmer(totalstudents ~ poly(Year,3) + (1 + poly(Year,3) | State) , data = df,
                      family = poisson(link = "log"), control=glmerControl(optCtrl=list(maxfun=2e5))) #seems like cubic re is important
summary(rePCA(new.m2.state))
summary(new.m2.state)

baseline.state.coef <- coef(new.m2.state)$State

#Get predicted values from model
baseline.state <- df.future %>% 
  select(State, Year, totalstudents) %>% 
  mutate(predvals = exp(predict(new.m2.state, newdata = df.future, allow.new.levels = TRUE, re.form= ~(1 + poly(Year,3) | State)))) %>% 
  mutate(under1000 = ifelse(totalstudents < 1000, "Under1000", "Above1000"), error = predvals - totalstudents) 
hist(baseline.state$totalstudents, nclass = 1000)

#Plot predicted vs. actual
baseline.state_plot <- baseline.state %>%
  filter(predvals <1e6) %>% 
  ggplot(aes(x = predvals, y = totalstudents, color = under1000)) + geom_point() + geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Predicted Vs. Observed: Baseline Model - 2 level, State, Poly3 Year RE") +
  scale_x_continuous(breaks = seq(0,100000, 10000), limits = c(0,100000)) +
  scale_y_continuous(breaks = seq(0,100000, 10000), limits = c(0,100000))
baseline.state_plot

#By both State and Institution

new.m2.inst_state <- glmer(totalstudents ~ poly(Year,3) + (1 + poly(Year,3) | UNITID) + (1|State), data = df,
                           family = poisson(link = "log"), control=glmerControl(optCtrl=list(maxfun=2e5))) 
summary(rePCA(new.m2.inst_state))
summary(new.m2.inst_state)

baseline.inst_state.coef <- coef(new.m2.inst_state)$UNITID

#Get predicted values from model
baseline.inst_state <- df.future %>% 
  select(State, Year, State, totalstudents) %>% 
  mutate(predvals = exp(predict(new.m2.inst_state, newdata = df.future, allow.new.levels = TRUE, re.form = ~(1 + poly(Year,3) | UNITID)+ (1|State)))) %>% 
  mutate(under1000 = ifelse(totalstudents < 1000, "Under1000", "Above1000"), error = predvals - totalstudents) 

hist(baseline.state$totalstudents, nclass = 1000)

#Plot predicted vs. actual
baseline.inst_state_plot <- baseline.inst_state %>%
  filter(predvals <1e6) %>% 
  ggplot(aes(x = predvals, y = totalstudents, color = under1000)) + geom_point() + geom_abline(intercept = 0, slope = 1) + 
  ggtitle("Predicted Vs. Observed: Baseline Model - 3 level, State, Poly3 Year RE @Inst, Only RanInt @State") +
  scale_x_continuous(breaks = seq(0,100000, 10000), limits = c(0,100000)) +
  scale_y_continuous(breaks = seq(0,100000, 10000), limits = c(0,100000))
baseline.inst_state_plot

#All Baseline plots
baseline.inst_plot
baseline.state_plot
baseline.inst_state_plot