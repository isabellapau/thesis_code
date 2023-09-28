# PARALLEL TRENDS ASSUMPTION GRAPHS AND REGRESSIONS (TABLE 2,3,4,A.6 FIGURE 2,3)

# Load packages
library(data.table)
library(dplyr)
library(tidyr)
library(collapse)
library(lubridate)
library(stringr)
library(readxl)
# table export
library(writexl)
library(here)
library(xtable)
library(modelsummary)
# regression
library(fixest)
library(ggplot2)
#library(binsreg)
#library(sjPlot)
#library(sjlabelled)
#library(sjmisc)


options(scipen=999, digits = 3)


#create panel data from 2014-2019 
df <- df %>%
  mutate(to = as.integer(year_return))

df$to[is.na(df$to)] <- 2019

df_panel <- df[!duplicated(df$user_id),]

df_panel$year_migr <- as.integer(df_panel$year_migr) #785.531 users


df_panel <- df_panel %>%
  filter(year_migr < 2017 & year_migr > 2013) #189.756

df_panel <- df_panel %>%
  filter(to < 2020) #189.205

df_panel <- merge(df_panel[, !c("year_migr", "to")], df_panel[, list(year = seq(year_migr, to)), by = user_id], by="user_id")

# create variables treat = migr_UK, post = >2016, y = return outcomes in that year
df_panel <- df_panel %>%
  mutate(post = ifelse(year > 2016, 1, 0))

df_panel <- df_panel %>%
  mutate(y = ifelse(year_return == year, 1, 0))

df_panel <- df_panel %>%
  mutate(treat = migr_UK * post)

df_panel$y[is.na(df_panel$y)] <- 0

df_panel <- df_panel %>%
  mutate(age_year = age - (2023-year))


#parallel trends
df %>%
  filter(year_migr > 2013, year_migr < 2017, year_return < 2020) %>%
  group_by(migr_UK, year_return) %>% 
  summarise(count= log(n()), na.rm = TRUE) %>%
  ggplot(aes(x = year_return, y= count))+
  geom_line(aes(color = factor(migr_UK), group = migr_UK))+
  geom_point(aes(group = factor(migr_UK)))+
  scale_x_continuous(breaks = seq(2014,2020,2))+
  geom_vline(xintercept = 2017, color = "red", size=0.5, linetype="dashed")+
  labs(x= "Year", y = "log (n. Returning Migrants)", color = "Migrant to UK")

# event study parallel trends
df_panel[, time_to_treat := ifelse(migr_UK==1, year - 2017, 0)]

mod_twfe = feols(y ~ i(time_to_treat, migr_UK, ref = -1) | ## key interaction: time × treatment status
                   nationality_bac + year,                             ## FEs
                 cluster = ~ nationality_bac,                          ## Clustered SEs
                 data = df_panel)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = '')



#######OLS REGRESSIONS 
# OLS baseline
est_1 <- feols(y ~ migr_UK + post + migr_UK*post , df_panel, panel.id = c('user_id','year'))
#OLS control for time invariant observables (age_migr, gender, highest degree)
est_2 <- feols(y ~ migr_UK + post + migr_UK*post + highest_degree_new + age_migr + gender , df_panel, panel.id = c('user_id','year'))
#control for time invariant and time variant (age by year) observables
est_3 <- feols(y ~ migr_UK + post + migr_UK*post+ highest_degree_new + age_migr + gender + age_year, df_panel, panel.id = c('user_id','year'))

etable(est_1, est_2, est_3, tex = TRUE)


# ADDING FE
fe_est_1 <- feols(y ~ migr_UK + post + migr_UK*post + highest_degree_new + age_migr + gender + age_year | nationality_bac , df_panel, panel.id = c('user_id','year'))

fe_est_2 <- feols(y ~ migr_UK +  migr_UK*post + highest_degree_new + age_migr + gender + age_year | year , df_panel, panel.id = c('user_id','year'))

fe_est_3 <- feols(y ~ migr_UK + migr_UK*post + highest_degree_new + age_migr + gender + age_year | nationality_bac + year, df_panel, panel.id = c('user_id','year'))

etable(fe_est_1, fe_est_2, fe_est_3, tex = TRUE)
plot(fixef(fe_est_2))


####HETEROGENEOUS EFFECTS ANALYSIS
est_deg <- feols(y ~ migr_UK + post + migr_UK*post*highest_degree_new + age_migr + gender + age_year| nationality_bac + year, df_cross, panel.id = c('user_id','year'))
etable(est_deg)

est_gen <- feols(y ~ migr_UK + post + migr_UK*post*gender + highest_degree_new + age_migr + age_year| nationality_bac + year, df_cross, panel.id = c('user_id','year'))
etable(est_gen)

etable(est_deg, est_gen, tex = TRUE)
