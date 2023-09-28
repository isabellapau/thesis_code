# ROBUSTNESS AND PLACEBO TESTS (TABLE 5)

# Load packages
library(data.table)
library(dplyr)
library(tidyr)
library(collapse)
library(lubridate)
library(stringr)
library(readxl)
#  table export
library(writexl)
library(here)
library(xtable)
library(modelsummary)
# regression
library(fixest)
library(ggplot2)



options(scipen=999, digits = 3)

### ROBUSTNESS
# 1. keep smallest sample and run baseline regressions on it
df_smallest_sample <- df_panel %>%
  filter(is.na(age) == FALSE & is.na(gender) == FALSE & is.na(highest_degree_new) == FALSE)


rob_est_1 <- feols(y ~ migr_UK + post + migr_UK*post , df_smallest_sample, panel.id = c('user_id','year'))

# 2. drop emigrants in June and July since most likely to be interns
df_no_inter <- df_panel %>%
  filter(month_migr != 6 & month_migr != 7)

r2_fe_est_3 <- feols(y ~ migr_UK + migr_UK*post + HIGHEST_DEGREE + age_migr + gender + age_year | nationality_bac + year, df_no_inter, panel.id = c('user_id','year'))

# 3. IRELAND
df_i <- df_panel %>%
  filter((destination_country %in%
            c("Ireland", "United Kingdom")))

ife_est_3 <- feols(y ~ migr_UK + migr_UK*post + highest_degree_new + age_migr + gender + age_year | nationality_bac + year, df_i, panel.id = c('user_id','year'))

#### PLACEBO
# treatment in 2013, sample 2010-2016, migrated 2010-2012
df_p <- df[!duplicated(df$user_id),]

df_p$year_migr <- as.integer(df_p$year_migr) #822495 users

df_p <- df_p %>%
  filter(year_migr < 2013 & year_migr > 2009) #432251

df_p <- df_p%>%
  mutate(to = replace(to, to == 2020, 2016))

df_p <- df_p %>%
  filter(to < 2017) #189.205

df_p <- merge(df_p[, !c("year_migr", "to")], df_p[, list(year = seq(year_migr, to)), by = user_id], by="user_id")

#treat = migr_UK, post = >2016, y = return in that year

df_p <- df_p %>%
  mutate(post = ifelse(year > 2012, 1, 0))

df_p<- df_p %>%
  mutate(y = ifelse(year_return == year, 1, 0))

df_p <- df_p %>%
  mutate(treat = migr_UK * post)

df_p$y[is.na(df_p$y)] <- 0

df_p <- df_p %>%
  mutate(age_year = age - (2023-year))

# PLACEBO REGRESSION 
pfe_est_3 <- feols(y ~ migr_UK + migr_UK*post + highest_degree_new + age_migr + gender + age_year | nationality_bac + year, df_p, panel.id = c('user_id','year'))



# PRINT REOBUSTNESS AND PLACEBO TABLE
etable(rob_est_1,r2_fe_est_3, ife_est_3, pfe_est_3, tex = TRUE)




