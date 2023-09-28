## GRAPHS MOBILITY AND DESCRIPTIVE EVIDENCE (FIGURES 4,5 TABLE 1, FIGURES A.1, A.2, A.3)

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
# Graphs
library(ggplot2)



options(scipen=999)


# MOBILITY GRAPHS

# PLOT immigration to UK 
df_UK %>%
  filter(year_migr > 2013, year_migr < 2020) %>%
  group_by(nationality_bac, year_migr) %>% 
  summarise(count= n(), na.rm = TRUE) %>%
  ggplot(aes(x = year_migr, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  stat_summary(fun = mean, geom = "line", linewidth = 1)+  
  scale_x_continuous(breaks = seq(2014,2019,2))+
  labs(x= "Year", y = "n. Emigrants", color = "Nationality")

# PLOT return migration from UK
df_UK %>%
  filter(year_return > 2013, year_return < 2020) %>%
  group_by(nationality_bac, year_return) %>% 
  summarise(count= n(), na.rm = TRUE) %>%
  ggplot(aes(x = year_return, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  labs(x= "Year", y = "n. Returning migrants", color = "Nationality")

#UK further migration
df_UK %>%
  filter(year_remigr > 2013, year_remigr < 2020) %>%
  group_by(nationality_bac, year_remigr) %>% 
  summarise(count= n(), na.rm = TRUE) %>%
  ggplot(aes(x = year_remigr, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  stat_summary(fun = mean, geom = "line", size = 1)+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  labs(x= "Year", y = "n. migrants further emigrating", color = "Nationality")

# PLOT immigration to anywhere except UK by origin
df %>%
  filter(nationality_bac != destination_country, migrant == 1, migr_UK == 0, year_migr > 2013, year_migr <2020) %>%
  group_by(nationality_bac, year_migr) %>% 
  summarise(count= n(), na.rm = TRUE) %>%
  ggplot(aes(x = year_migr, y= count))+
  geom_line(aes(group = nationality_bac, color = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  labs(x= "Year", y = "n. Emigrants", color = "Nationality")


# PLOT return migration from anywhere except UK by origin
df %>%
  filter(nationality_bac != destination_country, migrant == 1, migr_UK == 0, year_return > 2013, year_return < 2020) %>%
  group_by(nationality_bac, year_return) %>% 
  summarise(count= n(), na.rm = TRUE) %>%
  ggplot(aes(x = year_return, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  labs(x= "Year", y = "n. Returning migrants", color = "Nationality")

#to anywhere further migration
df %>%
  filter(nationality_bac != destination_country, migrant == 1, migr_UK == 0, year_remigr > 2013, year_remigr < 2020) %>%
  group_by(nationality_bac, year_remigr) %>% 
  summarise(count= n(), na.rm = TRUE) %>%
  ggplot(aes(x = year_remigr, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  stat_summary(fun = mean, geom = "line", size = 1)+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  labs(x= "Year", y = "n. migrants further emigrating", color = "Nationality")


#### RETURN RATE MOBILITY GRAPHS
#UK
# n migrants by country: relatives n with tot users by country
number_UK_migrants <- df_UK %>%
  filter(year_migr > 2008)%>%
  group_by(nationality_bac, year_migr) %>%
  distinct() %>%
  count()%>%
  rename(migrants = n)%>%
  rename(year = year_migr)

number_UK_return <- df_UK %>%
  filter(temporary_migr == 1 & year_return > 2008) %>%
  group_by(nationality_bac, year_return) %>%
  distinct() %>%
  count()%>%
  rename(returning = n)%>%
  rename(year = year_return)

number_UK_further <- df_UK %>%
  filter(year_remigr > 2008)%>%
  group_by(nationality_bac, year_remigr) %>%
  distinct() %>%
  count()%>%
  rename(further = n)%>%
  rename(year = year_remigr)

number_tot_users <- df %>%
  group_by(nationality_bac) %>%
  distinct() %>%
  count() %>%
  rename(tot_users = n)

number_UK <- merge(number_UK_migrants, number_tot_users, by='nationality_bac')
number_UK <- merge(number_UK, number_UK_return, by = c('nationality_bac', 'year'))
number_UK <- merge(number_UK, number_UK_further, by = c('nationality_bac', 'year'))

number_UK <- number_UK %>%
  mutate(net = migrants - returning - further)

number_UK <- number_UK %>%
  group_by(nationality_bac) %>%
  mutate(stock = cumsum(net))

number_UK <- number_UK %>%
  group_by(nationality_bac) %>%
  mutate(stock = cumsum(net))

number_UK <- number_UK %>%
  group_by(nationality_bac) %>%
  mutate(rate = returning/stock)

number_UK <- number_UK %>%
  group_by(nationality_bac) %>%
  mutate(lag_stock = lag(stock))

number_UK <- number_UK %>%
  group_by(nationality_bac) %>%
  mutate(lag_migr = lag(migrants))


# plot as a share of lag stock 
# return
number_UK %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= (returning/lag_stock)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")

# secondary migr
number_UK %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= (further/lag_stock)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")

# out-migr
number_UK %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= ((returning+further)/lag_stock)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")


# plot as a share of lag immigrants
# return
number_UK %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= (returning/lag_migr)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")

# further
number_UK %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= (further/lag_migr)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")

# out-migr
number_UK %>%
  filter(year > 2013, year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= ((returning+further)/lag_migr)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")



#non_UK
#table for any migrant 
number_migrants <- df %>%
  filter(year_migr > 2008)%>%
  filter(migrant == 1,  nationality_bac != destination_country, migr_UK == 0)%>%
  group_by(nationality_bac, year_migr) %>%
  distinct() %>%
  count()%>%
  rename(migrants = n)%>%
  rename(year = year_migr)

number_return <- df %>%
  filter(year_return > 2008)%>%
  filter(temporary_migr == 1, nationality_bac != destination_country , migr_UK == 0)%>%
  group_by(nationality_bac, year_return) %>%
  distinct() %>%
  count()%>%
  rename(returning = n)%>%
  rename(year = year_return)

number_further <- df %>%
  filter(year_remigr > 2008, nationality_bac != destination_country , migr_UK == 0)%>%
  group_by(nationality_bac, year_remigr) %>%
  distinct() %>%
  count()%>%
  rename(further = n)%>%
  rename(year = year_remigr)


number <- merge(number_migrants, number_tot_users, by='nationality_bac')
number <- merge(number, number_return, by = c('nationality_bac', 'year'))
number <- merge(number, number_further, by = c('nationality_bac', 'year'))

number <- number %>%
  mutate(net = migrants - returning - further)

number <- number %>%
  group_by(nationality_bac) %>%
  mutate(stock = cumsum(net))

number <- number %>%
  group_by(nationality_bac) %>%
  mutate(lag_stock = lag(stock))

number <- number %>%
  group_by(nationality_bac) %>%
  mutate(lag_migr = lag(migrants))



# plot as a share of lag stock 
# return
number %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= (returning/lag_stock)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")

# secondary migr
number %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= (further/lag_stock)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")

# out-migr
number %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= ((returning+further)/lag_stock)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")


# plot as a share of lag immigrants
# return
number %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= (returning/lag_migr)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")

# further
number %>%
  filter(year > 2013,  year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= (further/lag_migr)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")

# out-migr
number %>%
  filter(year > 2013, year< 2020) %>%
  group_by(nationality_bac, year) %>% 
  summarise(count= ((returning+further)/lag_migr)*1000, na.rm = TRUE) %>%
  ggplot(aes(x = year, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  scale_x_continuous(breaks = seq(2014,2019,2))+
  stat_summary(fun.y = mean, geom = "line", size = 1)+
  labs(x= "Year", y = "rate per 1.000", color = "Nationality")




# BALANCING TEST
# UK before Brexit
df_UK %>%
  filter(year_migr<2017, year_migr > 2013)%>%
  summarise(mean_age = mean(age_migr, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            mean_stay = mean(months_stay, na.rm = TRUE),
            sd_stay = sd(months_stay, na.rm = TRUE))

df_UK_before <- df_UK %>%
  filter(year_migr<2017, year_migr > 2013)

prop.table(table(df_UK_before$highest_degree_new))
prop.table(table(df_UK_before$gender))

# migrant to other countries before Brexit
df %>%
  filter(year_migr<2017, year_migr > 2013)%>%
  summarise(mean_age = mean(age_migr, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            mean_stay = mean(months_stay, na.rm = TRUE),
            sd_stay = sd(months_stay, na.rm = TRUE))

df_before <- df %>%
  filter(year_migr<2017, year_migr > 2013)

prop.table(table(df_before$highest_degree_new))
prop.table(table(df_before$gender))


#Bar plot of age distribution
#all migrants
df %>%
  filter(age < 65, year_migr > 2013, year_migr < 2017)%>%
  ggplot( aes(x=age)) +
  geom_bar()+
  scale_x_continuous(breaks = seq(15,65,5))+
  labs(x= "Age", y = "n. users")

# to UK
df %>%
  filter(age < 65, year_migr > 2013, year_migr < 2017, migr_UK == 1)%>%
  ggplot( aes(x=age)) +
  geom_bar()+
  scale_x_continuous(breaks = seq(15,65,5))+
  labs(x= "Age", y = "n. users")

# to non_UK
df %>%
  filter(age < 65, year_migr > 2013, year_migr < 2017, migr_UK == 0)%>%
  ggplot( aes(x=age)) +
  geom_bar()+
  scale_x_continuous(breaks = seq(15,65,5))+
  labs(x= "Age", y = "n. users")


#Bar plot of highest degree distribution
# all migrants
df %>%
  filter(year_migr > 2013, year_migr < 2017 )%>%
  ggplot( aes(x=highest_degree_new)) +
  geom_bar()+
  labs(x= "Highest Degree", y = "n. users")

# to UK
df %>%
  filter(year_migr > 2013, year_migr < 2017, migr_UK == 1)%>%
  ggplot( aes(x=highest_degree_new)) +
  geom_bar()+
  labs(x= "Highest Degree", y = "n. users")

# to non_UK
df %>%
  filter(year_migr > 2013, year_migr < 2017, migr_UK == 0)%>%
  ggplot( aes(x=highest_degree_new)) +
  geom_bar()+
  labs(x= "Highest Degree", y = "n. users")

