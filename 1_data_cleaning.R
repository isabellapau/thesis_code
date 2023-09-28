## First code to run before any other code: 
##data cleaning and creation necessary variables 
# TABLES A.2, A.3, A.4, A.5

# Load packages
library(arrow)
library(data.table)
library(dplyr)
library(tidyr)
library(collapse)
library(lubridate)
library(tictoc)
library(xtable)
library(ggplot2)

options(scipen=999)


# Data path
path_in <- "C:/Master/Thesis/data/v5_bac_EU/bac_EU_v3"

# Read data
df_conn <- open_dataset(path_in)

# read in variables using dplyr
tic()
df <- df_conn %>%
  select(everything()) %>%
  collect()
toc()

# Transform to data.table
setDT(df)


# CLEAN DATA
# delete duplicates
df <- df[!duplicated(df)] 

#length(unique(df$user_id)) #7724240

# temporary_migr set to NA when not migrant 
df$temporary_migr[df$migrant == 0] <- NA

#convert dates to dates values 
df$date_migr <- ymd(df$date_migr)
df$date_return <- ymd(df$date_return)
df$date_remigr <- ymd(df$date_remigr)


# CREATE NEW VARIABLES
#from dates create year variables 
df$year_migr <- year(df$date_migr)
df$year_return <- year(df$date_return)
df$year_remigr <- year(df$date_remigr)

# from dates vars create month variables 
df$month_migr <- month(df$date_migr)
df$month_return <- month(df$date_return)
df$month_remigr <- month(df$date_remigr)

# length of stay 
df$months_stay <- as.numeric(as.period(interval(df$date_migr, df$date_return)), "months")
df$months_stay_remigr <- as.numeric(as.period(interval(df$date_migr, df$date_remigr)), "months")

# gender
df <- df %>%
  mutate(gender = ifelse(F_PROB > 0.9, "Female", "Male"))

# age
df <- df %>%
  mutate(age_migr = age - (2023-year_migr))

df <- df %>%
  mutate(age_ret = age - (2023-year_return))

df <- df %>%
  mutate(
    age_group_ret = case_when(
      age_ret > 17 & age_ret <= 24 ~ "18-24",
      age_ret > 24 & age_ret <= 30 ~ "25-30",
      age_ret > 30 & age_ret <= 35 ~ "31-35",
      age_ret > 34 & age_ret <= 40 ~ "35-40",
      age_ret > 40 & age_ret <= 45 ~ "41-45",
      age_ret > 44 & age_ret <= 50 ~ "45-50",
      age_ret > 50 & age_ret <= 55 ~ "51-55",
    )
  )

df <- df %>%
  mutate(
    age_group_migr = case_when(
      age_migr > 17 & age_migr <= 24 ~ "18-24",
      age_migr > 24 & age_migr <= 30 ~ "25-30",
      age_migr > 30 & age_migr <= 35 ~ "31-35",
      age_migr > 34 & age_migr <= 40 ~ "35-40",
      age_migr > 40 & age_migr <= 45 ~ "41-45",
      age_migr > 44 & age_migr <= 50 ~ "45-50",
      age_migr > 50 & age_migr <= 55 ~ "51-55",
    )
  )




# Create UK data: needed for mobility graph 
# restrict to migr_UK = 1
df_UK <- df[migr_UK == 1,]

# drop migrated, return_UK columns
#df_UK <- subset(df_UK, select = -c(return_UK))

# keep unique values
df_UK <- df_UK[!duplicated(df_UK)]

#length(unique(df_UK$user_id)) #


# N. USERS AND MIGRANTS BY COUNTRY
# rank n. users by nationality
rank_users <- df %>%
  group_by(nationality_bac) %>% 
  summarise(count= n()) %>% 
  arrange(desc(count)) 

rank_origin_countries.table <- xtable(rank_users)
print(rank_origin_countries.table, include.rownames = FALSE)

# rank migrants to any country and filter accordingly
rank_migr  <- df %>%
  filter(year_migr > 2013 & year_migr < 2020) %>%
  filter(migrant == 1)%>%
  group_by(nationality_bac) %>% 
  summarise(count= n()) %>% 
  arrange(desc(count)) #select countries at the top of the list

rank_origin_countries_migr.table <- xtable(rank_migr)
print(rank_origin_countries_migr.table, include.rownames = FALSE)

# rank to find countries with most migrants to UK
rank_migr_UK <- df_UK %>%
  filter(year_migr > 2013, year_migr < 2020) %>%
  group_by(nationality_bac) %>% 
  summarise(count= n()) %>% 
  arrange(desc(count)) 

rank_origin_countries_UK.table <- xtable(rank_migr_UK)
print(rank_origin_countries_UK.table, include.rownames = FALSE)


# SELECT 9 NATIONALITIES
# filter both df and df_UK to keep only top countries in home country rank for emigrants
df<- df %>%
  filter(nationality_bac %in% 
           c("France", "Spain", "Germany", "Italy", "Switzerland", "Netherlands", "Belgium", "Greece","Portugal"))

df_UK <- df_UK %>%
  filter(nationality_bac %in% 
           c("France", "Spain", "Germany", "Italy", "Switzerland", "Netherlands", "Belgium", "Greece","Portugal"))

#length(unique(df$user_id)) #6584580
#length(unique(df_UK$user_id)) #318837


# N. IMMIGRANTS BY DESTINATION COUNTRY TO SELECT MOST FREQUENT EU DESTINATIONS
# rank to find countries with most immigrants
rank_destination <- df %>%
  filter(year_migr > 2013, year_migr < 2020, nationality_bac != destination_country) %>%
  group_by(destination_country) %>% 
  summarise(count= n()) %>% 
  arrange(desc(count)) %>%
  slice(1:15) 

rank_d.table <- xtable(rank_destination)
print(rank_d.table, include.rownames = FALSE)


# filter df for destinations in EU with most immigrants
df <- df %>%
  filter((destination_country %in%
            c("Germany", "Spain", "Switzerland", "Netherlands", "France", "Belgium", "Italy", "Ireland", "United Kingdom")))

#length(unique(df$user_id)) #816476
