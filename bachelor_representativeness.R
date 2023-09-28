# INFERRING NATIONALITY FROM BACHELOR: 
# Table A.1

# Load packages
library(readxl)
library(data.table)
library(dplyr)
library(collapse)
library(lubridate)
library(tidyr)
# Nice table export
library(writexl)
library(here)
library(xtable)


options(scipen=999)


#read outbound tertiary students
outbound <- read_excel("C:/Master/Thesis/data/Total outbound internationally mobile tertiary students studying abroad, all countries, both sexes (number).xlsx")

#read in country students all tertiary, 6 and secondary
number_tertiary <- read_excel("C:/Master/Thesis/data/Enrolment in tertiary education, all programmes, both sexes (number).xlsx")
number_bach <- read_excel("C:/Master/Thesis/data/Enrolment in tertiary education, ISCED 6 programmes, both sexes (number).xlsx")
number_secondary <- read_excel("C:/Master/Thesis/data/Enrolment in secondary education, both sexes (number).xlsx")

#read inbound students 
inbound <- read_excel("C:/Master/Thesis/data/Total inbound internationally mobile students, both sexes (number).xlsx")

#calculate share of outbound to incountry for each specification of in-country
long_number_tertiary <- number_tertiary %>%
  pivot_longer(cols = c(contains('2')),
               names_to = 'Year',
               values_to = 'Enrolment')

long_outbound <- outbound %>%
  pivot_longer(cols = c(contains('2')),
               names_to = 'Year',
               values_to = 'Outbound')

long_inbound <- inbound %>%
  pivot_longer(cols = c(contains('2')),
               names_to = 'Year',
               values_to = 'Inbound')

share_out_tertiary <- left_join(long_outbound, long_number_tertiary, by = c('LOCATION', 'Country','Year'))
share_out_tertiary <- left_join(share_out_tertiary, long_inbound, by = c('LOCATION', 'Country','Year'))

share_out_tertiary <- transform(share_out_tertiary, Share = Outbound / (Enrolment-Inbound))


share_inbound <- left_join(long_inbound, long_number_tertiary, by = c('LOCATION', 'Country','Year'))

share_inbound <- transform(share_inbound, Share = Inbound / Enrolment)


#descriptives country, year, number outbound, only for selected countries
share_out_tertiary_table_year <- share_out_tertiary %>%
  filter(Year > 2009, Country %in% 
           c("France", "Spain", "Germany", "Italy", "Switzerland", "Netherlands", "Belgium", "Greece", "United Kingdom of Great Britain and Northern Ireland")) %>%
  select(!c('Outbound','Enrolment', 'Inbound')) %>%
  pivot_wider(names_from = Year, values_from = c('Share'))

share_out_tertiary_table <- share_out_tertiary %>%
  filter(Year > 2009, Country %in% 
           c("France", "Spain", "Germany", "Italy", "Switzerland", "Netherlands", "Belgium", "Greece", "United Kingdom of Great Britain and Northern Ireland")) %>%
  group_by(Country)%>%
  summarise(Outbound = sum(Outbound),
            Inbound = sum(Inbound, na.rm = TRUE),
            Enrolment = sum(Enrolment, na.rm = TRUE),
            Share_out = Outbound/(Enrolment-Inbound),
            Share_in = Inbound/Enrolment)

write.table(share_out_tertiary_table, file = "share_bac.txt", sep = ",", quote = FALSE, row.names = F)

x_official <- xtable(share_out_tertiary_table)
print(x_official)
