rm(list=ls())

install.packages("dplyr")
install.packages("tidyverse")
install.packages("haven")
install.packages("magrittr")
install.packages("lubridate")
library(dplyr)
library(tidyverse)
library(haven)
library(readr)
library(lubridate)


setwd("C:/Users/user/Desktop")
data<- read_dta("Compiled_IPD_Data_211201.dta")
data<-data%>% 
  mutate(country =
           case_when(site_id == 2 ~ "Hong Kong",
                     site_id == 3 ~ "Chile", 
                     site_id == 5 ~ "Madrid, Spain", 
                     site_id == 10 ~ "Mexico", 
                     site_id == 11 ~ "Columbia", 
                     site_id == 13 ~ "India", 
                     site_id == 14 ~ "Africa", 
                     site_id == 15 ~ "Kenya",
                     site_id == 17~ "Puerto Rico", 
                     site_id == 19 ~ "Rome, Italy",
                     site_id == 20 ~ "Blank", 
                     site_id == 21 ~ "Turkey", 
                     site_id == 2201 ~ "Barcelona, Spain", 
                     site_id == 2202 ~ "Barcelona, Spain",
                     site_id == 25 ~ "New Jersey, USA",
                     site_id == 28 ~ "South Africa", 
                     site_id == 29 ~ "Illinois, USA"))

data_naomit <- data%>% drop_na(covid_sympdate)

data_naomit$date_character <- as.character(data_naomit$covid_sympdate)

unique(data_naomit$country)
  
data_naomit <- data_naomit%>%
  mutate(strain = 
           case_when((date_character >= "2020-01-01") & (date_character <= "2020-07-31") & (country == "Chile") ~ "Alpha", 
                     (date_character >= "2020-08-01") & (date_character <= "2020-10-31") & (country == "Chile") ~ "Delta", 
                     (date_character >= "2020-11-01") & (date_character <= "2021-05-31") & (country == "Chile") ~ "Omicron",
                     (date_character >= "2020-01-01") & (date_character <= "2020-04-31") & (country == "Madrid, Spain") ~ "Alpha", 
                     (date_character >= "2020-05-01") & (date_character <= "2020-08-31") & (country == "Madrid, Spain") ~ "Beta", 
                     (date_character >= "2020-09-01") & (date_character <= "2020-11-31") & (country == "Madrid, Spain") ~ "Omicron",
                     (date_character >= "2020-01-01") & (date_character <= "2020-04-31") & (country == "Mexico") ~ "Alpha", 
                     (date_character >= "2020-05-01") & (date_character <= "2020-08-31") & (country == "Mexico") ~ "Beta", 
                     (date_character >= "2020-09-01") & (date_character <= "2020-11-31") & (country == "Mexico") ~ "Delta",
                     (date_character >= "2020-12-01") & (date_character <= "2021-01-31") & (country == "Mexico") ~ "Gamma",
                     (date_character >= "2021-02-01") & (date_character <= "2021-04-31") & (country == "Mexico") ~ "Omicron", 
                     (date_character >= "2020-01-01") & (date_character <= "2020-04-31") & (country == "Columbia") ~ "Alpha", 
                     (date_character >= "2020-05-01") & (date_character <= "2020-08-31") & (country == "Columbia") ~ "Beta", 
                     (date_character >= "2020-09-01") & (date_character <= "2020-11-31") & (country == "Columbia") ~ "Delta", 
                     (date_character >= "2020-12-01") & (date_character <= "2021-01-31") & (country == "Columbia") ~ "Gamma", 
                     (date_character >= "2021-02-01") & (date_character <= "2021-04-31") & (country == "Columbia") ~ "Omicron", 
                     (date_character >= "2020-01-01") & (date_character <= "2020-04-31") & (country == "Puerto Rico") ~ "Alpha", 
                     (date_character >= "2020-05-01") & (date_character <= "2020-08-31") & (country == "Puerto Rico") ~ "Beta", 
                     (date_character >= "2020-09-01") & (date_character <= "2020-11-31") & (country == "Puerto Rico") ~ "Delta", 
                     (date_character >= "2020-12-01") & (date_character <= "2021-01-31") & (country == "Puerto Rico") ~ "Gamma",
                     (date_character >= "2021-02-01") & (date_character <= "2021-04-31") & (country == "Puerto Rico") ~ "Omicron",
                     (date_character >= "2020-05-01") & (date_character <= "2020-08-31") & (country == "Kenya") ~ "Beta", 
                     (date_character >= "2020-09-01") & (date_character <= "2020-11-31") & (country == "Kenya") ~ "Delta", 
                     (date_character >= "2020-12-01") & (date_character <= "2021-01-31") & (country == "Kenya") ~ "Gamma", 
                     (date_character >= "2021-02-01") & (date_character <= "2021-04-31") & (country == "Kenya") ~ "Omicron", 
                     TRUE ~ "NA"))


view(data_naomit%>%select(covid_sympdate, site_id, country, date_character, strain))






























