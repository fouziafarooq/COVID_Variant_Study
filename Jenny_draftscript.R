install.packages("dplyr")
install.packages("tidyverse")
install.packages("haven")
library(dplyr)
library(tidyverse)
library(haven)
library(readr)

rm(list=ls())

setwd("C:/Users/user/Desktop")
data<- read_dta("Compiled_IPD_Data_211201.dta")

ls(data)
view(data%>%select(covid_sympdate, site_id))
data$site_id <- as.logical(data$site_id)


unique(data$site_id)

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
view(data%>%select(covid_sympdate, site_id, country))

unique(data$country)
unique(data_naomit$country)

data_naomit <- data_naomit%>%
  mutate(strain = 
           case_when((covid_sympdate >= 2020-01-01) & (covid_sympdate <= 2020-07-31) & (country == "Chile") ~ "Alpha", 
                     (covid_sympdate >= 2020-01-01) & (covid_sympdate <= 2020-04-31) & (country == "Madrid, Spain") ~ "Alpha", 
                     (covid_sympdate )))



