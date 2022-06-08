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


setwd("/Users/yeonheekim/Desktop")
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
                     site_id == 17~ "Puerto Rico, USA", 
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
  mutate(strain1 = 
           case_when((date_character >= "2020-01-01") & (date_character <= "2020-07-15") & (country == "Chile") ~ "Pre-Alpha", 
                     (date_character >= "2020-07-16") & (date_character <= "2021-4-15") & (country == "Chile") ~ "Unknown", 
                     (date_character >= "2021-04-16") & (date_character <= "2021-08-15") & (country == "Chile") ~ "Overlaps",
                     (date_character >= "2021-08-16") & (date_character <= "2022-01-15") & (country == "Chile") ~ "Delta",
                     
                     (date_character >= "2020-01-01") & (date_character <= "2020-10-15") & (country == "Madrid, Spain") ~ "Pre-Alpha", 
                     (date_character >= "2020-10-16") & (date_character <= "2020-11-15") & (country == "Madrid, Spain") ~ "Overlaps", 
                     (date_character >= "2020-11-16") & (date_character <= "2021-04-15") & (country == "Madrid, Spain") ~ "Alpha",
                     (date_character >= "2021-04-16") & (date_character <= "2021-12-15") & (country == "Madrid, Spain") ~ "Delta",
                     
                     (date_character >= "2020-01-01") & (date_character <= "2020-11-15") & (country == "Mexico") ~ "Pre-Alpha", 
                     (date_character >= "2020-11-16") & (date_character <= "2021-08-15") & (country == "Mexico") ~ "Overlaps", 
                     (date_character >= "2021-08-16") & (date_character <= "2021-12-15") & (country == "Mexico") ~ "Delta",
                  
                     (date_character >= "2020-01-01") & (date_character <= "2020-05-15") & (country == "Columbia") ~ "Pre-Alpha", 
                     (date_character >= "2020-05-16") & (date_character <= "2021-04-15") & (country == "Columbia") ~ "Unknown", 
                     (date_character >= "2021-04-16") & (date_character <= "2021-08-15") & (country == "Columbia") ~ "Mu", 
                     (date_character >= "2021-08-16") & (date_character <= "2021-09-15") & (country == "Columbia") ~ "Overlaps", 
                     (date_character >= "2021-09-15") & (date_character <= "2022-01-15") & (country == "Columbia") ~ "Delta", 
                    
                     (date_character >= "2020-01-01") & (date_character <= "2020-11-15") & (country == "Puerto Rico, USA") ~ "Pre-Alpha", 
                     (date_character >= "2020-11-16") & (date_character <= "2021-07-15") & (country == "Puerto Rico, USA") ~ "Overlaps", 
                     (date_character >= "2021-07-16") & (date_character <= "2021-10-15") & (country == "Puerto Rico, USA") ~ "Delta", 
                     (date_character >= "2021-10-16") & (date_character <= "2021-12-15") & (country == "Puerto Rico, USA") ~ "Overlaps",
                     (date_character >= "2021-12-16") & (date_character <= "2022-05-15") & (country == "Puerto Rico, USA") ~ "Omicron",
                     
                     (date_character >= "2020-01-01") & (date_character <= "2020-11-15") & (country == "Kenya") ~ "Pre-Alpha", 
                     (date_character >= "2020-11-16") & (date_character <= "2021-07-15") & (country == "Kenya") ~ "Overlaps", 
                     (date_character >= "2021-07-16") & (date_character <= "2021-09-15") & (country == "Kenya") ~ "Delta", 
                     (date_character >= "2021-09-16") & (date_character <= "2021-11-15") & (country == "Kenya") ~ "Overlaps", 
                     (date_character >= "2021-11-16") & (date_character <= "2022-04-15") & (country == "Kenya") ~ "Omicron",
                     
                     TRUE ~ "Unknown"))


view(data_naomit%>%select(covid_sympdate, site_id, country, date_character, strain))

## Categorizing the variables---------------------------------------------------------------------------------------------------

ls(data)
view(data_naomit%>%select(bact_inf, bact_inf1))

###MATERNAL MORBIDITY---------------------------------------------------------------------------------------------------------------------
##PREECLAMPSIA---------------------------------------------------------------------------------------------------------------------------
preeclampsia <- data_naomit%>% select(strain, pre_or_eclampsia, pre_or_eclampsiam, pre_or_eclampsia1, pre_or_eclampsia0, preeclampsia, 
                                     preeclampsia0, preeclampsia1, preeclampsiam)
preeclampsia[preclampsia == 88] <- 0

pre_or_eclampsia<- data_naomit%>%select(strain, country, pre_or_eclampsia)
pre_or_eclampsia_t<- data.frame(pre_or_eclampsia %>% group_by(strain)%>% summarise(counting1=(sum(pre_or_eclampsia==1))))

pre_or_eclampsiam<- data_naomit%>%select(strain, country, pre_or_eclampsiam)
pre_or_eclampsiam_t <- data.frame(pre_or_eclampsiam %>% group_by(strain)%>% summarise(counting2=sum(pre_or_eclampsiam==1)))

pre_or_eclampsia1<- data_naomit%>%select(strain, country, pre_or_eclampsia1)
pre_or_eclampsia1_t<- data.frame(pre_or_eclampsia1 %>% group_by(strain)%>% summarise(counting3=(sum(pre_or_eclampsia1==1))))

pre_or_eclampsia0 <- data_naomit%>%select(strain, country, pre_or_eclampsia0)
pre_or_eclampsia0_t<- data.frame(pre_or_eclampsia0 %>% group_by(strain)%>% summarise(counting4=(sum(pre_or_eclampsia0==1))))

preeclampsia11 <- data_naomit%>%select(strain, country, preeclampsia)
preeclampsia_t<- data.frame(preeclampsia11 %>% group_by(strain)%>% summarise(counting5=(sum(preeclampsia==1))))

preeclampsia0 <- data_naomit%>%select(strain, country, preeclampsia0)
preeclampsia0_t<- data.frame(preeclampsia0  %>% group_by(strain)%>% summarise(counting6=(sum(preeclampsia0==1))))

preeclampsia1 <- data_naomit%>%select(strain, country, preeclampsia1)
preeclampsia1_t<- data.frame(preeclampsia1  %>% group_by(strain)%>% summarise(counting7=(sum(preeclampsia1==1))))

preeclampsiam <- data_naomit%>%select(strain, country, preeclampsiam)
preeclampsiam_t<- data.frame(preeclampsiam  %>% group_by(strain)%>% summarise(counting8=(sum(preeclampsiam==1))))

preclampsia_count <- list(pre_or_eclampsia_t, pre_or_eclampsiam_t, pre_or_eclampsia1_t, pre_or_eclampsia0_t, preeclampsia_t, preeclampsia0_t, preeclampsia1_t,
                preeclampsiam_t)

preclampsia_count <- data.frame(preclampsia_count %>% reduce(full_join, by = "strain"))

preclampsia_count$preclampsia_total = rowSums(preclampsia_count[,c(2:9)])
preclampsia_count

sum(preeclampsia==1)

##INFECTION------------------------------------------------------------------------------------------





###NEONATAL MORBIDITY---------------------------------------------------------------------------------
##stillbirth
view(data_naomit%>%select(stillbirth_280, stillbirth_281, stillbirth_28m))

stillbirth <- data_naomit%>% select(stillbirth_280, stillbirth_281, stillbirth_28m)

stillbirth280 <- data_naomit%>%select(strain, country, stillbirth_280)
stillbirth280_t <- data.frame(stillbirth280 %>% group_by(strain)%>% 
                                summarise(counting1=(sum(stillbirth_280==1))))
stillbirth280_t

stillbirth281 <- data_naomit%>%select(strain, country, stillbirth_281)
stillbirth281_t <- data.frame(stillbirth281 %>% group_by(strain)%>% 
                                summarise(counting2=(sum(stillbirth_281==1))))
stillbirth281_t

stillbirth28m <- data_naomit%>%select(strain, country, stillbirth_28m)
stillbirth28m_t <- data.frame(stillbirth28m %>% group_by(strain)%>% 
                                summarise(counting3=(sum(stillbirth_28m==1))))

stillbirth_count <- list(stillbirth280_t, stillbirth281_t, stillbirth28m_t)

stillbirth_count <- data.frame(stillbirth_count %>% reduce(full_join, by = "strain"))

stillbirth_count
stillbirth_count$stillbirth_total = rowSums(preclampsia_count[,c(2:4)])
stillbirth_count



