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


# setwd("/Users/yeonheekim/Desktop")
data <- read_dta('data/Compiled_IPD_Data_211201.dta')
# data<- read_dta("Compiled_IPD_Data_211201.dta")

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
                     site_id == 2201 ~ "Barcelona (Cohort I), Spain", 
                     site_id == 2202 ~ "Barcelona (Cohort II), Spain",
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

## Categorizing the variables---------------------------------------------------------------------------------------------------

ls(data)
view(data_naomit%>%select(bact_inf, bact_inf1))

###MATERNAL MORBIDITY---------------------------------------------------------------------------------------------------------------------
##PREECLAMPSIA---------------------------------------------------------------------------------------------------------------------------
preeclampsia <- data_naomit%>% select(strain, pre_or_eclampsia, pre_or_eclampsiam, pre_or_eclampsia1, pre_or_eclampsia0, preeclampsia, 
                                     preeclampsia0, preeclampsia1, preeclampsiam)
preeclampsia[preeclampsia == 88] <- 0

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



