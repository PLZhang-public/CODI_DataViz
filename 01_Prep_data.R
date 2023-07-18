## Author: Patrick Zhang
## Purpose: Prep raw synthetic data
## Start Date: 3/14/23

## ------------------------ Background --------------------------- ##
# Prep data for EDA
# vb - Pull hispanic out of race_cat to match census

## ------------------------ Tasks --------------------------- ##

## ------------------------- Questions -------------------------- ##

## -------------------------Start Code -------------------------- ##

##### Prep data #####
## import libraries
library(stringr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(readxl)
library(readr)
library(data.table)
library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)


## ------------------------- Census tract data -------------------------- ##
## Goal - pull out census tracts in need
## Maybe just use census tracts in chapel hill and durham?

## Import census tract data
census_tracts_raw <- read_file("C:/Users/rvy3/OneDrive - CDC/CDC_Work/PHIFP/Capstone/DMI_SQ_Capstone/Data/data_vdw_census_demog_b.sql")

## Pull out census tract
census_tracts_d <- strsplit(x = census_tracts_raw, split = "\n", fixed = F)

census_tracts_d2 <- lapply(census_tracts_d[[1]], substring, first=36, last=46)
# census_tracts_d2

## Import data
census_tracts_d3 <- unlist(census_tracts_d2)

census_tracts_d4 <- unique(census_tracts_d3)

NC_Census_Tracts_all <- data.frame(census_tracts_d4)

## ------------------------- Census Tract Work -------------------------- ##
## Durham county are ones that start with 3706300
## orange county (Chapel hill) are ones that start with 371350
## keep census tracts in durham or orange county

NC_Census_Tracts_dur_ch <- NC_Census_Tracts_all %>%
  filter(grepl('3706300|371350', census_tracts_d4 ))
## 87, too many census tracts.  use only durham

NC_Census_Tracts_dur <- NC_Census_Tracts_all %>%
  filter(grepl('3706300', census_tracts_d4)) %>%
  ## Filter out some of the less populated areas
  filter(!grepl('2100|1801|1806|1900|1604|1603|1601|
                1503|1501|2027|2028|2016|1707|1807', census_tracts_d4))
## 59 total, minus 14 manually.  46 end count

## ------------------------- Synthea data -------------------------- ##

### Tasks
## Find how many unique patients there are
## Randomly assign census tracts

## Define dataset directory location
synthea_csv <- "C:/Users/rvy3/OneDrive - CDC/CDC_Work/PHIFP/Capstone/DMI_SQ_Capstone/Data/use case 1/UseCase1.csv"

## Import data
synthea_data_raw <- fread(synthea_csv)

## Get unique patients
patid <- unique(synthea_data_raw$patid)
df_patid_ctract <- data.frame(patid)
## 101 unique patients

## Randomly assign census tracts
df_patid_ctract$ctract<-print(sample(unlist(NC_Census_Tracts_dur),101,replace=TRUE))

## Merge census tracts back into dataset
synthea_ctracts <- synthea_data_raw |> 
  left_join(df_patid_ctract, by = "patid") |> 
  rename("GEOID" = "ctract")
## 505 obs, 365 var, good

## ------------------------- Rename programs -------------------------- ##
## For synthetic data.  
## programid; wic to program_id_1; ymca to program_id_1
## program_name; WIC to program_name_1; YMCA DPP to program_name_2
data_prognames <- synthea_ctracts |> 
  mutate(programid = case_when(programid == "wic" ~ "prog_id_1",
                             programid == "ymca" ~ "prog_id_1",
                             TRUE ~ "NA")) |> 
  mutate(program_name = case_when(program_name == "WIC" ~ "prog_name_1",
                                  program_name == "YMCA DPP" ~ "prog_name_2",
                               TRUE ~ "NA"))

## ------------------------- Add BMI -------------------------- ##
## BMI imperial formula: (lb/in^2) * 703

data_BMI_a <- data_prognames |> 
  mutate(BMI1 = ((WT1)/(HT1^2))*703,
         BMI2 = ((WT2)/(HT2^2))*703,
         BMI3 = ((WT3)/(HT3^2))*703) |> 
  ## Choose first BMI that is not NA
  rowwise() |> 
  mutate(BMI_list = list(first(na.omit(c(BMI1,BMI2,BMI3))))) |> 
  mutate(BMI = BMI_list[1]) |> 
  select(-BMI_list)

### Define obesity categories

data_BMI_b <- data_BMI_a |> 
  mutate(BMI_cat = case_when(BMI < 18.5 ~ "1 - Underweight",
                             BMI < 25 ~ "2 - Healthy Weight",
                             BMI < 30 ~ "3 - Overweight",
                             BMI < 35 ~ "4 - Overweight Class I",
                             BMI < 40 ~ "5 - Overweight Class II",
                             BMI >= 40 ~ "6 - Overweight Class III",
                             TRUE ~ "NA"))

# typeof(data_BMI_b$BMI)
# typeof(data_BMI_b$BMI_cat)
## Good

###### Add age categorization

data_agecat_a <- data_BMI_b |> 
  mutate(birth_date_yr = str_sub(birth_date, start=-4))


data_agecat_b <- data_agecat_a |> 
  mutate(birth_date_yr = as.numeric(str_sub(birth_date, start=-4))) |> 
  mutate(age = year-birth_date_yr) |> 
  mutate(age_cat = case_when(age < 20 ~ "<= 19",
                             age < 30 ~ "20-29",
                             age < 40 ~ "30-39",
                             age < 50 ~ "40-49",
                             age < 60 ~ "50-59",
                             age >= 60 ~ ">= 60",
                             TRUE ~ "NA")) |> 
  mutate(age_cat_census = case_when(age <18 ~ "1 - <18",
                                    age <65 ~ "2 - 18-64",
                                    age >= 65 ~ "3 - >=65",
                                    TRUE ~ "NA"))



###### Add race categorization
####### Add race information
## Race information - from codebook
# RACE_TYPE	01	American Indian or Alaska Native
# RACE_TYPE	02	Asian
# RACE_TYPE	03	Black or African American
# RACE_TYPE	04	Native Hawaiian or Other Pacific Islander
# RACE_TYPE	05	White
# RACE_TYPE	06	Multiple race
# RACE_TYPE	07	Refuse to answer
# RACE_TYPE	NI	No information
# RACE_TYPE	UN	Unknown
# RACE_TYPE	OT	Other

data_racecat <- data_agecat_b |> 
  mutate(race_cat = case_when(hispanic == "Y" ~ "0 - Hispanic",
                              race == 1 ~ "1 - American Indian or Alaska Native alone",
                             race == 2 ~ "2 - Asian alone",
                             race == 3 ~ "3 - Black or African American alone",
                             race == 4 ~ "4 - Native Hawaiian or other Pacific Islander alone",
                             race == 5 ~ "5 - White alone",
                             race == 6 ~ "6 - Multiple Race",
                             race == 7 ~ "7 - Refuse to answer",
                             TRUE ~ "UN - Unknown")) 



## ------------------------- Save prepped data -------------------------- ##
# write.csv(data_racecat, file = "C:/Users/rvy3/OneDrive - CDC/CDC_Work/PHIFP/Capstone/DMI_SQ_Capstone/Data/use case 1/UseCase1_prepped.csv", row.names = FALSE)


