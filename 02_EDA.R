## Author: Patrick Zhang
## Purpose: Use Case 1 - EDA
## Start Date: 3/14/23

## ------------------------ Background --------------------------- ##
# Preliminary EDA on synthetic data

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

##### Set Variables Based on Local Directories #####

## Set wd
# setwd("C:/Users/rvy3/OneDrive - CDC/CDC_Work/PHIFP/Capstone/DMI_SQ_Capstone/Data/use case 1")
## Define dataset directory location
source_data <- "C:/Users/rvy3/OneDrive - CDC/CDC_Work/PHIFP/Capstone/DMI_SQ_Capstone/Data/use case 1/UseCase1_prepped.csv"

## Import data
synthea_data_ctracts_raw <- fread(source_data) |> 
  mutate("GEOID" = as.character(GEOID))


##### Investigate Data #####
## ------------------------- GIS data -------------------------- ##

### Make durham base map
durham_gis_base <- get_acs(
  state = "NC",
  county = "Durham",
  geography = "tract",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2020
)

durham_gis_base %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf() +
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar) 

## Merge location data with synthea data
## Left join causes an error for some reason, must use right for GIS

# synthea_GIS <- synthea_data_ctracts_raw |> 
#   left_join(durham_gis_base, by = "GEOID")

synthea_GIS <- durham_gis_base |> 
  right_join(synthea_data_ctracts_raw, by = "GEOID", multiple= "all")

## PZ - Investigate later. missing some census tracts?  37063002027; 37063002028

## Try plotting
synthea_GIS %>%
  ggplot(aes(fill = race)) + 
  geom_sf() +
  theme_void() + 
  scale_fill_viridis_c(labels = scales::dollar) 

## Okay.


## ------------------------- Workflow for making maps -------------------------- ##

## Use synthea_data_ctracts_raw dataset as starting point.
##  starting fields: select(GEOID, patid,
## Summarize to relevant information
## merge data into durham_gis_base
## Make the map

#### Basic map
## Among all program participants from 2015-2020, Count number of unique participants by census tract
patid_count_by_ctract  <- synthea_data_ctracts_raw |> 
  select(GEOID, patid, programid, completion_date1, enrollment_date1) |> 
  distinct() |> 
  group_by(GEOID) |> 
  summarise(count_patid = n())
## Good

patid_count_by_ctract_b <- durham_gis_base |>
  left_join(patid_count_by_ctract, by = "GEOID")


patid_count_by_ctract_b %>%
  ggplot(aes(fill = factor(count_patid))) + 
  geom_sf() +
  theme_void() + 
  scale_fill_viridis_d("Participant\nCount") +
  labs(
      title = "Unique Program Participants in 2015-2020\nby Census Tract"
  )


# ### What columns should we keep?
# 
# ## For Use Case 1, keep the demographics variables (var 1-21).  
# ##      Also keep variables year_admit, year_session, year_measure, year_result
# 
# vec_colnames <- colnames(synthea_data_ctracts_raw)
# vec_keepcols <- vec_colnames[c(1:21, match(c("year_admit", "year_session", "year_measure", "year_result", 
#                                              "HT1", "HT2", "HT3", "HT4", "HT5", "HT6", "HT7", "HT8",
#                                              "WT1", "WT2", "WT3", "WT4", "WT5", "WT6", "WT7", "WT8",
#                                              "measure_date1", "measure_date2", "measure_date3", "measure_date4", "measure_date5", "measure_date6", "measure_date7", "measure_date8",
#                                              "encounterid1", "encounterid2", "encounterid3", "encounterid4", "encounterid5", "encounterid6", "encounterid7", "encounterid8"
#                                              
# ),vec_colnames))]
# 
# df_SQ1 <- synthea_data_ctracts_raw %>%
#   select(all_of(vec_keepcols))
# # Good
# 
# ### Calculate BMI
# 
# ### Crosstab check: See if the aims are same for each programs
# 
# table(df_SQ1$program_name, df_SQ1$aim_nutrition)
# table(df_SQ1$program_name, df_SQ1$aim_activity)
# table(df_SQ1$program_name, df_SQ1$aim_weight)
# 
# table(df_SQ1$program_name, df_SQ1$prescribed_total_dose)
# 
# ## Define table 1
# df_test1 <- df_SQ1 %>%
#   select(program_name, sdoh_category1)
# tbl_1 <- table(df_test1)
# 
# tbl_1
# 
# ## Define table 2
# 
# df_test2 <- df_SQ1 %>%
#   filter(!is.na(year_SDOH ))
# 
# plot1 <- ggplot(df_SQ1, aes(x=year_enroll)) + geom_bar() + labs(title="Enrollments by Year")  # Y axis derived from counts of X item
# print(plot1)



## Good.  For this sample dataset, all aims and prescribed dose are same for each program
