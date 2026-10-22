########## Load Libraries ##########  
library(survey)
library(here)
library(haven)
library(tidyverse)
library(gt)
library(scales)
library(labelled)

########### Load Data ##########
rawdata3 <- read_sav(here("Rawdata/R75252L02 (Block 3)-household characteristics.sav"))
rawdata4 <- read_sav(here("Rawdata/R75252L04 (Block-4)-Demographic and other particulars of household members.sav"))
rawdata5 <- read_sav("Rawdata/R75252L05 (Block-5)-Dducation particulars on basic course of the persons of age 3 to 35 years who are currently attending education.sav")
rawdata6 <- read_sav("Rawdata/R75252L06 (Block 6)-Particulars of expenditure (Rs.sav")

########### Merge data ##########
rawdata5_clean <- rawdata5 |> 
  mutate(PID = paste0(HHID, Per_serialno))  #create a unique person identifier

rawdata6_clean <- rawdata6 |> 
  mutate(PID = paste0(HHID, Per_serialno)) |>  #create a unique person identifier
  select(PID, Total_expenditure_amt)

# combine rawdata5_clean & rawdata6_clean
rawdata_5_6 <- left_join(rawdata5_clean, rawdata6_clean, by = "PID")

# Combine rawdata5_clean & rawdata3_clean to get religion, HH size, social group
rawdata3_clean <- rawdata3 |> 
  select(HHID, Household_size, Religion, Social_group)

rawdata_5_6_3 <- left_join(rawdata_5_6, rawdata3_clean, by = "HHID")

# Combine rawdata_5_6_3 to rawdata4 to get gender
rawdata4_clean <- rawdata4 |> 
  mutate(PID = paste0(HHID, Per_serialno)) |> #create a unique person identifier
  select(PID, Gender)

rawdata_all <- left_join(rawdata_5_6_3, rawdata4_clean, by = "PID")

# Save the merged file for further cleaning and analysis
write.csv(rawdata_all, "prepped_data/df_merged.csv")
write_sav(rawdata_all, "prepped_data/df_merged.sav")
