########## Load Libraries ##########  
library(survey)
library(here)
library(haven)
library(tidyverse)
library(gt)
library(scales)
library(labelled)

################ Import Data ################
df_merged <- read_sav(here("prepped_data/df_merged.sav"))

################ Clean Data ################

df_merged[df_merged == ""] <- NA # make blank cells NA

df_merged <- df_merged[,c(1:23, 26:29, 32:35, 51,53:64)]  #remove irrelevant columns
#df_merged <- df_merged[,c(2:24, 27:30, 33:36, 52,54:65)]  #remove irrelevant columns

df_merged <- df_merged %>%                  
  mutate(PID = as.character(PID),                      #change to character
         Social_group = as_factor(Social_group),       #change to factors
         Gender = as_factor(Gender),
         Religion = as_factor(Religion),
         Residance_insttn_distance = as_factor(Residance_insttn_distance),
         Nature_institution = as_factor(Nature_institution),
         Institution_type = as_factor(Institution_type),
         Medium_instruction = as_factor(Medium_instruction),
         Pvt_insttn_reason1 = as_factor(Pvt_insttn_reason1),
         Pvt_insttn_reason2 = as_factor(Pvt_insttn_reason2),
         Enrol_basic_course = as_factor(Enrol_basic_course),
         Course_attending = as_factor(Course_attending),
         Type_course = as_factor(Type_course),
         Sector = as_factor(Sector),
         Taking_pvt_coaching = as_factor(Taking_pvt_coaching),
         Household_size = as.numeric(Household_size),        #change to numeric
         Total_expenditure_amt = as.numeric(Total_expenditure_amt),
         Age = as.numeric(Age)
         )

summary(df_merged)

############### Create new variables #############

# Create Age group
df_merged <- df_merged |> 
  mutate(Age_group =
           if_else(Age < 6, "3-5 yrs",
            if_else(Age >=6 & Age <=10, "6-10 yrs",
            if_else(Age >=11 & Age <=15, "11-15 yrs",
            if_else(Age >=16 & Age <=20, "16-20 yrs", "above 20 yrs"
            )))))

# create total expenditure amount group
df_merged <- df_merged |>           
  mutate(Total_expenditure_group = 
    if_else(Total_expenditure_amt < 1000, "< 1000",
    if_else(Total_expenditure_amt >= 1000 & Total_expenditure_amt < 3000, "1000-2999",
    if_else(Total_expenditure_amt >= 3000 & Total_expenditure_amt < 10000, "3000-9999",
    if_else(Total_expenditure_amt >= 10000 & Total_expenditure_amt < 20000, "10000-19999",">= 20000")))))

# Institution_type_new
df_merged$Institution_type_new[df_merged$Institution_type == "private aided"] <- "Private Aided"
df_merged$Institution_type_new[df_merged$Institution_type == "Government"] <- "Government"
df_merged$Institution_type_new[df_merged$Institution_type == "private unaided"] <- "Private Unaided"
df_merged$Institution_type_new[df_merged$Institution_type == "not known"] <- NA

# Private school admission (Dependent Variable)
df_merged <- df_merged |> 
  mutate(Pvt_school_adm = 
           if_else(Institution_type_new == "Government", 0, 1))  #Outcome variable

# Taking pvt coaching
df_merged <- df_merged |> 
  mutate(Taking_pvt_coaching = if_else(Taking_pvt_coaching == 1, "Yes", "No"))

# Religion to majority/minority 
df_merged$Religion_new[df_merged$Religion == "Jainism"] <- "Minority"
df_merged$Religion_new[df_merged$Religion == "Christianity"] <- "Minority"
df_merged$Religion_new[df_merged$Religion == "Sikhism"] <- "Minority"
df_merged$Religion_new[df_merged$Religion == "Others"] <- "Minority"
df_merged$Religion_new[df_merged$Religion == "Hinduism"] <- "Majority"
df_merged$Religion_new[df_merged$Religion == "Islam"] <- "Minority"

# Clean reasons for private school admission
df_merged$Pvt_insttn_reason1 <- str_to_sentence(df_merged$Pvt_insttn_reason1)
df_merged$Pvt_insttn_reason1[df_merged$Pvt_insttn_reason1 == "Quality of education in nearby government institution is per"] <- "Quality of education in nearby government school is unsatisfactory"
df_merged$Pvt_insttn_reason1[df_merged$Pvt_insttn_reason1 == "Social reasons (like children of colleagues/neighbours/relat"] <- "Social reasons (peer pressure)"
df_merged$Pvt_insttn_reason1[df_merged$Pvt_insttn_reason1 == "Private coaching not required to supplement the school educa"] <- "Private coaching not required to supplement school education"

# HOusehold size
df_merged <- df_merged |>           
  mutate(Household_size_cat = 
    if_else(Household_size <= 3, "<= 3",
    if_else(Household_size >= 4 & Household_size <= 5, "4-5", ">= 6"
    )))


# Save Cleaned file
write.csv(df_merged, "prepped_data/df_clean.csv")

