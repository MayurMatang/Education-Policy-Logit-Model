#####################  Load Libraries #####################   
library(survey)
library(here)
library(haven)
library(tidyverse)
library(gt)
library(gtsummary)
library(labelled)
library(margins)
library(jtools)

######################  Load Data ##################### 
df <- read_csv(here("prepped_data/df_clean.csv")) |> 
  select(-1)          # remove auto generated first column 

######################  Clean Data ##################### 

df_filtered <- df |> 
  filter(State == "24") |>     # filter for Gujarat state
  filter(!Enrol_basic_course %in% c("graduate","diploma/certificate course (graduation and above)", "post graduate and above", "other non-formal", "below primary (nursery/ Kindergarten-preparatory levels)"))

############### Recode and Relevel Variables ###############
df_model <- df_filtered |> 
  select(Institution_type_new, Taking_pvt_coaching,MULT_Combined, Household_size, Religion_new, Social_group, Gender, Residance_insttn_distance, Age_group ,Sector, FSU, Stratum, Sub_stratum) |> 
  mutate(Pvt_school_adm = if_else(Institution_type_new == "Private Unaided", 1, 0))
  
df_model$Social_group = factor(df_model$Social_group,levels = c("Others","Scheduled Tribes","Scheduled Castes", "Other Backward Classes"))
df_model$Residance_insttn_distance = factor(df_model$Residance_insttn_distance,levels = c("<1 km","1 km  d < 2 kms","2 km  d < 3 kms", "3 kms  d < 5 kms", ">= 5 kms"))
df_model$Sector = factor(df_model$Sector,levels = c("urban", "rural"))
df_model$Gender = factor(df_model$Gender,levels = c("Male", "Female"))
df_model$Taking_pvt_coaching = factor(df_model$Taking_pvt_coaching,levels = c("No", "Yes"))
df_model$Age_group = factor(df_model$Age_group,levels = c("3-5 yrs", "6-10 yrs", "11-15 yrs", "16-20 yrs", "above 20 yrs"))
  
# save design object
nsso_design <- svydesign(ids= ~FSU, strata= ~Stratum, weights= ~MULT_Combined,data = df_model)                               
############### Testing new -- save design object #############
nsso_design <- svydesign(ids= ~FSU, strata= ~interaction(Stratum,Sub_stratum), weights= ~MULT_Combined,data = df_model)               

#Model                              
logit_model <- svyglm(Pvt_school_adm ~ Age_group + Gender + Social_group + Religion_new + Household_size + Residance_insttn_distance + Sector + Taking_pvt_coaching, family = quasibinomial, design=nsso_design, na.action = na.omit)

summary(logit_model)   

log_odds_table <- tbl_regression(logit_model) |> # coefficients in log odds form
  modify_column_unhide(column = std.error) |> 
  bold_labels() |> 
  modify_caption("**Logit Model: Log Odds Ratio**") |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gtsave(file = "Output_tables/14. model_logodds_(uniaded_1_else_o).html")

odds_table <- tbl_regression(logit_model, exponentiate = TRUE) |>   # Odds ratio table
  modify_column_unhide(column = std.error) |> 
  bold_labels() |> 
  modify_caption("**Logit Model: Odds Ratio**") |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gtsave(file = "Output_tables/15. model_oddsratio_(uniaded_1_else_o).html")

################## Model Fit #################

nullmodel <- svyglm(Pvt_school_adm ~ 1, design=nsso_design, family = quasibinomial)
(McFadden_R2 <- 1 - (logLik(nullmodel) - logLik(logit_model))/logLik(nullmodel)) 

(chi_sq <- -2 * (logLik(nullmodel) - logLik(logit_model)))  # Calculate chi-squared value

chi_sq_df <- length(coef(logit_model)) - length(coef(nullmodel)) # Calculate p-value
(p_val <- 1 - pchisq(chi_sq, chi_sq_df))

#A p-value of zero provides strong evidence against the null hypothesis. It suggests that the fitted model provides a significantly better fit to the data than the null model. 

  
  
