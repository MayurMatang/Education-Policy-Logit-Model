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
df <- read_csv(here("Output/df_clean.csv")) |> 
  select(-1)          # remove auto generated first column 

######################  Clean Data ##################### 

df_filtered <- df |> 
  filter(State == "24") |>     # filter for Gujarat state
  filter(!Enrol_basic_course %in% c("graduate","diploma/certificate course (graduation and above)", "post graduate and above", "other non-formal"))   # filter for school students only

# Change class
df_filtered <- df_filtered %>%                  
  mutate(PID = as.character(PID),                      #change to character
         Social_group = as_factor(Social_group),       #change to factors
         Gender = as_factor(Gender),
         Religion = as_factor(Religion),
         Religion_new = as_factor(Religion_new),
         Residance_insttn_distance = as_factor(Residance_insttn_distance),
         Nature_institution = as_factor(Nature_institution),
         Institution_type = as_factor(Institution_type),
         Institution_type_new = as_factor(Institution_type_new),
         Medium_instruction = as_factor(Medium_instruction),
         Pvt_insttn_reason1 = as_factor(Pvt_insttn_reason1),
         Pvt_insttn_reason2 = as_factor(Pvt_insttn_reason2),
         Enrol_basic_course = as_factor(Enrol_basic_course),
         Sector = as_factor(Sector),
         Taking_pvt_coaching = as_factor(Taking_pvt_coaching),
         Age_group = as_factor(Age_group),
         Total_expenditure_group = as_factor(Total_expenditure_group),
         Household_size = as.numeric(Household_size),            #change to numeric
         Total_expenditure_amt = as.numeric(Total_expenditure_amt),
         Age = as.numeric(Age)
  )

  # Relevel factors
df_filtered$Medium_instruction = factor(df_filtered$Medium_instruction,levels = c("English","Gujarati","Hindi", "Kannada", "Urdu", "Marathi", "Oriya", "Dogri"))
df_filtered$Religion_new = factor(df_filtered$Religion_new,levels = c("Majority","Minority"))
df_filtered$Social_group = factor(df_filtered$Social_group,levels = c("Others","Scheduled Tribes","Scheduled Castes", "Other Backward Classes"))
df_filtered$Residance_insttn_distance = factor(df_filtered$Residance_insttn_distance,levels = c("<1 km","1 km  d < 2 kms","2 km  d < 3 kms", "3 kms  d < 5 kms", ">= 5 kms"))
df_filtered$Sector = factor(df_filtered$Sector,levels = c("urban", "rural"))
df_filtered$Gender = factor(df_filtered$Gender,levels = c("Male", "Female"))
df_filtered$Taking_pvt_coaching = factor(df_filtered$Taking_pvt_coaching,levels = c("No", "Yes"))
df_filtered$Total_expenditure_group = factor(df_filtered$Total_expenditure_group,levels = c("< 1000", "1000-2999", "3000-9999", "10000-19999", ">= 20000"))
df_filtered$Age_group = factor(df_filtered$Age_group,levels = c("3-5 yrs", "6-10 yrs", "11-15 yrs", "16-20 yrs", "above 20 yrs"))
df_filtered$Enrol_basic_course = factor(df_filtered$Enrol_basic_course,levels = c("below primary (nursery/ Kindergarten-preparatory levels)","primary (class I to V)", "upper primary/middle","diploma /certificate course (upto secondary)", "secondary","diploma /certificate course (higher secondary)", "higher secondary"))
df_filtered$Religion = factor(df_filtered$Religion,levels = c("Hinduism", "Islam","Jainism","Christianity", "Sikhism","Others"))
df_filtered$Institution_type_new = factor(df_filtered$Institution_type_new,levels = c("Government", "Private Aided","Private Unaided"))
  
colSums(is.na(df_filtered))   #check NA in the dataframe


############### Demographics of Students (unweighted) ##############

# Demographics table 1
demo_unweight_1 <- df_filtered |> 
  tbl_summary(missing = "no",
              include = c(Gender, Sector, Religion, Social_group, Household_size,Age, Age_group)) |> 
  bold_labels() |> 
  modify_header(label = "**Variable**") |> 
  modify_caption("**Figure 1. Demographics of sample school students in Gujarat (unweighted)**") |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/1. demo_unweight_1.html")

# Demographics table 2
demo_unweight_2 <- df_filtered |> 
  tbl_summary(missing = "no",
              include = c(Institution_type, Enrol_basic_course, Medium_instruction, Residance_insttn_distance, Taking_pvt_coaching, Total_expenditure_group, Total_expenditure_amt),
              type = list(Taking_pvt_coaching ~ "categorical")) |> 
  bold_labels() |> 
  modify_header(label = "**Variable**") |> 
  modify_caption("**Figure 2. Demographics of sample school students in Gujarat (unweighted)**") |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/2. demo_unweight_2.html")  


########### Demographics of Students (weighted) ##########

# Demographics table 1
demo_weight_1 <-
  svydesign(ids = ~FSU, strata = ~Stratum, weights = ~MULT_Combined, data = df_filtered) |> 
  tbl_svysummary(missing = "no",
                 include = c(Gender, Sector, Religion, Social_group, Household_size,Age, Age_group)) |>
  bold_labels() |> 
  modify_header(label = "**Variable**") |> 
  modify_caption("**Figure 3. Demographics of school students in Gujarat (weighted)**") |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/3. demo_weight_1.html")

# Demographics table 2
demo_weight_2 <-
  svydesign(ids = ~FSU, strata = ~Stratum, weights = ~MULT_Combined, data = df_filtered) |> 
  tbl_svysummary(missing = "no",
                 include = c(Institution_type, Enrol_basic_course, Medium_instruction, Residance_insttn_distance, Taking_pvt_coaching, Total_expenditure_group, Total_expenditure_amt),
                 type = list(Taking_pvt_coaching ~ "categorical")) |>
  bold_labels() |> 
  modify_header(label = "**Variable**") |> 
  modify_caption("**Figure 4. Demographics of school students in Gujarat (weighted)**") |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/4. demo_weight_2.html")


########### Crosstabs of outcome variable with other variables (weighted) ##########

# crosstab 1
crosstab_weight_1 <-
  svydesign(ids = ~FSU, strata = ~Stratum, weights = ~MULT_Combined, data = df_filtered) |> 
  tbl_svysummary(missing = "no", by = Institution_type_new,
                 include = c(Gender, Sector, Religion, Social_group, Age_group), statistic = list(all_categorical() ~ "{p}")) |>
  bold_labels() |> 
  modify_header(label = "**Variable**", stat_1 = '**Government<br>N = {style_number(n)}**', stat_2 = '**Private Aided<br>N = {style_number(n)}**', stat_3 = '**Private Unaided<br>N = {style_number(n)}**') |> 
  modify_caption("**Figure 5. Proportion of students across school types in Gujarat (weighted)**") |> 
  modify_spanning_header(all_stat_cols() ~ "**School Type**") |> 
  modify_footnote(all_stat_cols() ~ NA) |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/5. crosstab_weight_1.html")

# crosstab 2
crosstab_weight_2 <-
  svydesign(ids = ~FSU, strata = ~Stratum, weights = ~MULT_Combined, data = df_filtered) |> 
  tbl_svysummary(missing = "no", by = Institution_type_new,
                 include = c(Age, Household_size, Total_expenditure_amt)) |>
  bold_labels() |> 
  modify_header(label = "**Variable**", stat_1 = '**Government<br>N = {style_number(n)}**', stat_2 = '**Private Aided<br>N = {style_number(n)}**', stat_3 = '**Private Unaided<br>N = {style_number(n)}**') |> 
  modify_caption("**Figure 6. Median & IQR across school types in Gujarat (weighted)**") |> 
  modify_spanning_header(all_stat_cols() ~ "**School Type**") |> 
  modify_footnote(all_stat_cols() ~ NA) |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/6. crosstab_weight_2.html")

# crosstab 3
crosstab_weight_3 <-
  svydesign(ids = ~FSU, strata = ~Stratum, weights = ~MULT_Combined, data = df_filtered) |> 
  tbl_svysummary(missing = "no", by = Institution_type_new,
                 statistic = list(all_categorical() ~ "{p}"),
                 type = list(Taking_pvt_coaching ~ "categorical"),
                 include = c(Enrol_basic_course, Medium_instruction, Residance_insttn_distance, Taking_pvt_coaching, Total_expenditure_group)) |>
  bold_labels() |> 
  modify_header(label = "**Variable**", stat_1 = '**Government<br>N = {style_number(n)}**', stat_2 = '**Private Aided<br>N = {style_number(n)}**', stat_3 = '**Private Unaided<br>N = {style_number(n)}**') |> 
  modify_caption("**Figure 7. Proportion of students across school types in Gujarat (weighted)**") |> 
  modify_spanning_header(all_stat_cols() ~ "**School Type**") |> 
  modify_footnote(all_stat_cols() ~ NA) |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/7. crosstab_weight_3.html")

# Crosstab 4 (split by govt & private)

crosstab_weight_4 <-
  svydesign(ids = ~FSU, strata = ~Stratum, weights = ~MULT_Combined, data = df_filtered) |> 
  tbl_svysummary(missing = "no", by = Pvt_school_adm,
                 include = c(Gender, Sector, Religion, Social_group, Age_group), statistic = list(all_categorical() ~ "{p}")) |>
  bold_labels() |> 
  modify_header(label = "**Variable**", stat_1 = '**Government<br>N = {style_number(n)}**', stat_2 = '**All Private<br>N = {style_number(n)}**') |> 
  modify_caption("**Figure 8. Proportion of students across govt & private schools in Gujarat (weighted)**") |> 
  modify_spanning_header(all_stat_cols() ~ "**School Type**") |> 
  modify_footnote(all_stat_cols() ~ NA) |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/8. crosstab_weight_4.html")

# Crosstab 5 (split by govt & private)

crosstab_weight_5 <-
  svydesign(ids = ~FSU, strata = ~Stratum, weights = ~MULT_Combined, data = df_filtered) |> 
  tbl_svysummary(missing = "no", by = Pvt_school_adm,
                 include = c(Age, Household_size, Total_expenditure_amt)) |>
  bold_labels() |> 
  modify_header(label = "**Variable**", stat_1 = '**Government<br>N = {style_number(n)}**', stat_2 = '**All Private<br>N = {style_number(n)}**') |> 
  modify_caption("**Figure 9. Median & IQR across school types in Gujarat (weighted)**") |> 
  modify_spanning_header(all_stat_cols() ~ "**School Type**") |> 
  modify_footnote(all_stat_cols() ~ NA) |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/9. crosstab_weight_5.html")

# Crosstab 6 (split by govt & private)
crosstab_weight_6 <-
  svydesign(ids = ~FSU, strata = ~Stratum, weights = ~MULT_Combined, data = df_filtered) |> 
  tbl_svysummary(missing = "no", by = Pvt_school_adm,
                 statistic = list(all_categorical() ~ "{p}"),
                 type = list(Taking_pvt_coaching ~ "categorical"),
                 include = c(Enrol_basic_course, Medium_instruction, Residance_insttn_distance, Taking_pvt_coaching, Total_expenditure_group)) |>
  bold_labels() |> 
  modify_header(label = "**Variable**", stat_1 = '**Government<br>N = {style_number(n)}**', stat_2 = '**All Private<br>N = {style_number(n)}**') |> 
  modify_caption("**Figure 10. Proportion of students across school types in Gujarat (weighted)**") |> 
  modify_spanning_header(all_stat_cols() ~ "**School Type**") |> 
  modify_footnote(all_stat_cols() ~ NA) |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gt::gtsave(filename = "Output_tables/10. crosstab_weight_6.html")

####################### Hypo 1: Perception of private schools ##################

Reason_df <- df_filtered |>                      #Reason for admission in private school
  filter(Institution_type == c("private aided", "private unaided")) 

nsso_reason_design <- svydesign(
  ids = ~FSU, 
  strata = ~Stratum, 
  weights = ~MULT_Combined, 
  #nest = T,
  #check.strata = TRUE,
  #fpc = NULL, 
  data = Reason_df)

# Reasons for private school admission

reason_table <- 
  svytable(~Pvt_insttn_reason1, design = nsso_reason_design) |> 
  as.data.frame() |>
  rename(N = Freq, Reason = Pvt_insttn_reason1) |> 
  mutate(N = round(N), Percent = round(100*N/sum(N))) |> 
  arrange(desc(Percent)) |> 
  gt() |> 
  tab_header(title = md("**Reasons for admissions to Private Schools (weighted)**")) |> 
  cols_align(align = "left",columns = Reason) |> 
  cols_align(align = "center",columns = c(N, Percent)) |>
  fmt_number(columns = N, decimals = 0) |> 
  cols_label(Reason = md("**Reason**"),N = md("**N**"),Percent = md("**Percent**")) |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "Data is filtered for school students in Gujarat state. N = 1,793,120") |> 
  tab_source_note(source_note = "Question: Reason for attending current private institution.") |> 
gtsave(file = "Output_tables/11. reason_table.html")

# Reasons for admission into private aided school
reason_aided <- svytable(~Pvt_insttn_reason1+Institution_type, design = nsso_reason_design) |> 
  as.data.frame() |> 
  group_by(Institution_type) |> 
  mutate(Freq = round(Freq), 
         Percent = round(100*Freq/sum(Freq)), 
         Institution_type = toupper(Institution_type)) |> 
  rename(N = Freq, Reason = Pvt_insttn_reason1) |>
  ungroup() |> 
  select(Reason, Institution_type, N, Percent) |> 
  filter(Institution_type == "PRIVATE AIDED") |> 
  arrange(desc(Percent)) |>
  gt() |> 
  cols_hide(columns = c(Institution_type)) |> 
  tab_header(title = md("**Reasons for admission to Private Aided Schools (weighted)**")) |> 
  fmt_number(columns = N, decimals = 0) |> 
  cols_label(Reason = md("**Reason**"),N = md("**N**"),Percent = md("**Percent**")) |> 
  cols_align(align = "left",columns = c(Reason)) |>
  cols_align(align = "center",columns = c(N, Percent)) |>
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "Data is filtered for school students in Gujarat state. N = 964,273") |> 
  tab_source_note(source_note = "Question: Reason for attending current private institution.") |> 
gtsave(file = "Output_tables/12. reason_aided.html")

# Reasons for admission into private unaided school
reason_unaided <- svytable(~Pvt_insttn_reason1+Institution_type, design = nsso_reason_design) |> 
  as.data.frame() |> 
  group_by(Institution_type) |> 
  mutate(Freq = round(Freq), 
         Percent = round(100*Freq/sum(Freq)), 
         Institution_type = toupper(Institution_type)) |> 
  rename(N = Freq, Reason = Pvt_insttn_reason1) |>
  ungroup() |> 
  select(Reason, Institution_type, N, Percent) |> 
  filter(Institution_type == "PRIVATE UNAIDED") |> 
  arrange(desc(Percent)) |>
  gt() |> 
  cols_hide(columns = c(Institution_type)) |> 
  tab_header(title = md("**Reasons for admission to Private Unaided Schools (weighted)**")) |> 
  cols_align(align = "left",columns = c(Reason)) |>
  cols_align(align = "center",columns = c(N, Percent)) |>
  fmt_number(columns = N, decimals = 0) |> 
  cols_label(Reason = md("**Reason**"),N = md("**N**"),Percent = md("**Percent**")) |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "Data is filtered for school students in Gujarat state. N = 828,847") |> 
  tab_source_note(source_note = "Question: Reason for attending current private institution.") |> 
  gtsave(file = "Output_tables/13. reason_unaided.html")

####################### Logistic Model ######################## 

df_filtered <- df_filtered |>  # change independent variables to factors
  mutate(Medium_instruction = as.factor(Medium_instruction),  
         Religion = as.factor(Religion),
         Social_group = as.factor(Social_group),
         Residance_insttn_distance = as.factor(Residance_insttn_distance),
         Sector = as.factor(Sector),
         Gender = as.factor(Gender),
         Taking_pvt_coaching = as.factor(Taking_pvt_coaching)
         )

df_model <- df_filtered |> 
  select(Pvt_school_adm,Institution_type_new, Medium_instruction, Taking_pvt_coaching,MULT_Combined, Enrol_basic_course, Total_expenditure_group, Household_size, Religion_new, Social_group, Gender, Residance_insttn_distance, Age_group ,Sector, FSU, Stratum) |> 
  na.omit()         # Remove NAs

# save design object
nsso_design <- svydesign(ids= ~FSU, strata= ~Stratum, weights= ~MULT_Combined,data = df_model)                               
#Model                              
logit_model <- svyglm(Pvt_school_adm ~ Residance_insttn_distance + Social_group + Religion_new + Gender + Age_group + Sector + Taking_pvt_coaching + Household_size, family = quasibinomial, design=nsso_design, na.action = na.omit)

summary(logit_model)   

log_odds_table <- tbl_regression(logit_model) |> # coefficients in log odds form
  modify_column_unhide(column = std.error) |> 
  bold_labels() |> 
  modify_caption("**Logit Model: Log Odds Ratio**") |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gtsave(file = "Output_tables/14. model_logodds.html")

odds_table <- tbl_regression(logit_model, exponentiate = TRUE) |>   # Odds ratio table
  modify_column_unhide(column = std.error) |> 
  bold_labels() |> 
  modify_caption("**Logit Model: Odds Ratio**") |> 
  as_gt() |> 
  tab_source_note(source_note = "Source: NSSO Education Consumption 2018.") |> 
  tab_source_note(source_note = "NSSO data is filtered for school students in Gujarat state.") |> 
  gtsave(file = "Output_tables/15. model_oddsratio.html")

################## Model Fit #################

nullmodel <- svyglm(Pvt_school_adm ~ 1, design=nsso_design, family = quasibinomial)
(McFadden_R2 <- 1 - (logLik(nullmodel) - logLik(logit_model))/logLik(nullmodel)) 

(chi_sq <- -2 * (logLik(nullmodel) - logLik(logit_model)))  # Calculate chi-squared value

chi_sq_df <- length(coef(logit_model)) - length(coef(nullmodel)) # Calculate p-value
(p_val <- 1 - pchisq(chi_sq, chi_sq_df))

#A p-value of zero provides strong evidence against the null hypothesis. It suggests that the fitted model provides a significantly better fit to the data than the null model. 
