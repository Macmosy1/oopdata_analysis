# Load libraries
library(MEPS)
library(tidyverse)
library(haven)
library(devtools)
library(dplyr)
library(gtsummary)
library(gt)
library(patchwork)
library(survey)
library(ggplot2)
library(viridis)
library(broom)
library(lmtest)
library(sandwich)
library(pROC)
library(pagedown)
library(sjPlot)
library(kableExtra)

meps <- read_MEPS("h243.dat")

janitor::clean_names(meps)



# Select specified variables, excluding ADPAIN42, DLAYDN42, AFRDDN42, DVTOFD22, ERTOFD22, IPTOFD22, OPTOFD22, RXOFD22
meps_variables <- meps %>%
  select(
    # Out-of-pocket costs
    TOTEXP22,DVTSLF22, OBDSLF22, OPDSLF22, IPDSLF22, RXSLF22,
    # Delayed or Foregone Care
    DLAYCA42, DLAYPM42, AFRDCA42, AFRDPM42,
    # Subgroup Variables
    AGE22X, SEX, RACETHX, HIDEG, POVCAT22, INSURC22, REGION22,
    # Logistic Regression Predictors
    FAMINC22, ADGENH42, CHRONIC_CONDITIONS = DIABDX_M18,
    # Economic & Health Outcomes
    ADDEBT42, ADLATEUTIL42, ADLATERENT42, POVCAT22,
    # Health Conditions
    DIABDX_M18, HIBPDX,
    # Survey variables
    VARPSU, VARSTR, PERWT22F
  )

# Create total OOP variable
meps_df <- meps_variables %>%
  mutate(
    OOP_TOTAL = DVTSLF22 + OBDSLF22 + OPDSLF22 + IPDSLF22 + RXSLF22
  )

meps_df <- meps_df %>%
  rename(
    # Expenditure Variables
    total_health_expenditure = TOTEXP22,
    dental_oop = DVTSLF22,
    physician_oop = OBDSLF22,
    outpatient_oop = OPDSLF22,
    inpatient_oop = IPDSLF22,
    rx_oop = RXSLF22,
    oop_total = OOP_TOTAL,
    
    # Care Access Variables
    delayed_care = DLAYCA42,
    delayed_pmed = DLAYPM42,
    affordability_issue = AFRDCA42,
    pmed_affordability = AFRDPM42,
    
    # Demographics
    age = AGE22X,
    gender = SEX,
    race_ethnicity = RACETHX,
    education_level = HIDEG,
    poverty_category = POVCAT22,
    insurance_coverage = INSURC22,
    region = REGION22,
    family_income = FAMINC22,
    
    # Health Outcomes
    self_rated_health = ADGENH42,
    diabetes_diagnosis = CHRONIC_CONDITIONS,
    hypertension_diagnosis = HIBPDX,
    
    # Economic Outcomes
    medical_debt = ADDEBT42,
    late_utilities = ADLATEUTIL42,
    late_rent = ADLATERENT42
  )

meps_df <- meps_df %>%
  mutate(
    gender = as.numeric(gender),
    race_ethnicity = as.numeric(race_ethnicity),
    insurance_coverage = as.numeric(insurance_coverage)
  )
# factor gender, race_ethnicity, and insurance_coverage

meps_df1 <- meps_df %>%
  filter(age >= 18) %>%
  mutate(
    gender = ifelse(gender %in% c(-15, -8, -7, -1), NA, gender),
    gender = factor(gender, 
                    levels = c(1, 2), 
                    labels = c("Male", "Female")),
    
    race_ethnicity = ifelse(race_ethnicity %in% c(-15, -8, -7, -1), NA, race_ethnicity),
    race_ethnicity = factor(race_ethnicity,
                            levels = c(1:5),
                            labels = c("Hispanic", "White", "Black", "Asian", "Other")),
    
    poverty_category = ifelse(poverty_category %in% c(-15, -8, -7, -1), NA, poverty_category),
    poverty_category = factor(poverty_category, 
                              levels = c(1:5),
                              labels = c("Negative Income/Debt", "Near Poor", "Low Income", 
                                         "Middle Income", "High Income")),
    
    insurance_coverage = ifelse(insurance_coverage %in% c(-15, -8, -7, -1), NA, insurance_coverage),
    insurance_coverage = factor(insurance_coverage,
                                levels = c(1:8),
                                labels = c("<65 Any Private Insurance", "<65 Public Only",
                                           "<65 Uninsured", "65+ Medicare Only",
                                           "65+ Medicare + Private", "65+ Medicare + Other Public",
                                           "65+ Uninsured", "65+ No Medicare but Other Coverage")),
    
    education_level = ifelse(education_level %in% c(-15, -8, -7, -1, 8), NA, education_level),
    education_level = factor(education_level,
                             levels = c(1:7),
                             labels = c("No degree", "GED", "High School Diploma",
                                        "Bachelor's Degree", "Master's Degree",
                                        "Doctorate Degree", "Other Degree")),
    
    # Corrected oop_category
    oop_category = case_when(
      oop_total == 0 ~ "No Spending",
      oop_total <= 100 ~ "Low (1-100)",
      oop_total <= 500 ~ "Moderate (101-500)",
      oop_total > 500 ~ "High (>500)"
    ),
    oop_category = factor(oop_category,
                          levels = c("No Spending", "Low (1-100)", 
                                     "Moderate (101-500)", "High (>500)")),
    
    # Age categories (correct as-is)
    age_categories = cut(age,
                         breaks = c(18, 25, 35, 45, 55, 65, Inf),
                         labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                         right = FALSE)
  )


meps_df2 <- meps_df1 %>%
  mutate(
    # Recode region
    region = ifelse(region %in% c(-1), NA, region),
    region = factor(region,
                    levels = c(1, 2, 3, 4),
                    labels = c("Northeast", "Midwest", "South", "West")),
    
    # Recode self_rated_health
    self_rated_health = ifelse(self_rated_health %in% c(-15, -1), NA, self_rated_health),
    self_rated_health = factor(self_rated_health,
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    
    # Recode medical_debt
    medical_debt = ifelse(medical_debt %in% c(-15, -1), NA, medical_debt),
    medical_debt = factor(medical_debt,
                          levels = c(1, 2),
                          labels = c("Yes", "No")),
    affordability_issue = ifelse( affordability_issue %in% c(-8, -7 -1), NA,  affordability_issue),
    affordability_issue = factor( affordability_issue,
                                  levels = c(1, 2),
                                  labels = c("Yes", "No")),
    
    # Recode late_utilities
    late_utilities = ifelse(late_utilities %in% c(-15, -1), NA, late_utilities),
    late_utilities = factor(late_utilities,
                            levels = c(1, 2, 3),
                            labels = c("Yes", "No", "Don't know")),
    
    
    # Recode late_rent
    late_rent = ifelse(late_rent %in% c(-15, -1), NA, late_rent),
    late_rent = factor(late_rent,
                       levels = c(1, 2, 3),
                       labels = c("Yes", "No", "Don't know"))
  )

meps_df3 <- meps_df2 %>%
  mutate(
    family_income_category = case_when(
      family_income < 25000 ~ "Low Income",
      family_income >= 25000 & family_income < 50000 ~ "Lower-Middle Income",
      family_income >= 50000 & family_income < 75000 ~ "Middle Income",
      family_income >= 75000 & family_income < 100000 ~ "Upper-Middle Income",
      family_income >= 100000 ~ "High Income",
      TRUE ~ NA_character_  # Handle missing values
    )
  )

meps_df3 <- meps_df3 %>%
  mutate(
    # 1. Clean hypertension_diagnosis (numeric → numeric)
    hypertension_diagnosis = ifelse(
      hypertension_diagnosis %in% c(-15, -8, -7, -1), 
      NA, 
      hypertension_diagnosis
    ),
    
    # 2. Create binary version (using numeric codes)
    hypertension_diagnosis_binary = case_when(
      hypertension_diagnosis == 1 ~ 1,  # Yes
      hypertension_diagnosis == 2 ~ 0,   # No
      TRUE ~ NA_real_                    # Preserve NA
    ),
    
    # 3. Convert to labeled factor (numeric → factor)
    hypertension_diagnosis = factor(
      hypertension_diagnosis,
      levels = c(1, 2),
      labels = c("Diagnosed Hypertension", "No Hypertension")
    )
  )

meps_df3 <- meps_df2 %>%
  mutate(
    diabetes_diagnosis= ifelse(diabetes_diagnosis %in% c(-8, -7, -1), NA, diabetes_diagnosis),
    diabetes_diagnosis = case_when(
      diabetes_diagnosis == 1 ~ "Yes",  # 1 = Yes
      diabetes_diagnosis == 2 ~ "No",         # 2 = No
      TRUE ~ NA_character_                 # Handle missing values
    )
  )
# Create the binary version for logistic regression

meps_df4 <- meps_df3 %>%
  mutate(
    hypertension_diagnosis = as.numeric(hypertension_diagnosis)
  )

meps_df4 <- meps_df4 %>% 
  mutate(
    delayed_care_binary = ifelse(delayed_care %in% c(-8, -7, -1), NA, delayed_care),
    # Convert to binary numeric (1 = Yes, 0 = No)
    delayed_care_binary = case_when(
      delayed_care_binary == 1 ~ 1,  # Yes → 1
      delayed_care_binary == 2 ~ 0,  # No → 0
      TRUE ~ NA_real_               # Keep NAs numeric
    ),
    affordability_issue_binary = case_when(
      affordability_issue == "Yes"~ 1,  # Yes → 1
      affordability_issue == "No" ~ 0,  # No → 0
      TRUE ~ NA_real_               # Keep NAs numeric
    ),
    
    diabetes_diagnosis = as.character(diabetes_diagnosis),
    diabetes_diagnosis_binary = case_when(
      diabetes_diagnosis == "Yes" ~ 1,  
      diabetes_diagnosis == "No" ~ 0,          
      TRUE ~ NA_real_                                  
    ),
    
    # Binary indicator for late utility payments
    late_utilities_binary = case_when(
      late_utilities == "Yes" ~ 1,  # Yes → 1
      late_utilities == "No" ~ 0,  # No → 0
      TRUE ~ NA_real_           # Keep NAs as numeric
    ),
    
    # Binary indicator for medical debt
    medical_debt_binary = case_when(
      medical_debt == "Yes" ~ 1,  # Yes → 1
      medical_debt == "No" ~ 0,  # No → 0
      TRUE ~ NA_real_         # Keep NAs as numeric
    ),
    delayed_care_categorical = case_when(
      delayed_care_binary == 1 ~ "Yes",  
      delayed_care_binary == 0 ~ "No",         
      TRUE ~ NA_character_                 
    ),
    hypertension_diagnosis= ifelse(hypertension_diagnosis %in% c(-8, -7, -1), NA, hypertension_diagnosis),
    hypertension_diagnosis_cat = case_when(
      hypertension_diagnosis == 1 ~ "Yes",  # 1 = Yes
      hypertension_diagnosis == 2 ~ "No",         # 2 = No
      TRUE ~ NA_character_                 # Handle missing values
    ),
    hypertension_diagnosis = as.character(hypertension_diagnosis),
    hypertension_diagnosis_binary = case_when(
      hypertension_diagnosis == 1 ~ 1,
      hypertension_diagnosis == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Define the survey design
meps_design <- svydesign(
  ids = ~VARPSU,          # Cluster identifier
  strata = ~VARSTR,      # Stratum identifier
  weights = ~PERWT22F,   # Survey weight
  data = meps_df4,      # Loaded dataframe
  nest = TRUE            # Nest PSUs within strata
)
