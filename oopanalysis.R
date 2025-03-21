
# Loading dependencies---------------------------- ----------------------
library(tidyverse)
library(scales)
library(readxl)
library(haven)
library(foreign)
library(dplyr)
library(survey)
library(broom)
library(here)
library(gtsummary)


# Read dataset and view ---------------------------------------------------
adult23 <- read_csv("C:/Users/Moses Alikali/Desktop/Out of Pocket Research/NHIS/adult23.csv")


# Cleaning data -----------------------------------------------------------
nhis_df <- adult23 %>% 
  janitor::clean_names()
clean_df <- nhis_df %>% 
  select(urbrrl,region, raceallp_a,hispallp_a, sex_a, agep_a,educp_a,
         povrattc_a, cover_a, cover65_a, phstat_a,hypev_a, hypmed_a, dibev_a, 
         dibins_a,hospongt_a, meddl12m_a, medng12m_a,rxdg12m_a, rxls12m_a, 
         paybll12m_a, payworry_a)
view(clean_df)
table(clean_df$agep_a)
 
# Rename the columns

clean_df <- clean_df %>% 
  rename(
  rural_urban = "urbrrl",
  usa_region = "region",
  race = "raceallp_a",
  race_ethnicity = "hispallp_a",
  sex = "sex_a",
  education = "educp_a",
  poverty_categories = "povrattc_a",
  insurance_coverage = "cover_a",
  `insurance_coverage65+` = "cover65_a",  # Backticks for special characters
  general_health_status = "phstat_a",
  diagnosed_bp = "hypev_a",
  taking_bp_medicatiom = "hypmed_a",
  diagnosed_diabetes = "dibev_a",
  managing_diabetes_with_insulin = "dibins_a",
  hospitalized_overnight = "hospongt_a",
  delay_care_bcos_of_cost = "meddl12m_a",
  needed_care_but_didnnot_get_due_to_cost = "medng12m_a",
  didnot_get_prescription_bcos_of_cost = "rxdg12m_a",
  took_less_medication_prescribed_to_save_money = "rxls12m_a",
  unable_to_pay_medical_bills = "paybll12m_a",
  worried_about_being_unable_to_pay_medbills = "payworry_a"
)
view(clean_df)

# Renaming the categories
# Recode rural_urban column to "Urban" and "Rural" and onvert to factor

clean_df <- clean_df %>%
  mutate(
    urban_rural = case_when(
      rural_urban %in% c(1, 2, 3) ~ "Urban",
      rural_urban == 4 ~ "Rural"
    ),
    urban_rural = factor(urban_rural, levels = c("Urban", "Rural"))
  )

clean_df <- clean_df %>% 
  mutate(usa_region= recode(usa_region,
                            "1" ="Northeast",
                            "2" = "Midwest",
                            "3" = "South",
                            "4" = "West"
                            ),
         usa_region = factor(usa_region, levels = c("Northeast", "Midwest",
                                                    "South", "West"))
         )

White only
2: Black or African American only
3: Asian only
4: American Indian or Alaska Native (AIAN) only
5: Multiple races
6: Other

Hispanic
2: Non-Hispanic White
3: Non-Hispanic Black
4: Non-Hispanic Asian
5: Non-Hispanic AIAN
6: Other
  
view(clean_df)
data <- data %>%
  mutate(category = recode(category, 
                           "A" = "Alpha", 
                           "B" = "Beta", 
                           "C" = "Gamma", 
                           "D" = "Delta"))
# Categorize age

clean_df <- clean_df %>% 
  mutate(age_cat = case_when(
    agep_a <35 ~"18-34",
    agep_a >=35& agep_a<45~"35-44",
    agep_a >=45 & agep_a <55~"45-54",
    agep_a >=55 & agep_a<65~"55-64",
    agep_a >=65 & agep_a<75~"65-74",
    agep_a >=75~"75+"
  ))
view(clean_df)

# Recode rural_urban column to "Urban" and "Rural"
clean_df <- clean_df %>%
  mutate(urban_rural = case_when(
    rural_urban %in% c(1, 2, 3) ~ "Urban",
    rural_urban == 4 ~ "Rural"
  ))

# Check the new counts
table(clean_df$urban_rural)

data <- data |>
  mutate(htn_cat = factor(
    case_when(
      mean_sbp < 120 ~ "normal",
      mean_sbp >= 120 & mean_sbp < 130 ~ "elevated",
      mean_sbp >= 130 & mean_sbp < 140 ~ "stage1",
      mean_sbp >= 140 ~ "stage2",
      is.na(mean_sbp) ~ NA_character_
    )
  ))


# Weighting ---------------------------------------------------------------

# Creating a Survey Design Object -----------------------------------------
# Define the survey design
nhissvy <- svydesign(
  ids = ~PPSU,              
  strata = ~PSTRAT,        
  weights = ~WTFA_A,   
  data = adult23,    
  nest = TRUE              
)

# Calculate weighted mean of a variable 'age'
mean_urr <- svymean(~URBRRL, nhissvy, na.rm = TRUE)
print(mean_urr)
# Weighted total count
svytable(~URBRRL, nhissvy)

# Weighted proportion of race categories
svytable(~RACEALLP_A, nhissvy)

view(nhissvy)

# Visualization -----------------------------------------------------------


