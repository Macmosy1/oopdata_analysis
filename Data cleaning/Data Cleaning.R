
# Cleaning data -----------------------------------------------------------
nhis_df <- nhis_data %>% 
  janitor::clean_names()

# Select variables and rename them
clean_df <- nhis_df %>% 
  select(urbrrl,region, raceallp_a,hispallp_a, sex_a, agep_a,educp_a,
         ratcat_a, cover_a, cover65_a, phstat_a,hypev_a, hypmed_a, dibev_a, 
         dibins_a,hospongt_a, meddl12m_a, medng12m_a,rxdg12m_a, rxls12m_a, 
         paybll12m_a, payworry_a,ppsu, wtfa_a, pstrat) %>% 
  rename(
    rural_urban = "urbrrl",
    usa_region = "region",
    race = "raceallp_a",
    race_ethnicity = "hispallp_a",
    sex = "sex_a",
    education = "educp_a",
    poverty_categories = "ratcat_a",
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

rename_urban_others_df <- clean_df %>%
  mutate(
    urban_rural = case_when(
      rural_urban %in% c(1:2) ~ "Urban",
      rural_urban == 3 ~ "Medium & Small Metro",
      rural_urban == 4 ~ "Rural"
    ),
    urban_rural = factor(urban_rural, levels = c("Urban", "Medium & Small Metro", "Rural"))
  ) %>%
  mutate(sex = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
    sex %in% c(7,9) ~ NA_character_
    ),
    sex = factor(sex, levels = c("Male", "Female"))
  ) %>% 
  mutate(usa_region = case_when(
    usa_region == 1 ~ "Northeast",
    usa_region == 2 ~ "Midwest",
    usa_region == 3 ~ "South",
    usa_region == 4 ~ "West"
  ),
  usa_region = factor(usa_region, levels = c("Northeast", "Midwest", "South", "West"))
  ) %>%
  mutate(
    age_category = case_when(
      agep_a < 25 ~ "18-24",
      agep_a >= 25 & agep_a < 35 ~ "25-34",
      agep_a >= 35 & agep_a < 45 ~ "35-44",
      agep_a >= 45 & agep_a < 55 ~ "45-54",
      agep_a >= 55 & agep_a < 65 ~ "55-64",
      agep_a >= 65 & agep_a < 75 ~ "65-74",
      agep_a >= 75 ~ "75+"
    ),
    age_category = factor(age_category, levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))
  ) %>%
  mutate(
    race_category = factor(
      case_when(
        race_ethnicity %in% c(5:7) ~ "Non-Hispanic AIAN and any others",
        race_ethnicity == 4 ~ "Non-Hispanic Asian",
        race_ethnicity == 3 ~ "Black Non-Hispanic",
        race_ethnicity == 2 ~ "White Non-Hispanic",
        race_ethnicity == 1 ~ "Hispanic"
      ),
      levels = c("Hispanic", "White Non-Hispanic", "Black Non-Hispanic", "Non-Hispanic Asian", "Non-Hispanic AIAN and any others")
    )
  ) %>%
  mutate(insurance_coverage = case_when(
    insurance_coverage == 1 ~ "Private",
    insurance_coverage == 2 ~ "Medicaid and other public",
    insurance_coverage == 3 ~ "Other coverage",
    insurance_coverage == 4 ~ "Uninsured",
    insurance_coverage == 5 ~ NA_character_
  ),
  insurance_coverage = factor(insurance_coverage, levels = c("Private", "Medicaid and other public", "Other coverage", "Uninsured"))
  )

regroup_education_df <- rename_urban_others_df %>%
  mutate(
    education_categories = factor(
      case_when(
        education %in% c(1:2) ~ "Less than High School",
        education %in% c(3:4) ~ "High School or Equivalent",
        education %in% c(5:7) ~ "Some College or Associate Degree",
        education == 8 ~ "Bachelor's Degree",
        education == 9 ~ "Master's Degree",
        education == 10 ~ "Advanced Degree",
        education %in% c(97, 99) ~ NA_character_, # Combine 97 and 99 as NA
        TRUE ~ as.character(education) # Retain original value for other unmatched codes
      ),
      levels = c("Less than High School", "High School or Equivalent", "Some College or Associate Degree", 
                 "Bachelor's Degree", "Master's Degree", "Advanced Degree")
    )
  )

income_group_df <- regroup_education_df %>%
  mutate(
    income_category = factor(
      case_when(
        poverty_categories %in% c("1", "2", "3", "4", "5") ~ "Low income",
        poverty_categories %in% c("6", "7", "8", "9", "10") ~ "Middle income",
        poverty_categories %in% c("11", "12", "13", "14") ~ "High income",
        poverty_categories == "98" ~ NA_character_
      ),
      levels = c("Low income", "Middle income", "High income")
    )
  )

# Convert unable_to_pay_medical_bills to Yes/No
not_able_to_pay_bill_group_df <- income_group_df %>%
  mutate(unable_to_pay_medical_bills = case_when(
    unable_to_pay_medical_bills == 1 ~ "Yes",
    unable_to_pay_medical_bills == 2 ~ "No",
    unable_to_pay_medical_bills %in% c(7, 8, 9) ~ NA_character_,  # Use NA_character_ for categorical data
    TRUE ~ NA_character_
  ))
# Convert unable_to_pay_medical_bills to binary

not_able_to_pay_bill_binary <- income_group_df %>%
  mutate(not_able_to_pay_bill_binary = case_when(
    unable_to_pay_medical_bills == 1 ~ 1,
    unable_to_pay_medical_bills == 2 ~ 0,
    unable_to_pay_medical_bills %in% c(7, 8, 9) ~ NA_real_,
    TRUE ~ NA_real_  # Change this to NA_real_ to maintain consistent numeric type
  ))
# Convert general_health_status to Excellent/Good/Poor
health_status_group_df <- not_able_to_pay_bill_binary %>%
  mutate(general_health_status = case_when(
    general_health_status %in% c(1, 2) ~ "Excellent",
    general_health_status %in% c(3, 4) ~ "Good",
    general_health_status == 5 ~ "Poor",
    general_health_status %in% c(7, 8, 9) ~ NA_character_,
    TRUE ~ NA_character_
  ))

# Convert delayed care to Yes/No

delay_care_yes_no <- health_status_group_df %>%
  mutate(delay_care_bcos_of_cost = case_when(
    delay_care_bcos_of_cost == 1 ~ "Yes",
    delay_care_bcos_of_cost == 2 ~ "No",  
    delay_care_bcos_of_cost %in% c(7, 8, 9) ~ NA_character_,  # Use NA_character_ for categorical data
    TRUE ~ NA_character_
  ))
# Convert needed_care_but_didnnot_get_due_to_cost to binary (1 = Yes, 0 = No)
final_df <- health_status_group_df %>% 
  mutate(needed_care_but_didnnot_get_due_to_cost = case_when(
    needed_care_but_didnnot_get_due_to_cost == 1 ~ 1,
    needed_care_but_didnnot_get_due_to_cost == 2 ~ 0,
    needed_care_but_didnnot_get_due_to_cost %in% c(7, 8, 9) ~ NA_real_,  # Use NA_real_ for numeric data
    TRUE ~ NA_real_
  ))


final_df <- final_df %>%
  mutate(
    diagnosed_bp = case_when(
      diagnosed_bp == 1 ~ "Yes",
      diagnosed_bp == 2 ~ "No",
      diagnosed_bp %in% c(7, 8, 9) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    diagnosed_bp = factor(diagnosed_bp, levels = c("No", "Yes"))
  )

final_df <- final_df %>%
  mutate(
    diagnosed_diabetes = case_when(
      diagnosed_diabetes == 1 ~ "Yes",
      diagnosed_diabetes == 2 ~ "No",
      diagnosed_diabetes %in% c(7, 8, 9) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    diagnosed_diabetes = factor(diagnosed_diabetes, levels = c("No", "Yes"))
  )


final_df1 <- final_df %>%
  mutate(
    # Keep the categorical version for summary tables
    delay_care_categorical = case_when(
      delay_care_bcos_of_cost == 1 ~ "Yes",
      delay_care_bcos_of_cost == 2 ~ "No",
      delay_care_bcos_of_cost %in% c(7, 8, 9) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # Create the binary version for logistic regression
    delay_care_binary = case_when(
      delay_care_bcos_of_cost == 1 ~ 1,  # Yes → 1
      delay_care_bcos_of_cost == 2 ~ 0,  # No → 0
      delay_care_bcos_of_cost %in% c(7, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
  ),
  unable_to_pay_medical_bills = case_when(
    unable_to_pay_medical_bills == 1 ~ 1,
    unable_to_pay_medical_bills == 2 ~ 0,
    unable_to_pay_medical_bills %in% c(7, 8, 9) ~ NA_real_,
    TRUE ~ NA_real_
  ),
  
  took_less_medicine_categorical = case_when(
  took_less_medication_prescribed_to_save_money == 1 ~ "Yes",
  took_less_medication_prescribed_to_save_money == 2 ~ "No",
  took_less_medication_prescribed_to_save_money %in% c(8, 9) ~ NA_character_,
  TRUE ~ NA_character_
 )
)

final_df <- final_df1 %>% 
  select(sex, urban_rural, usa_region, income_category, education_categories,race_category,
         age_category, ppsu, wtfa_a, pstrat, insurance_coverage, general_health_status,
         diagnosed_bp, taking_bp_medicatiom, diagnosed_diabetes, managing_diabetes_with_insulin,
         hospitalized_overnight,delay_care_categorical,delay_care_binary, needed_care_but_didnnot_get_due_to_cost,
         didnot_get_prescription_bcos_of_cost, took_less_medicine_categorical,
         unable_to_pay_medical_bills,worried_about_being_unable_to_pay_medbills,
         unable_to_pay_medical_bills, not_able_to_pay_bill_binary)



# Creating a Survey Design Object -----------------------------------------
# Define the survey design
nhissvy <- svydesign(
  ids = ~ppsu,              
  strata = ~pstrat,        
  weights = ~wtfa_a,   
  data = final_df,    
  nest = TRUE              
)


# Create state-to-region crosswalk (based on U.S. Census definitions)
state_region <- data.frame(
  state = tolower(state.name),
  region = state.region
) |>
  mutate(region = as.character(region)) |>
  mutate(region = case_when(
    region == "Northeast" ~ "Northeast",
    region == "South" ~ "South",
    region == "North Central" ~ "Midwest",
    region == "West" ~ "West",
    TRUE ~ region
  ))

