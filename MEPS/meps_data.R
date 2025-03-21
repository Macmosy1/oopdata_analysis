
# setwd("C:/Users/Moses Alikali/Desktop/Out of Pocket Research/MEPS") 
# install_github("e-mitchell/meps_r_pkg/MEPS")
# install.packages("tidyverse")
# install.packages("haven")
# install.packages("pagedown")
# install.packages("sjPlot")
# install.packages("kableExtra")


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

# Demographic Characteristics and Healthcare Access-----------------------------

demographic_table <- meps_df4 %>%
  select(gender, age_categories, region, poverty_category, education_level, race_ethnicity) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "no",
    label = list(
      gender = "Gender",
      region = "Geographic Region",
      poverty_category = "Income Level",
      education_level = "Education Level",
      race_ethnicity = "Race/Ethnicity",
      age_categories = "Age Group"
    )
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  modify_caption("**Summary of Study Population Characteristics**") %>%
  bold_labels() %>%
  as_gt()  # Convert to GT table for publication

demographic_table


tbl_delay_care <- meps_df4 %>%
  select(gender, age_categories, region, poverty_category, education_level, 
         race_ethnicity,insurance_coverage, delayed_care_categorical) %>%
  tbl_summary(
    by = delayed_care_categorical,  # Grouping by delay_care
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "ifany"
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  bold_labels()

# Creating table for unable to pay medical bills and each variable
tbl_medical_dept <- meps_df4 %>%
  select(gender, age_categories, region, poverty_category, education_level, 
         race_ethnicity,insurance_coverage, medical_debt) %>%
  tbl_summary(
    by = medical_debt,  # Grouping by unable_to_pay
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "ifany"
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  bold_labels()

# Creating table for health status and each variable
tbl_oop_categeory <- meps_df4 %>%
  select(gender, age_categories, region, poverty_category, education_level, 
         race_ethnicity,insurance_coverage, oop_category) %>%
  tbl_summary(
    by = oop_category,  
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "ifany"
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  bold_labels()

# Combine the tables side by side
Variables_table <- tbl_merge(
  tbls = list(tbl_delay_care, tbl_medical_dept, tbl_oop_categeory),
  tab_spanner = c("**Delayed Care**", "**Medical Bills Debts**", "**Out-of-Pocket-Categories**")
) %>%
  modify_caption("**Comparison of Demographics by Healthcare Access**") %>%
  as_gt()  # Convert to GT table for publication

# Print the final table
Variables_table


# Creating table for diabetes diagnose and each variable
tbl_diabetes <- meps_df4 %>%
  select(gender, age_categories, region, poverty_category, education_level, 
         race_ethnicity,diabetes_diagnosis) %>%
  tbl_summary(
    by = diabetes_diagnosis,  # Grouping by diagnosed_diabetes
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "ifany"
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  bold_labels()

# Creating table for hypertension diagnosed and each variable
tbl_hyperthension <- meps_df4 %>%
  select(gender, age_categories, region, poverty_category, education_level, 
         race_ethnicity, hypertension_diagnosis_cat) %>%
  tbl_summary(
    by = hypertension_diagnosis_cat,  # Grouping by hyperthension
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "ifany"
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  bold_labels()

# Combine the tables side by side
health_condition_table <- tbl_merge(
  tbls = list(tbl_diabetes, tbl_hyperthension),
  tab_spanner = c("**Diagnosed with Diabetes**", "**Hypertension Diagnosed**")
) %>%
  modify_caption("**Comparison of Demographics by Healthcare Condition**") %>%
  as_gt()  # Convert to GT table for publication

# Print the final table
health_condition_table

# Creating heatmap for health outcomes ----------------------------------------

# Creating heatmap for the health care outcomes by USA region)
# Creating Hypertension heatmap
heatmap_data <- meps_df4 %>%
  count(region, hypertension_diagnosis_cat) %>%
  mutate(hypertension_diagnosis_cat = as.factor(hypertension_diagnosis_cat))

bp_heatmap <- ggplot(heatmap_data %>% na.omit(), aes(x = hypertension_diagnosis_cat, y = region, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Diagnosed Hypertension",
       y = "USA Region",
       fill = "Count") +
  theme_minimal() +
  theme(plot.title = element_blank(),   # Remove individual titles
        axis.text = element_text(size = 12))

bp_heatmap
# Creating Diabetes heatmap
diabetes_heatmap_data <- meps_df4 %>%
  drop_na(diabetes_diagnosis ) %>%
  count(region, diabetes_diagnosis , name = "n") 

diabetes_heatmap <- ggplot(diabetes_heatmap_data, aes(x = diabetes_diagnosis , y = region, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Diagnosed Diabetes",
       y = "USA Region",
       fill = "Count") +
  theme_minimal() +
  theme(plot.title = element_blank(),   # Remove individual titles
        axis.text = element_text(size = 12))

diabetes_heatmap
# Combine the plots with a single title

final_plot <- (bp_heatmap + diabetes_heatmap) + 
  plot_annotation(title = "Heatmap of Diagnosed BP and Diabetes by USA Region") & 
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 12, face = "bold"))

final_plot


delay_prevalence <- meps_df4 |>
  mutate(
    poor_health = ifelse(self_rated_health == "Poor", 1, 0),
    delayed_care = delayed_care_binary
  ) |>
  group_by(region) |>
  summarise(
    poor_health = weighted.mean(poor_health, wt = PERWT22F, na.rm = TRUE),
    delayed_care = weighted.mean(delayed_care, wt = PERWT22F, na.rm = TRUE)
  ) |>
  ungroup()

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
# Get U.S. state map data and merge with regions
states_map1 <- map_data("state") |>
  left_join(state_region, by = c("region" = "state")) |>
  left_join(delay_prevalence, by = c("region.y" = "region"))

# Plotting map of prevelence of Poor Health by USA region
poor_health_map <- ggplot(states_map1, aes(x = long, y = lat, group = group, fill = poor_health)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "Prevalence",
    labels = scales::percent,
    option = "C"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 13, face = "bold"))+
  labs(title = "Prevalence of Poor Health by U.S. Region in 2022")

poor_health_map

# Plotting map of prevelence of Delay care by USA region
delay_care_map <- ggplot(states_map1, aes(x = long, y = lat, group = group, fill = delayed_care)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "Prevalence",
    labels = scales::percent,
    option = "D"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 13, face = "bold"))+
  labs(title = "Prevalence of Delayed Care by U.S. Region in 2022")
delay_care_map

# Map for unable to pay medical bills by USA region
# Calculate weighted prevalence for both metrics
medical_debt_prevalence <- meps_df4 |>
  mutate(
    had_medical_debt = ifelse(medical_debt == "Yes", 1,0),
    diagnosed_hypertension = ifelse(hypertension_diagnosis_cat == "Yes", 1, 0)
  ) |>
  group_by(region) |>
  summarise(
    had_medical_debt = weighted.mean(had_medical_debt, wt = PERWT22F, na.rm = TRUE),
    diagnosed_hypertension = weighted.mean(diagnosed_hypertension, wt = PERWT22F, na.rm = TRUE)
  ) |>
  ungroup()

# Get U.S. state map data and merge with regions
states_map2 <- map_data("state") |>
  left_join(state_region, by = c("region" = "state")) |>
  left_join(medical_debt_prevalence, by = c("region.y" = "region"))

medical_debt_map <- ggplot(states_map2, aes(x = long, y = lat, group = group, fill = had_medical_debt)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "Prevalence",
    labels = scales::percent_format(accuracy = 1),
    option = "magma",
    direction = -1
  ) +
  theme_void() +
  labs(title = "Prevalence of Medical Debt by U.S. Region") +
  theme(plot.title = element_text(hjust = 0.9, vjust = 2, size = 13, face = "bold"))

medical_debt_map

# Plotting map of prevalence of Hypertension by USA region
Hypetension_map <- ggplot(states_map2, aes(x = long, y = lat, group = group, fill = diagnosed_hypertension)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "Prevalence",
    labels = scales::percent_format(accuracy = 1),
    option = "inferno",
    direction = -1
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 13, face = "bold"))+
  labs(title = "Prevalence of Diagnosed Hypertension by U.S. Region")

Hypetension_map


# Modelling ---------------------------------------------------------------

# Objective1
# Descriptive Statistics:Calculate the prevalence of delayed/foregone care due 
# to cost across rural/urban settings, racial/ethnic groups, and income categories.

# Final model with interactions and key predictors
#final_model <- svyglm(
  #delayed_care_binary ~ 
   # oop_category * (race_ethnicity + poverty_category + region) +  # Key interactions
   # insurance_coverage + 
   # age_categories + 
   # education_level + 
   # self_rated_health + 
   # hypertension_diagnosis_binary + 
   # diabetes_diagnosis_binary + 
   # medical_debt_binary + 
   # late_utilities_binary,
  #design = meps_design,
 # family = quasibinomial()
#)

#summary(final_model1)
final_mode11 <- svyglm(delayed_care_binary ~ oop_category*(race_ethnicity + region) + 
                         poverty_category + insurance_coverage + self_rated_health + 
                         medical_debt_binary + late_utilities_binary,
                       design = meps_design, family = quasibinomial())

results <- tidy(coeftest(final_mode11), conf.int = TRUE)
print(results, n = Inf)

# Calculate AUC
# 1. Extract predictions and response directly from the model
predictions2 <- fitted(final_mode11)  # Fitted probabilities
response2 <- model.frame(final_mode11)$delayed_care_binary  # Actual outcomes

# 2. Calculate AUC without modifying the survey design
roc_obj2 <- pROC::roc(response2, predictions2)
auc_score2 <- pROC::auc(roc_obj2)

# 3. Print and plot results
cat("AUC:", auc_score2, "\n")
plot(roc_obj2, main = paste("AUC =", round(auc_score2, 3)))

plot(roc_obj2, main = "ROC Curve (0.7363)", col = "blue")
# Creating publication-ready tables filter p-values<0.05 and summarizing regression model results

tbl_delay_model4 <- final_mode11 |> 
  tbl_regression(
    exponentiate = TRUE,
    intercept = TRUE,
    label = list(
      'oop_category:race_ethnicity' = 'Out-of-Pocket by Race/Ethnicity',
      'oop_category:region' = 'Out-of-Pocket by US Region',
      'poverty_category' = 'Poverty Categories',
      'insurance_coverage' = 'Insurance Coverage Status',
      'self_rated_health' = 'Self Rated Health Status',
      'medical_debt_binary' = 'Medical Debt'
    )
  ) |>
  modify_table_body(
    ~ dplyr::filter(.x, is.na(p.value) | p.value < 0.05)
  )

# Converting the tbl_merging to gt table object
gt_table <- 
  as_gt(tbl_delay_model4) %>%
  gt::gtsave(filename = "Logistic_models1.html") 

gt_table

gt_table2 <- as_gt(tbl_delay_model4)
gt_table2
# Plotting Models ---------------------------------------------------------
# plotting the results of the models from the sjPlot package and saving as png.


plot_model(
  final_mode12,
  data = meps_design,  # Explicitly point to your dataset
  vline.color = "blue",
  show.values = TRUE
)

# Create the forest plot with cleaned labels

# model_summary <- broom::tidy(final_mode12, conf.int = TRUE)
# Filtering the variables to display only the significant ones

model_summary <- tidy(final_mode12, conf.int = TRUE)

model_summary1 <- model_summary %>% filter(p.value < 0.05)

model_summary2 <- model_summary1 %>%
  mutate(
    estimate = exp(estimate),      # Overwrite estimate with odds ratio
    conf.low = exp(conf.low),      # Overwrite conf.low
    conf.high = exp(conf.high)     # Overwrite conf.high
  )

svy_delay_model_results <- model_summary2 %>%
  mutate(term = str_replace_all(term, "_", " "),   # Replace _ with space
         term = str_replace(term, "(\\w+) (\\w+)$", "\\1 [\\2]"), # Format category in brackets
         term = str_to_title(term))  # Capitalize first letter

ggplot(svy_delay_model_results, aes(y = term, x = estimate)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Multiple Regression Model for Delayed Care",
    x = "Estimate (Odds Ratio)", 
    y = "Predictors"
  ) +
  theme(
    plot.title = element_text(hjust = 0.3, size = 11, face = "bold", margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 15)),  
    axis.title.y = element_text(size = 12, margin = margin(t = 15)),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )

