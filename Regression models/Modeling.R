library(ggeffects)
# install.packages("arsenal")
library(arsenal)

# install.packages("tableone")

library(tableone)

# Logistic regression for Delayed Care with survey design

# Objective1
# Descriptive Statistics:Calculate the prevalence of delayed/foregone care due 
# to cost across rural/urban settings, racial/ethnic groups, and income categories.

model1 <- glm(delay_care_binary ~ insurance_coverage,
                        data = final_df,
                           family = quasibinomial())

svy_delay_model1 <- svyglm(delay_care_binary ~ insurance_coverage, 
                           design = nhissvy, 
                           family = quasibinomial())

svy_delay_model2 <- svyglm(delay_care_binary ~ insurance_coverage + income_category + 
                             urban_rural + sex, 
                           design = nhissvy, 
                           family = quasibinomial())

svy_delay_model3 <- svyglm(delay_care_binary ~ insurance_coverage + income_category + 
                             urban_rural + sex + race_category + usa_region + age_category,
                           design = nhissvy, 
                           family = quasibinomial())
summary(svy_delay_model3)


results <- tidy(coeftest(svy_delay_model1), conf.int = TRUE)
print(results, n = Inf)

nhissvy$prediction1 <- predict(svy_delay_model1, nhissvy, type="response")

roc.fit <- roc(nhissvy$delay_care_binary, nhissvy$prediction1)


nhissvy$prediction1 <- predict(svy_delay_model1, newdata = nhissvy, type = "response", se.fit = TRUE)$fit

roc.fit <- roc(nhissvy$delay_care_binary, nhissvy$prediction)
plot(roc.fit, col="blue", main="ROC Curve")
auc(roc.fit)  # Compute AUC

nhissvy$prediction1 <- predict(svy_delay_model1, type = "response")
roc.fit <- roc(nhissvy$delay_care_binary, nhissvy$prediction)
plot(roc.fit, col = "blue", lwd = 2, main = "ROC Curve")
auc(roc.fit)

final_df$prediction <- predict(model1, final_df, type = "response")

# Calculate the ROC curve using the original dataframe
roc.fit <- pROC::roc(final_df$delay_care_binary, final_df$prediction)

# Plot or print ROC as needed
plot(roc.fit)

coords(roc.fit, "best", best.method="youden")

final_df$prediction <- predict(model1, final_df, type="response")
roc.fit <- roc(final_df$delay_care_binary, final_df$prediction)

coords(roc.fit, "best", best.method="youden")

plot(roc.fit, main = "ROC Curve (0.7363)", col = "blue")
# Repeat for other categorical variables
#broom::tidy(svy_delay_model3)

# Creating publication-ready tables summarizing regression model results

tbl_delay_model1 <- svy_delay_model1 |> 
  tbl_regression(
    intercept = TRUE,
    label = list( 'insurance_coverage' ~ 'Insurance Coverage')) 
                  

tbl_delay_model2 <- svy_delay_model2 |> 
  tbl_regression(
    intercept = TRUE,
    label = list( 'insurance_coverage' ~ 'Insurance Coverage',
                  'urban_rural' ~ 'Urban/Rural',
                  'income_category' ~ 'Income Categories',
                  'sex' ~ 'Gender'))

tbl_delay_model3 <- svy_delay_model3 |> 
  tbl_regression(
    intercept = TRUE,
    label = list( 'insurance_coverage' ~ 'Insurance Coverage',
                  'urban_rural' ~ 'Urban/Rural',
                  'income_category' ~ 'Income Categories',
                  'sex' ~ 'Gender',
                  'race_category' ~ 'Race Category',
                  'usa_region' ~ 'USA Region',
                  'age_category' ~ 'Age Category'))
                  

# Merging the models
tbl_merging <-
  tbl_merge(
    tbls = list(tbl_delay_model1,
                tbl_delay_model2,
                tbl_delay_model3),
    tab_spanner = c("**Model 1**", "**Model 2**",  "**Model 3**")
  )

tbl_merging

# Converting the tbl_merging to gt table object
# Saving this as html
gt_table <- 
  as_gt(tbl_merging) %>%
  gt::gtsave(filename = "merged_models.html") 

gt_table

# Model Investigation -----------------------------------------------------

# The ANOVA output compares two nested survey models
anova(svy_delay_model2, svy_delay_model3, test = "F")

# Test Summary Null Hypothesis: The variables income_category, urban_rural, and 
# sex have no effect on delay_care_binary (i.e., their coefficients are all zero).
# Alternative Hypothesis: At least one of these variables significantly contributes 
# to predicting delay_care_binary.
# The test (a Rao‐Scott adjusted F-test based on a likelihood ratio statistic) shows:
# 2logLR = 84.221: This is the likelihood ratio test statistic for the additional predictors.
# p = 1.4562e-10: The extremely small p-value indicates that the additional predictors 
# significantly improve the model fit.
# Denom df = 587: The denominator degrees of freedom used in the F-test.
# Scale factors: These are adjustment factors accounting for the complex survey design.
# Interpretation: We reject the null hypothesis that race_category, usa_region, 
# and age_category have no effect. In other words, adding these predictors significantly 
# improves the explanation of variation in delay_care_binary, justifying their 
# inclusion in the model.


# Variance Inflation Factor (VIF) for checking potential multicollinearity
# In general, a GVIF^(1/(2*Df)) value near 1 indicates very little multicollinearity. 
# Typically, values above 2 (or a VIF above 5 or 10 in simpler cases) might raise concerns.

vif(svy_delay_model3) 

# All the predictors in the model have adjusted GVIF values very close to 1, 
# which indicates that there is no evidence of problematic multicollinearity 
# among them. Each variable appears to provide largely unique information for 
# predicting the outcome.

# Plotting Models ---------------------------------------------------------
# plotting the results of the models from the sjPlot package and saving as png.

model_summary <- tidy(svy_delay_model3, conf.int = TRUE)

model_summary1 <- model_summary %>% filter(p.adjust(p.value) < 0.05)


model_summary2 <- model_summary1 %>%
  mutate(
    estimate = exp(estimate),      # Overwrite estimate with odds ratio
    conf.low = exp(conf.low),      # Overwrite conf.low
    conf.high = exp(conf.high)     # Overwrite conf.high
  )
model_summary2


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



plot_model(
  svy_delay_model1,
  data = nhissvy,  # Explicitly point to your dataset
  vline.color = "blue",
  show.values = TRUE
)

# ggsave("Model1_plot.png", width = 8, height = 6)

plot_model(svy_delay_model2, data = nhissvy,
           vline.color = "blue")
# ggsave("Model2_plot.png", width = 8, height = 6)

# it may be helpful to plot a line to indicate the null value of beta=0
plot_model(svy_delay_model3, data = nhissvy, vline.color = "red")

# Extract coefficients and confidence intervals
svy_delay_model_results <- tidy(svy_delay_model3, conf.int = TRUE)
# Clean variable names for better readability
svy_delay_model_results <- svy_delay_model_results %>%
  mutate(term = str_replace_all(term, "_", " "),   # Replace _ with space
         term = str_replace(term, "(\\w+) (\\w+)$", "\\1 [\\2]"), # Format category in brackets
         term = str_to_title(term))  # Capitalize first letter

model_summary <- broom::tidy(final_mode12, conf.int = TRUE)

ggplot(model_summary, aes(y = term, x = estimate)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Multiple Regression for Delay in Medical Care Model",
    x = "Estimate (Log-OR)", 
    y = "Variables"
  ) +
  theme(
    plot.title = element_text(hjust = 0.3, size = 11, face = "bold", margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 15)),  
    axis.title.y = element_text(size = 12, margin = margin(t = 15)),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")
  )
# Create the forest plot with cleaned labels
ggplot(svy_delay_model_results, aes(y = term, x = estimate)) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Multiple Regression for Delay in Medical Care Model",
       x = "Estimate (Log-OR)", 
       y = "Variables")+
  theme(
    # Adjust spacing between elements
    plot.title = element_text(hjust = 0.3, size = 11, face = "bold", margin = margin(b = 20)),
    axis.title.x = element_text(size = 12, margin = margin(t = 15)),  
    axis.title.y = element_text(size = 12, margin = margin(t = 15)),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted")  
  )

# ggsave("Model3_plot.png", width = 8, height = 6)

# Compare health outcomes by care delay status -----------------------------

svy_health_outcome_model <- svyglm(delay_care_binary ~ diagnosed_diabetes + diagnosed_bp, 
                           design = nhissvy, 
                           family = quasibinomial())


tbl_health_outcome_model <- svy_health_outcome_model |> 
  tbl_regression(
    intercept = TRUE,
    label = list( 'diagnosed_diabetes' ~ 'Diagnosed with Diabetes', 
                  'diagnosed_bp' ~ 'Diagnosed with Hypertension'))


gt_health_outcome_table <- as_gt(tbl_health_outcome_model)

gt_health_outcome_table

health <- tidy(coeftest(svy_health_outcome_model), conf.int = TRUE)

print(health)
# Financial strain among those who delayed care --------------------------------

# Predict financial strain as a function of predictors.

svy_financial_strain_model <- svyglm(not_able_to_pay_bill_binary ~ income_category + insurance_coverage +
                                       urban_rural + diagnosed_bp + diagnosed_diabetes, 
                                   design = nhissvy, 
                                   family = quasibinomial())

summary(svy_financial_strain_model)

tbl_financial_strain_model <- svy_financial_strain_model |> 
  tbl_regression(
    intercept = TRUE,
    label = list( 'income_category' ~ 'Income Category', 
                  'insurance_coverage' ~ 'Insurance Coverage',
                  'urban_rural' ~ 'Urban/Rural',
                  'diagnosed_bp' ~ 'Diagnosed with Hypertension',
                  'diagnosed_diabetes' ~ 'Diagnosed with Diabetes'))


gt_financial_strain_table <- as_gt(tbl_financial_strain_model)

gt_financial_strain_table

finance <- tidy(coeftest(svy_financial_strain_model), conf.int = TRUE)

print(finance)

# Objective3
# Interaction Effects:
# Including interaction terms in logistic regression models to assess how 
# race/ethnicity, and income interactively influence delayed.

# Fit logistic regression with 2-way interaction

svy_interaction_model <- glm(delay_care_binary ~ race_category * income_category,
              data = nhissvy, family = quasibinomial())

summary(svy_interaction_model)

vif(svy_interaction_model) 

tbl_interaction_model <- svy_interaction_model |> 
  tbl_regression(
    intercept = TRUE,
    label = list( 
      'insurance_coverage' = 'Insurance Coverage',
      'income_category' = 'Income Category',
      'urban_rural:race_category' = 'Interaction of Urban/Rural and Race/Ethnicity'
    )
  )
gt_finteraction_table <- as_gt(tbl_interaction_model)

gt_finteraction_table

interactionterm <- tidy(coeftest(svy_interaction_model), conf.int = TRUE)

print(interactionterm)

# plotting the results of the models from the sjPlot package and saving as png.
plot_model(
  svy_interaction_model,
  data = nhissvy,
  vline.color = "blue"
)





final_mode12 <- svyglm(
  delay_care_binary ~ insurance_coverage*(race_category + urban_rural) + 
    income_category + general_health_status + age_category + 
    diagnosed_bp + diagnosed_diabetes +
    not_able_to_pay_bill_binary + took_less_medicine_categorical,
  design = nhissvy,
  family = binomial()
)


results <- tidy(coeftest(final_mode12), conf.int = TRUE)
print(results, n = Inf)

predictions3 <- fitted(final_mode12)  # Fitted probabilities
response3 <- model.frame(final_mode12)$delay_care_binary  # Actual outcomes

# 2. Calculate AUC without modifying the survey design
roc_obj3 <- pROC::roc(response3, predictions3)
auc_score3 <- pROC::auc(roc_obj3)

plot(roc_obj3, main = "ROC Curve (0.818)", col = "blue")
# 3. Print and plot results
cat("AUC:", auc_score2, "\n")
plot(roc_obj2, main = paste("AUC =", round(auc_score2, 3)))

model_summary <- tidy(final_mode12, conf.int = TRUE)

model_summary1 <- model_summary %>% filter(p.value < 0.05)

model_summary2 <- model_summary1 %>%
  mutate(
    estimate = exp(estimate),      # Overwrite estimate with odds ratio
    conf.low = exp(conf.low),      # Overwrite conf.low
    conf.high = exp(conf.high)     # Overwrite conf.high
  )

model_summary2


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

# Creating publication-ready tables filter p-values<0.05 and summarizing regression model results

tbl_delay_model4 <- final_mode12 |> 
  tbl_regression(
    exponentiate = TRUE,
    intercept = TRUE,
    label = list(
      'insurance_coverage:race_category' = 'Insurance Coverage by Race/Ethnicity',
      'race_category:urban_rural' = 'Insurance Coverage by Urban/Rural',
      'income_category' = 'Income Categories',
      'general_health_status' = 'General Health Status',
      'age_category' = 'Age Categories',
      'diagnosed_bp' = 'Diagnosed High BP',
      'diagnosed_diabetes' = 'Diagnosed Diabetes',
      'not_able_to_pay_bill_binary' = 'Not able to pay bills',
      'took_less_medicine_categorical' = 'Took less medicine due to cost'
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