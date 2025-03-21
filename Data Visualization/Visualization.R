

# Demographic Table -----------------------------------------------------------

# Creating demographic characteristic for the respondents
demographic_table <- final_df %>%
  select(sex, urban_rural, usa_region, income_category, education_categories, race_category, age_category) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "no",
    label = list(
      urban_rural = "Residence Type",
      usa_region = "Geographic Region",
      income_category = "Income Level",
      education_categories = "Education Level",
      race_category = "Race/Ethnicity",
      age_category = "Age Group"
    )
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  modify_caption("**NHIS Data Summary of Study Population Characteristics**") %>%
  bold_labels() %>%
  as_gt()  # Convert to GT table for publication

demographic_table
# gtsave(demographic_table, "demographic_table_output.html")


# Summary table for healthcare variables ---------------------------------------

# Creating table for delay care and each variable
tbl_delay_care <- final_df %>%
  select(sex, urban_rural, usa_region, income_category, education_categories, 
         race_category, age_category, delay_care_categorical) %>%
  tbl_summary(
    by = delay_care_categorical,  # Grouping by delay_care
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "ifany"
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  bold_labels()

# Creating table for unable to pay medical bills and each variable
tbl_unable_pay <- final_df %>%
  select(sex, urban_rural, usa_region, income_category, education_categories, 
         race_category, age_category, unable_to_pay_medical_bills) %>%
  tbl_summary(
    by = unable_to_pay_medical_bills,  # Grouping by unable_to_pay
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "ifany"
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  bold_labels()

# Creating table for health status and each variable
tbl_health_status <- final_df %>%
  select(sex, urban_rural, usa_region, income_category, education_categories, 
         race_category, age_category, general_health_status) %>%
  tbl_summary(
    by = general_health_status,  # Grouping by general health status
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "ifany"
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  bold_labels()

# Combine the tables side by side
Variables_table <- tbl_merge(
  tbls = list(tbl_delay_care, tbl_unable_pay, tbl_health_status),
  tab_spanner = c("**Delayed Care**", "**Unable to Pay Medical Bills**", "**General Health Status**")
) %>%
  modify_caption("**Comparison of Demographics by Healthcare Access**") %>%
  as_gt()  # Convert to GT table for publication

# Print the final table
Variables_table

# Saving the merge demographics by healthcare access table
# gtsave(Variables_table, "health_access_table_output.html")


# Comparing Diabetes and Hypertension diagnosed by delayed care ----------------
# Creating table for diabetes diagnose and each variable
tbl_diabetes <- final_df %>%
  select(sex, urban_rural, usa_region, income_category, education_categories, 
         race_category, age_category, diagnosed_diabetes) %>%
  tbl_summary(
    by = diagnosed_diabetes,  # Grouping by diagnosed_diabetes
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  
    missing = "ifany"
  ) %>%
  modify_header(label = "**Demographic Characteristics**") %>%
  bold_labels()

# Creating table for hypertension diagnosed and each variable
tbl_hyperthension <- final_df %>%
  select(sex, urban_rural, usa_region, income_category, education_categories, 
         race_category, age_category, diagnosed_bp) %>%
  tbl_summary(
    by = diagnosed_bp,  # Grouping by hyperthension
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
heatmap_data <- final_df %>%
  count(usa_region, diagnosed_bp) %>%
  mutate(diagnosed_bp = as.factor(diagnosed_bp))

bp_heatmap <- ggplot(heatmap_data %>% na.omit(), aes(x = diagnosed_bp, y = usa_region, fill = n)) +
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
diabetes_heatmap_data <- final_df %>%
  drop_na(diagnosed_diabetes) %>%
  count(usa_region, diagnosed_diabetes, name = "n") 

diabetes_heatmap <- ggplot(diabetes_heatmap_data, aes(x = diagnosed_diabetes, y = usa_region, fill = n)) +
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
final_plot <- (bp_heatmap | diabetes_heatmap) + 
  plot_annotation(title = "Heatmap of Diagnosed BP and Diabetes by USA Region") & 
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 12, face = "bold"))

# Print the final combined plot
print(final_plot)


# Graphs and Maps for Delay care, Insurance, and others ------------------------

ggplot(final_df %>% na.omit(), aes(x = race_category, fill = insurance_coverage)) + 
  geom_bar(position = "fill") +  # Stacked proportions
  labs(title = "Race/Ethnicity by Insurance Coverage",
       x = "Race Category",
       y = "Proportion",
       fill = "Insurance Coverage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 12, face = "bold"))+
  coord_flip() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "wheat", "lavender"))

  # Create the Bar chart
ggplot(final_df %>% na.omit(), aes(x = delay_care_categorical, fill = urban_rural)) +
  geom_bar(position = "dodge", alpha = 0.7) +  # 'dodge' to separate or 'stack' to overlay
  labs(title = "Delay in Care by Rural and Urban",
       x = "Delay in Care",
       y = "Count",
       fill = "Urban/Rural") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 12, face = "bold"))+
  scale_fill_manual(values = c("Urban" = "lightblue", "Medium & Small Metro"= "wheat", "Rural" = "lightgreen"))

# Summarize poor health and delayed care prevalence (with survey weights)
delay_prevalence <- final_df |>
  mutate(
    poor_health = ifelse(general_health_status == "Poor", 1, 0),
    delay_care = delay_care_binary
  ) |>
  group_by(usa_region) |>
  summarise(
    poor_health = weighted.mean(poor_health, wt = wtfa_a, na.rm = TRUE),
    delay_care = weighted.mean(delay_care, wt = wtfa_a, na.rm = TRUE)
  ) |>
  ungroup()

# Get U.S. state map data and merge with regions
states_map1 <- map_data("state") |>
  left_join(state_region, by = c("region" = "state")) |>
  left_join(delay_prevalence, by = c("region.y" = "usa_region"))

# Plotting map of prevelence of Poor Health by USA region
poor_health_map <- ggplot(states_map1, aes(x = long, y = lat, group = group, fill = poor_health)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "Prevalence",
    labels = scales::percent,
    option = "C"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 13, face = "bold"))

# Plotting map of prevelence of Delay care by USA region
delay_care_map <- ggplot(states_map1, aes(x = long, y = lat, group = group, fill = delay_care)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "Prevalence",
    labels = scales::percent,
    option = "D"
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 13, face = "bold"))

# Combine the plots with a single title

final_plot <- (poor_health_map + delay_care_map) + 
  plot_annotation(title = "Map of Prevalence of Poor Health and Delayed care by USA Region") & 
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 12, face = "bold"))

final_plot
# Map for unable to pay medical bills by USA region
# Calculate weighted prevalence for both metrics
unable_bill_prevalence <- final_df |>
  mutate(
    not_able_to_pay_bill_binary = ifelse(not_able_to_pay_bill_binary == "Yes", 1,0),
    diagnosed_bp = ifelse(diagnosed_bp == "Yes", 1, 0)
  ) |>
  group_by(usa_region) |>
  summarise(
    not_able_to_pay_bill_binary = weighted.mean(not_able_to_pay_bill_binary, wt = wtfa_a, na.rm = TRUE),
    diagnosed_bp = weighted.mean(diagnosed_bp, wt = wtfa_a, na.rm = TRUE)
  ) |>
  ungroup()
unable_bill_prevalence


# Load U.S. state shapes and merge with region data
states_map <- map_data("state") |>
  left_join(state_region, by = c("region" = "state")) |>
  left_join(unable_bill_prevalence, by = c("region.y" = "usa_region"))

unable_bills_map <- ggplot(states_map, aes(x = long, y = lat, group = group, fill = unable_to_pay_medical_bills )) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "Prevalence",
    labels = scales::percent_format(accuracy = 1),
    option = "magma",
    direction = -1
  ) +
  theme_void() +
  labs(title = "Prevalence of Unable to Pay Medical Bills by U.S. Region")+
  theme(plot.title = element_text(hjust = 0.9, vjust = 2, size = 13, face = "bold"))

unable_bills_map

# Plotting map of prevalence of Hypertension by USA region
Hypetension_map <- ggplot(states_map, aes(x = long, y = lat, group = group, fill = diagnosed_bp)) +
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

# Plotting Models ---------------------------------------------------------
# plotting the results of the models from the sjPlot package and saving as png.
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

# OR

# Extract coefficients and confidence intervals
svy_delay_model_results <- tidy(svy_delay_model3, conf.int = TRUE)
# Clean variable names for better readability
svy_delay_model_results <- svy_delay_model_results %>%
  mutate(term = str_replace_all(term, "_", " "),   # Replace _ with space
         term = str_replace(term, "(\\w+) (\\w+)$", "\\1 [\\2]"), # Format category in brackets
         term = str_to_title(term))  # Capitalize first letter

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

  