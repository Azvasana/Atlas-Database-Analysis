library(readxl)
library(dplyr)

df <- read_excel("Datasets/Final DCI.xlsx")

# Create summary table for numeric variables
numeric_summary <- df %>%
  summarise(
    Age_median = median(Age, na.rm = TRUE),
    Age_mean = mean(Age, na.rm = TRUE),
    Age_sd = sd(Age, na.rm = TRUE),
    DCI_median = median(DCI, na.rm = TRUE),
    DCI_mean = mean(DCI, na.rm = TRUE),
    DCI_sd = sd(DCI, na.rm = TRUE),
    Income_median = median(income, na.rm = TRUE),
    Income_mean = mean(income, na.rm = TRUE),
    Income_sd = sd(income, na.rm = TRUE),
    TotalCharges_median = median(TotalCharges, na.rm = TRUE),
    TotalCharges_mean = mean(TotalCharges, na.rm = TRUE),
    TotalCharges_sd = sd(TotalCharges, na.rm = TRUE),
    Population_median = median(Population, na.rm = TRUE),
    Population_mean = mean(Population, na.rm = TRUE),
    Population_sd = sd(Population, na.rm = TRUE),
    Distance_median = median(Distance_to_21287, na.rm = TRUE),
    Distance_mean = mean(Distance_to_21287, na.rm = TRUE),
    Distance_sd = sd(Distance_to_21287, na.rm = TRUE),
    DataValue_median = median(Data_Value, na.rm = TRUE),
    DataValue_mean = mean(Data_Value, na.rm = TRUE),
    DataValue_sd = sd(Data_Value, na.rm = TRUE)
  )

# Create summary table for categorical variables
gender_distribution <- df %>%
  count(Gender) %>%
  mutate(percentage = n / sum(n) * 100)

race_distribution <- df %>%
  count(Race) %>%
  mutate(percentage = n / sum(n) * 100)

# Combine summaries into a list
summary_list <- list(
  numeric_summary = numeric_summary,
  gender_distribution = gender_distribution,
  race_distribution = race_distribution
)

summary_df <- as.data.frame(numeric_summary)

summary_table <- summary_df %>%
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
  separate(Statistic, into = c("Variable", "Statistic"), sep = "_", remove = FALSE) %>%
  pivot_wider(names_from = Statistic, values_from = Value)

write_xlsx(summary_table, "Datasets/Summary table.xlsx")
