library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

data <- read_excel("Datasets/Final DCI.xlsx")

data <- data %>%
  mutate(across(where(is.factor), ~as.numeric(as.character(.))))

# Calculate summary statistics for numeric variables
numeric_summary <- data %>%
  summarise(across(where(is.numeric), list(
    median = ~median(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    proportion_1 = ~mean(. == 1, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

numeric_summary_long <- numeric_summary %>%
  pivot_longer(cols = everything(), names_to = c("variable", "statistic"), names_sep = "_") %>%
  pivot_wider(names_from = "statistic", values_from = "value")

# Calculate summary statistics for character variables
character_summary <- data %>%
  summarise(across(where(is.character), ~length(unique(.)), .names = "{.col}_unique")) 

# Reshape the character summary table for better readability
character_summary_long <- character_summary %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "unique_count") %>%
  mutate(statistic = "unique_count") %>%
  pivot_wider(names_from = "statistic", values_from = "unique_count")

# Combine numeric and character summaries
summary_combined <- bind_rows(numeric_summary_long, character_summary_long)

# Print the summary table
print(summary_combined)

write_xlsx(summary_combined, "Datasets/Summary Table.xlsx")
