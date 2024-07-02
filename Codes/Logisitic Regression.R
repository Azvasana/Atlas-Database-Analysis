# Load required library
library(readxl)
library(ROCR)
library(dplyr)
library(tidyr)
library(broom)
library(gt)
library(webshot)
library(writexl)
# Load data from Excel file
data <- read_xlsx("Datasets/Final DCI.xlsx")
View(data)

# Remove rows with missing values in the Data_Value column
data <- data[complete.cases(data$`Annual Checkup Rate`), ]
data <- subset(data, select = -Eventname)

# Replace rows with multiple values with "other"
data$Race <- ifelse(grepl(",", data$Race), "Other", data$Race)
data$Race <- ifelse(grepl("Declined to Answer", data$Race), "Other", data$Race)
data$Race <- ifelse(grepl("Unknown", data$Race), "Other", data$Race)

# Convert character variables to factors (assuming they are categorical)
character_vars <- sapply(data, is.character)
data[, character_vars] <- lapply(data[, character_vars], as.factor)
data[, c("poverty", "vancy", "employment", "establishment")] <- lapply(data[, c("poverty", "vancy", "employment", "establishment")], as.numeric)
data[, c("DeathWithin90DaysofSurgery", "insurance", "Readmittedwithin90days")] <- lapply(data[, c("DeathWithin90DaysofSurgery", "insurance", "Readmittedwithin90days")], as.factor)


# Build logistic regression model
model <- glm(DeathWithin90DaysofSurgery ~ Age + Gender + Race + income + mfi + `Annual Checkup Rate`, family = "binomial", data)
model <- glm(Readmittedwithin90days ~ Age + Gender + Race + income + mfi + `Annual Checkup Rate`, family = "binomial", data)

# Get the summary of the model using broom
tidy_summary <- tidy(model)
write_xlsx(tidy_summary, "logistic_model_summary.xlsx")

# Print summary of the model 
summary_model <- summary(model)
coefficients <- summary_model$coefficients
p_values <- coefficients[, 4]
odds_ratios <- exp(coefficients[, 1])

# Create a data frame with the results
results <- data.frame(
  Term = rownames(coefficients),
  Odds_Ratio = odds_ratios,
  P_Value = p_values,
  stringsAsFactors = FALSE
)

# Round the values for better presentation
results$Odds_Ratio <- round(results$Odds_Ratio, 3)
results$P_Value <- round(results$P_Value, 3)

# Create the gt table with results
summary_table <- gt(data = results) %>%
  tab_header(
    title = "Logistic Regression Results",
    subtitle = "Odds Ratios and P-values (Grouped)"
  ) %>%
  fmt_number(
    columns = c(Odds_Ratio, P_Value),
    decimals = 3
  )

# Save the gt table as a PNG
gtsave(summary_table, "logistic_regression_results_grouped.png")

# Testing reliability of model
predicted <- predict(model,type = "response")
prediction<-as.data.frame(predicted)
result<-cbind(data$DeathWithin90DaysofSurgery, prediction)
colnames(result)<- c("Actul Outcome", "Probability")

#Generate ROCR curves to know best cut off values
ROCRpred <-prediction(predicted, data$DeathWithin90DaysofSurgery)
ROCRref<-performance(ROCRpred, "tpr", "fpr")

#plot ROCR curve
ROCR <- plot(ROCRref, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

# Have to conduct ROC curve analysis first 
result$ourprediction<-ifelse(result$Probability>0.7,1,0)
result$ourprediction <- ifelse(result$ourprediction == 1, "yes", "no")
table(result$`Actul Outcome`)
table(result$`Actul Outcome`, result$ourprediction)
f <- result
f$ourprediction <- ifelse(f$ourprediction == "yes", "Presence", "Absence")
f$ourprediction <- as.factor(f$ourprediction)
cm <- confusionMatrix(f$ourprediction, f$`Actul Outcome`)