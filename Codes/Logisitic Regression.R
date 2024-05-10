# Load required library
library(readxl)
library(ROCR)
# Load data from Excel file
data <- read_excel("Datasets/Final DCI.xlsx")

# Remove rows with missing values in the Data_Value column
data <- data[complete.cases(data$Data_Value), ]

# Convert character variables to factors (assuming they are categorical)
character_vars <- sapply(data, is.character)
data[, character_vars] <- lapply(data[, character_vars], as.factor)
data[, c("employment", "establishment")] <- lapply(data[, c("employment", "establishment")], as.numeric)
data[, c("DeathWithin90DaysofSurgery", "insurance", "mfi")] <- lapply(data[, c("DeathWithin90DaysofSurgery", "insurance", "mfi")], as.factor)

# Build logistic regression model
model <- glm(DeathWithin90DaysofSurgery ~ Age + Gender + Race + income + insurance + mfi + Data_Value, family = "binomial", data)

# Print summary of the model
summary(model)
model$coefficients

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
result$ourprediction<-ifelse(result$Probability>0.1,1,0)
table(result$`Actul Outcome`)
