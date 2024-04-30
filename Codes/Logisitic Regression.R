# Load required library
library(readxl)

# Load data from Excel file
data <- read_excel("~/Desktop/Research/NCDB Project/DCI.xlsx")

# Convert character variables to factors (assuming they are categorical)
character_vars <- sapply(data, is.character)
data[, character_vars] <- lapply(data[, character_vars], as.factor)
data[, "DeathWithin30DaysofSurgery"] <- lapply(data[, "DeathWithin30DaysofSurgery"], as.factor)

# Build logistic regression model
model <- glm(DeathWithin30DaysofSurgery ~ Age + Gender + Race, family = "binomial", data)

# Print summary of the model
summary(model)
model$coefficients

# Testing reliability of model
predicted <- predict(model,type = "response")
prediction<-as.data.frame(predicted)
result<-cbind(data$DeathWithin30DaysofSurgery, prediction)
colnames(result)<- c("Actul Outcome", "Probability")

#Generate ROCR curves to know best cut off values
ROCRpred <-prediction(predicted, data$DeathWithin30DaysofSurgery)
ROCRref<-performance(ROCRpred, "tpr", "fpr")

#plot ROCR curve
plot(ROCRref, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

# Have to conduct ROC curve analysis first 
result$ourprediction<-ifelse(result$probability>0.1,1,0)
table(result$actuloutcome)