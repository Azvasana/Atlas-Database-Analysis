# Load required packages
library(readxl)
library(dplyr)
library(writexl)

# Read the first Excel file
data1 <- read_excel("~/Desktop/Research/NCDB Project/DCI.xlsx")

# Read the second Excel file
data2 <- read_excel("Dartmouth Atlas/Dartmouth Atlas.xlsx")

# Merge datasets based on zip code
merged_data <- merge(data1, data2, by = "Zip Code", all.x = TRUE)

# Write the merged dataset to a new Excel file if needed
write_xlsx(merged_data, "Merged DCI.xlsx")

