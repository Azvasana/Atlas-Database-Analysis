## opening the dataset 
library(readxl)
DCI <- read_excel("~/Desktop/Research/NCDB Project/DCI.xlsx")
View(DCI)

## changing variable types for easy manipulation 
## USE DPLYR, TAKE A LOOK AT WHAT LINDA SENT YOU 
convert_factors_to_levels <- function(file_path, numeric_columns, character_columns) {
  # Read data from Excel file
  data <- readxl::read_excel(file_path)
  
  # Convert specified numeric columns to factor levels
  for (column in numeric_columns) {
    data[[column]] <- factor(data[[column]])
  }
  
  # Convert specified character columns to factor levels
  for (column in character_columns) {
    data[[column]] <- factor(data[[column]])
  }
  
  # Save modified data back to the same Excel file
  write.xlsx(data, file_path, overwrite = TRUE)
  
  print("Specified columns converted to factor levels in the Excel file successfully!")
}

convert_factors_to_levels("~/Desktop/Research/NCDB Project/DCI.xlsx", numeric_columns = c("DCIdi", "DCIqua", "DeathWithin30DaysofSurgery", "Read30day", "DeathWithin90DaysofSurgery", "AdmissionSourceLookup", "Readmittedwithin90days", "mfi", "dispo", "insurance", "complication", "HAC", "Fracture", "Dislocation", "ICH", "crushing", "UTI", "DKA", "secondary", "PSI", "ulcer", "pneumothorax", "infection", "hip", "met", "failure", "PEDVT", "sepsis", "wound", "laceration"), c("Martial_Status"))