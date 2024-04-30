# Load required libraries
library(readxl)
library(ggplot2)

# Function to create a bar graph from an Excel file
create_bar_graph <- function(file_path, column_name, output_file_name) {
  # Read data from Excel file
  data <- readxl::read_excel(file_path)
  
  # Extract column data
  column_data <- data[[column_name]]
  
  # Count occurrences of each unique value in the column
  value_counts <- table(column_data)
  
  # Create a data frame from value counts
  bar_data <- data.frame(names(value_counts), as.numeric(value_counts))
  colnames(bar_data) <- c("Category", "Count")
  
  # Create bar graph
  bar_graph <- ggplot(bar_data, aes(x = Category, y = Count, fill = Category)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Bar Graph of", column_name),
         x = "Category",
         y = "Count",
         fill = "Category") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the bar graph as a PNG file
  ggsave(output_file_name, plot = bar_graph, width = 10, height = 6)
  
  print("Bar graph created successfully!")
}

create_bar_graph("example.xlsx", "Column1", "bar_graph.png") 

## BAR GRAPH OF ZIPCODES AND STATES 
# Load required libraries
library(zipcodeR)
library(dplyr)

# Load data from Excel file
data <- read_excel("~/Desktop/Research/NCDB Project/DCI.xlsx")

data <- data %>%
  mutate(state = sapply(Zip_Code, function(zip) reverse_zipcode(zip)$state))

# Create a bar graph of states
state_counts <- data %>%
  count(state)

# Plot the bar graph
p <- ggplot(state_counts, aes(x = state, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Zip Codes by State",
       x = "State",
       y = "Number of Zip Codes")

# Saving as a PNG
ggsave("zipcode_distribution.png", plot = p, width = 8, height = 6, units = "in", dpi = 300)
