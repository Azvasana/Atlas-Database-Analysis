knitr::opts_chunk$set(echo = TRUE)
install.packages("tidyverse")
library(tidyverse)

file_name <- "Dartmouth Atlas/Dartmouth_Atlas_HRR_PCP_access.csv"
df <- read_csv(file_name)

cross_walk <- read_csv("Dartmouth Atlas/ZipHsaHrr19.csv")

df %>% count(Cohort)

df <- df %>%
  filter(Year == 2019) %>% ## only do a yearly analysis now, later change this into a multi year analysis
  filter(Race == "Overall") %>%
  filter(Eventname == "ptbjune_amcare2")

df <- df %>% # 307 hrrs?
  dplyr::select(Geo_code, Population, Year, Eventname, Observed, Crude_Rate) %>%
  rename(hrrnum = Geo_code)

cross_walk <- cross_walk %>% # 306 hrrs?
  dplyr::select(zipcode19, hrrnum, hrrcity, hrrstate)

df <- left_join(df, cross_walk) 
df %>% head(5)

# Saving cleaned data 
install.packages("openxlsx")
library(openxlsx)
Dartmouth_Atlas <- data.frame(df)
file_name <- "Dartmouth Atlas.xlsx"
write.xlsx(Dartmouth_Atlas, file_name, rowNames = FALSE)