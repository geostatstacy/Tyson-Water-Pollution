# Author Stacy Woods
# For UCS investigation into total wastewater by Tysons meat processing plants, 2018 - 2022

# Data Source: https://echo.epa.gov/trends/loading-tool/water-pollution-search
# Downloaded 04182024
###Search Type: Discharge Monitoring Report 
###Year: 2018 to 2022 (separately)
###Location: Nationwide
###Community: All communities 
###Pollutant: Without calculated loadings: Wastewater Flow (selected) 
###Industry: All Point Sources 
###Facility Name: “Tyson”, “Hillshire Brands”, “Tecumseh Poultry” (separately)

# packages
library(tidyverse)
library(dplyr)

#read in all cvs's in folder
setwd("...")
getwd()

# bring in all csv's # List all CSV files in the directory
file_list <- list.files(pattern = "*.csv")

# Read all CSV files into a list of data frames, skipping the first three rows
df_list <- file_list %>%
  map(~ read_csv(., skip = 3))

# Assign each data frame from the list to a separate variable in the environment
df_list %>%
  set_names(file_list) %>%
  walk2(names(.), ~ assign(.y, .x, envir = .GlobalEnv))

print(head(HillshireBrands2018.csv, 10))

# Combine all data frames into one, appending by rows
combined_df <- df_list %>%
  reduce(bind_rows)

# Assign the combined data frame to a variable in the environment
assign("combined_df", combined_df, envir = .GlobalEnv)

########################## Clean data ###########################

# remove entries that are not tyson or subsidiary processing plants

colnames(combined_df)
unique(combined_df$`Facility Name`)

# Identify the rows to keep based on "Facility Name"
rows_to_keep <- which(!(combined_df$`Facility Name` %in% c(
  "MCGEE-TYSON AIRPORT",
  "CITY OF ARCADIA - WILLIAM TYSON WWTP",
  "TYSON MHP",
  "TYSONS CENTRAL OFFICE BUILDING A",
  "TYSONS TREE WOOD RECYCLERS",
  "GRANT COUNTY MULCH, INC-- TYSON'S",
  "SCHUSTER CONCRETE READY MIX - SCOTTS RUN/TYSONS",
  "BROWN TYSON RESIDENCE"
)))

# Filter the data frame to keep only the rows identified
mydata <- combined_df[rows_to_keep, ]

# Assign the filtered data frame to a variable in the environment
assign("mydata", mydata, envir = .GlobalEnv)

colnames(mydata)
unique(mydata$`Facility Name`)


###################  Calculate total wastewater ###################

str(mydata$`Total Annual Flow (MMGal)`)
sum(mydata$`Total Annual Flow (MMGal)`, na.rm = TRUE)

# = 87037.29 MMGal = 87.03729 billion gall

# END #
