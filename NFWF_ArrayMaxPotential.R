#APAL Outplant Array Analysis ~ NFWF Parameters - Finding Max Growth Potential Script  

#To whoever uses this - I tried to write notes, but ask away! 
#Feel free to change all df (dataframe) to whatever works best :) ~ Delsa 

#Install if you dont have these - otherwise call them at the start 
library(tidyverse)
library(dplyr)
library(readxl)

#Insert df
X2021_NFWF_Arrays <- read_excel("~/2021_NFWF_Arrays.xlsx")

#arggg .... Remove spaces and slashes from column name
colnames(X2021_NFWF_Arrays) <- gsub(" ", "_", colnames(X2021_NFWF_Arrays), fixed = TRUE)
colnames(X2021_NFWF_Arrays) <- gsub("-", "_", colnames(X2021_NFWF_Arrays), fixed = TRUE)
colnames(X2021_NFWF_Arrays) <- gsub("\\", "_", colnames(X2021_NFWF_Arrays), fixed = TRUE)
colnames(X2021_NFWF_Arrays) <- gsub("49", "Total", colnames(X2021_NFWF_Arrays), fixed = TRUE)


#Dividie Total_Week_Area by the number of times the same value appears in that column for each indiviual array
ex1 <- X2021_NFWF_Arrays %>%
  group_by(Array, Total_Week_Area) %>%
  mutate(
    result = ifelse(Split_Fuse == 'fuse', Total_Week_Area / n(), Total_Week_Area),
    number_of_entries = ifelse(Split_Fuse == 'fuse', n(), 1)
  ) %>%
  ungroup()

# Group by Array and calculate total result and total number of entries
final_summary <- ex1 %>%
  group_by(Array) %>%
  summarise(
    total_result = sum(result),
    total_number_of_entries = sum(number_of_entries)
  )

# Group by Array and calculate total initial area, excluding entries with 'dead' in the action column (this subtracts all the dead arrays initial area from the total new intial area)
ex2 <- X2021_NFWF_Arrays %>%
  filter(!grepl('dead', Action, ignore.case = TRUE)) %>%
  group_by(Array) %>%
  summarise(
    total_initial_area = sum(Initial_Area),
    total_week_entries = n_distinct(Total_Week_Area)
  )

# Merge the final summary and EX2 based on 'Array'
final_combined_data <- merge(final_summary, ex2, by = 'Array', all.x = TRUE)


#Finding Change in Surface Area cm2
final_combined_data$change_cm2 <- final_combined_data$total_result - final_combined_data$total_initial_area


#Finding Change per cm2 of Initial Live Tissue (cm2)
final_combined_data$live_planar_area_per_cm2 <- final_combined_data$change_cm2 / final_combined_data$total_initial_area



# Create the function to calculate growth rate
calculate_growth_rate <- function(total_initial_area, total_result) {
  tryCatch({
    growth_rate <- ((total_result - total_initial_area) / total_initial_area) * 100
    return(growth_rate)
  }, error = function(e) {
    cat("Error: Initial area cannot be zero.\n")
    return(NULL)
  })
}

# Calculate growth rate using initial and final area columns (%)
final_combined_data$growth_rate <- calculate_growth_rate(final_combined_data$total_initial_area,final_combined_data$total_result)

#Finding mean , SD , SE - this will give you mean change, max, min, median, SE, SD, 1st / 3rd Quartile 
summary_stats <- summary(final_combined_data)
view(summary_stats) #extened stats breakdown
view(summary) #abridged with column mean,sd,se

# Identify numeric columns
numeric_cols <- sapply(final_combined_data, is.numeric)

# Subset the data frame to include only numeric columns
numeric_data <- final_combined_data[, numeric_cols]

# Extract mean, sd, and se from the summary
means <- apply(numeric_data, 2, function(x) mean(x, na.rm = TRUE))
sds <- apply(numeric_data, 2, function(x) sd(x, na.rm = TRUE))
ses <- apply(numeric_data, 2, function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))


summary_ <- data.frame(
  Column = names(means),
  Mean = means,
  SD = sds,
  SE = ses
)

view(summary_)

library(ggplot2)

# Create a plot showing change in total array surface area per cm2 
# of intial tissue with a mean across all arrays 
ggplot(final_combined_data, aes(x = Array, y = live_planar_area_per_cm2)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  geom_hline(yintercept = mean(final_combined_data$live_planar_area_per_cm2), linetype = "dotted", color = "orange", size = 1) +
  labs(
    x = "Array",  # Replace "Custom X-Axis Label" with your desired label
    y = "Maximum Growth Potential (cm2/cm2 of initial)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

