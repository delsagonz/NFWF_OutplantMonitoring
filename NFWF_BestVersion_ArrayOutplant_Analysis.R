#APAL Outplant Array Analysis ~ NFWF Parameters 

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


#Group Final Areas if they are Fused or Split by Array = 49-WeekArea/#of Fragments ))
#only when an array has "fused" will this line take total weeks' area week and divide it by the number of times it reads that specific array  has "fuse" in the fuse/split coloumn 
ex1 <- X2021_NFWF_Arrays %>%
  group_by(Array) %>%
  mutate(
    result = ifelse(Split_Fuse == 'fuse', Total_Week_Area / sum(Split_Fuse == 'fuse'), Total_Week_Area),
    number_of_entries = ifelse(Split_Fuse == 'fuse', sum(Split_Fuse == 'fuse'), 1)
  ) %>%
  ungroup()

#sums each time an array has an entry of any sort to give you a new column showing you N (# of entires for Array X)
final_summary <- ex1 %>%
  group_by(Array) %>%
  summarise(
    total_result = sum(result),
    total_number_of_entries = sum(number_of_entries)
  )

#Sums up the initial area for each array group 
ex2 <- ex1 %>%
  group_by(Array) %>%
  summarise(
    total_result = sum(result),
    total_number_of_entries = sum(number_of_entries),
    total_initial_area = sum(Initial_Area)
  )

# Merge the final summary and EX2 based on 'Array'
final_combined_data <- merge(final_summary, ex2, by = 'Array', all.x = TRUE)


#Delete duplicates from merge 
final_combined_data <- final_combined_data %>%
  select(-trimws("total_result.x"), -trimws("total_number_of_entries.y"))  # Exclude both 'total_result.x' and 'total_result.y'


# Create the function to calculate growth rate
calculate_growth_rate <- function(total_initial_area, total_result.y) {
  tryCatch({
    growth_rate <- ((total_result.y - total_initial_area) / total_initial_area) * 100
    return(growth_rate)
  }, error = function(e) {
    cat("Error: Initial area cannot be zero.\n")
    return(NULL)
  })
}

# Calculate growth rate using initial and final area columns (%)
final_combined_data$growth_rate <- calculate_growth_rate(final_combined_data$total_initial_area,final_combined_data$total_result.y)

#Finding Change in Surface Area cm2
final_combined_data$growth <- final_combined_data$total_result.y - final_combined_data$total_initial_area

#Finding Change per cm2 of Initial Live Tissue (cm2)
final_combined_data$live_planar_area_per_cm2 <- final_combined_data$growth / final_combined_data$total_initial_area

#Percentage of Live Tissue (Initial Live Tissue Area/Final Live Tissue Area) X 100
final_combined_data$percentage_live_tissue <- (final_combined_data$total_result.y / final_combined_data$total_initial_area) * 100

#Finding mean , SD , SE 
summary_stats <- summary(final_combined_data)

# Identify numeric columns
numeric_cols <- sapply(final_combined_data, is.numeric)

# Subset the data frame to include only numeric columns
numeric_data <- final_combined_data[, numeric_cols]

# Extract mean, sd, and se from the summary
means <- apply(numeric_data, 2, function(x) mean(x, na.rm = TRUE))
sds <- apply(numeric_data, 2, function(x) sd(x, na.rm = TRUE))
ses <- apply(numeric_data, 2, function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))


#This does not reflect if mortality is accounted for 
# Combine the results into a data frame
summary_df <- data.frame(
  Column = names(means),
  Mean = means,
  SD = sds,
  SE = ses
)

# Load the ggplot2 package if not already loaded
#install.packages("ggplot2")
library(ggplot2)

# Create a plot showing change in total array surface area per cm2 
# of intial tissue with a mean across all arrays 
ggplot(final_combined_data, aes(x = Array, y = live_planar_area_per_cm2)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  geom_hline(yintercept = mean(final_combined_data$live_planar_area_per_cm2), linetype = "dotted", color = "orange", size = 1) +
  labs(
    x = "Array",  # Replace "Custom X-Axis Label" with your desired label
    y = "Growth (cm2/cm2 of initial)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#_____________________________-----------work IP zone - need to figure out how to add in DEAD FRAGMENTS TO SUBTRACT IT FROM INTIAL AREA-------------- _______________________________# 

#See Github for details of models
# Fit a linear regression model to estimate growth potential
model <- lm(growth ~ total_initial_area, data = final_combined_data)
# Summary of the model
summary(model)

# Extract the estimated coefficients
coefficients <- coef(model)
# The coefficient for 'initial_area' represents the estimated growth potential
max_growth_potential <- coefficients["total_initial_area"]
# Print the estimated maximum growth potential
print(max_growth_potential)


# Calculate final area after two years with 30% initial mortality and fixed growth potential
percentmort_30 <- final_combined_data$total_initial_area * 0.7 * max_growth_potential * 2
# Display the result
print(percentmort_30)
# Calculate final area after two years with 10% initial mortality and fixed growth potential
percentmort_10 <- final_combined_data$total_initial_area * 0.9 * max_growth_potential * 2
# Display the result
print(percentmort_10)

#_____________________________-----------work IP zone -------------- _______________________________# 

# Calculate growth potential for each array
final_combined_data$growth_potential_per_cm2 <- final_combined_data$growth / final_combined_data$total_initial_area

# Find the maximum growth potential
max_growth_potential_per_cm2 <- max(final_combined_data$growth_potential_per_cm2)

# Print or use the maximum growth potential as needed
print(max_growth_potential_per_cm2)
