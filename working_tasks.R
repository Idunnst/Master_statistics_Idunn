# Clear the environment
rm(list = ls())

# Load the necessary libraries
library(dplyr)
library(readxl)

# Read in data from the Excel file
df <- read_excel("svarV24.xlsx", sheet = "IN2000  Spørreundersøkelse - Vå")

# Check for missing or out-of-range responses in "Hva var din primære arbeidsfunksjon i teamet?"
missing_responses <- df %>%
  filter(is.na(`Hva var din primære arbeidsfunksjon i teamet?`) | 
           !(`Hva var din primære arbeidsfunksjon i teamet?` %in% 1:6))

print(nrow(missing_responses))  # Print how many rows are excluded (expected: 3)

# Map the numeric responses (1 to 6) to the actual roles, and assign "Unknown" for missing/out-of-range responses
df <- df %>%
  mutate(primary_function = case_when(
    `Hva var din primære arbeidsfunksjon i teamet?` == 1 ~ "Programmerer",
    `Hva var din primære arbeidsfunksjon i teamet?` == 2 ~ "Tester",
    `Hva var din primære arbeidsfunksjon i teamet?` == 3 ~ "Designer",
    `Hva var din primære arbeidsfunksjon i teamet?` == 4 ~ "Dokumentasjon",
    `Hva var din primære arbeidsfunksjon i teamet?` == 5 ~ "Arkitektur",
    `Hva var din primære arbeidsfunksjon i teamet?` == 6 ~ "Annet",
    TRUE ~ "Unknown"  # Assign "Unknown" to any missing or out-of-range responses
  ))

# Group by gender and primary function, calculate count and percentage within each gender
function_by_gender_pct <- df %>%
  group_by(Kjønn, primary_function) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Kjønn) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  pivot_wider(names_from = primary_function, values_from = percentage, values_fill = 0)

# Display the result
print(function_by_gender_pct)

# Calculate total students
total_students <- sum(function_by_gender_pct$count)

# Count the number of Kvinne (Women)
kvinne_count <- sum(function_by_gender_pct$count[function_by_gender_pct$Kjønn == "Kvinne"])

# Count the number of Mann (Men)
mann_count <- sum(function_by_gender_pct$count[function_by_gender_pct$Kjønn == "Mann"])

# Display the results
print(paste("Total Students:", total_students))
print(paste("Kvinne Count:", kvinne_count))
print(paste("Mann Count:", mann_count))
