# Clean workspace
rm(list = ls())

# Load necessary libraries
library(dplyr)
library(readxl)

# Read in data from the Excel file
df <- read_excel("svarV24.xlsx", sheet = "IN2000  Spørreundersøkelse - Vå")

# Step 1: Map the numeric responses (1 to 6) to the actual roles
df <- df %>%
  mutate(primary_function = case_when(
    `Hva var din primære arbeidsfunksjon i teamet?` == 1 ~ "Programmerer",
    `Hva var din primære arbeidsfunksjon i teamet?` == 2 ~ "Tester",
    `Hva var din primære arbeidsfunksjon i teamet?` == 3 ~ "Designer",
    `Hva var din primære arbeidsfunksjon i teamet?` == 4 ~ "Dokumentasjon",
    `Hva var din primære arbeidsfunksjon i teamet?` == 5 ~ "Arkitektur",
    `Hva var din primære arbeidsfunksjon i teamet?` == 6 ~ "Annet"
  ))

# Step 2: Manually assign the real team compositions
real_team_compositions <- data.frame(
  Teamnummer = 1:54,  # Adjust this range based on your actual team numbers
  Composition = c(
    "3M/3K", "3M/2K", "2M/4K", "0M/6K", "4M/1K", "3M/3K", "3M/3K", "2M/4K", "3M/3K", "3M/3K",
    "2M/4K", "3M/3K", "2M/4K", "5M/0K", "3M/3K", "2M/4K", "3M/3K", "6M/0K", "2M/4K",
    "6M/0K", "4M/2K", "3M/3K", "4M/1K", "2M/4K", "4M/2K", "3M/3K", "1M/5K", "3M/3K", "4M/2K",
    "3M/3K", "5M/1K", "4M/2K", "4M/2K", "3M/3K", "0M/6K", "6M/0K", "3M/3K", "3M/3K", "6M/0K",
    "6M/0K", "5M/0K", "1M/5K", "4M/2K", "6M/0K", "3M/3K", "6M/0K", "5M/0K", "6M/0K", "2M/4K",
    "6M/0K", "3M/3K", "5M/1K", "4M/2K", "2M/4K"
  )
)

# Step 3: Merge the real team compositions into the original dataframe
df <- df %>%
  left_join(real_team_compositions, by = "Teamnummer")

# Step 4: Group by gender, team composition, and primary function
# Calculate the total number of men or women in each composition
df_grouped_by_gender <- df %>%
  group_by(Kjønn, Composition) %>%
  summarise(total_in_composition = n(), .groups = 'drop')

# Merge this total count back into the original dataframe
df <- df %>%
  left_join(df_grouped_by_gender, by = c("Kjønn", "Composition"))

# Now calculate the count and percentage for each primary function within each gender and composition
df_grouped_by_gender_and_function <- df %>%
  group_by(Kjønn, Composition, primary_function) %>%
  summarise(
    count = n(),
    percentage = round((count / first(total_in_composition)) * 100, 2),  # Use first() to avoid repeating counts
    .groups = 'drop'
  )

# Step 5: Display the result without repeated rows
print(df_grouped_by_gender_and_function)

# Filter for women who work as programmers and find the composition with the highest count
top_female_programmer_composition <- df_grouped_by_gender_and_function %>%
  filter(Kjønn == "Kvinne", primary_function == "Programmerer") %>%
  arrange(desc(count)) %>%
  slice(1)  # Select the row with the highest count

# Print the result
print("Combination where women work most as Programmers:")
print(top_female_programmer_composition)

# Filter for women who work as designers and find the composition with the highest count
top_female_designer_composition <- df_grouped_by_gender_and_function %>%
  filter(Kjønn == "Kvinne", primary_function == "Designer") %>%
  arrange(desc(count)) %>%
  slice(1)  # Select the row with the highest count

# Print the result
print("Combination where women work most as Designers:")
print(top_female_designer_composition)
