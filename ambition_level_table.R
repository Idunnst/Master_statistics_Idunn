# Clean workspace
rm(list = ls())


# Load necessary libraries
library(dplyr)
library(tidyr)
library(readxl)

# Data: Replace this with your actual team ambition data
teams_ambition <- data.frame(
  Teamnummer = 1:54,
  Composition = c(
    "3M/3K", "3M/2K", "2M/4K", "0M/6K", "4M/1K", "3M/3K", "3M/3K", "2M/4K", "3M/3K", "3M/3K",
    "2M/4K", "3M/3K", "2M/4K", "5M/0K", "3M/3K", "2M/4K", "3M/3K", "6M/0K", "2M/4K",
    "6M/0K", "4M/2K", "3M/3K", "4M/1K", "2M/4K", "4M/2K", "3M/3K", "1M/5K", "3M/3K", "4M/2K",
    "3M/3K", "5M/1K", "4M/2K", "4M/2K", "3M/3K", "0M/6K", "6M/0K", "3M/3K", "3M/3K", "6M/0K",
    "6M/0K", "5M/0K", "1M/5K", "4M/2K", "6M/0K", "3M/3K", "6M/0K", "5M/0K", "6M/0K", "2M/4K",
    "6M/0K", "3M/3K", "5M/1K", "4M/2K", "2M/4K"
  ),
  Ambition_Level = I(list(
    c(5, 5, 5, 5, 5, 5), c(5, 5, 5, 4, 4), c(5, 5, 5, 5, 5, 5), c(5, 5, 5, 5, 5, 5),
    c(5, 5, 5, 5, 5), c(5, 5, 5, 5, 5, 5), c(5, 5, 5, 5, 5, 5), c(5, 5, 5, 5, 4, 4),
    c(5, 5, 5, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4),
    c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 3), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4),
    c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4),
    c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4),
    c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4),
    c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 4), c(4, 4, 4, 4, 4, 3), c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3),
    c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3),
    c(3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3),
    c(3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3), c(3, 3, 3, 3, 3, 3),
    c(3, 3, 3, 2, 2, 2), c(2, 2, 2, 2, 1, 1), c(4, 4, 3, 3, 2, 2), c(3, 3, 3, 2, 2, 2), c(3, 3, 3, 3, 3, 4))
  )
)

# Step 4: Unnest the ambition level list
teams_ambition <- teams_ambition %>%
  unnest(Ambition_Level)

# Step 5: Ensure all ambition levels (1-5) are represented, even if they don't appear in some data
all_ambition_levels <- expand.grid(Primary_Ambition_Level = 1:5, Composition = unique(teams_ambition$Composition))

# Step 6: For each team, assign the highest ambition level as the team's "primary" ambition level
teams_ambition <- teams_ambition %>%
  group_by(Teamnummer, Composition) %>%
  summarise(Primary_Ambition_Level = max(Ambition_Level), .groups = 'drop')

# Step 7: Create a summary of teams by primary ambition level and team composition
team_summary <- teams_ambition %>%
  group_by(Primary_Ambition_Level, Composition) %>%
  summarise(count = n(), .groups = 'drop') %>%
  right_join(all_ambition_levels, by = c("Primary_Ambition_Level", "Composition")) %>%
  replace_na(list(count = 0))

# Step 8: Pivot the data to get the ambition levels as rows and compositions as columns
team_table <- team_summary %>%
  pivot_wider(names_from = Composition, values_from = count, values_fill = 0)

# Step 9: Calculate the total row for each ambition level
team_table <- team_table %>%
  mutate(Total = rowSums(select(., -Primary_Ambition_Level)))

# Step 10: Add a total row at the bottom of the table
total_row <- team_table %>%
  summarise(across(everything(), sum)) %>%
  mutate(Primary_Ambition_Level = "Total")  # Label the total row

team_table <- bind_rows(team_table, total_row)

# Step 11: Reorder rows so that ambition levels are sorted in ascending order
team_table <- team_table %>%
  arrange(as.numeric(Primary_Ambition_Level))

# Step 12: Display the final ambition table
print(team_table)

# Read in the data from the Excel file
df <- read_excel("svarV24.xlsx", sheet = "Ambition")

# Calculate the mean and standard deviation for each gender
summary_stats <- df %>%
  group_by(Kjønn) %>%
  summarise(
    Mean_Ambition = mean(Ambition, na.rm = TRUE),
    SD_Ambition = sd(Ambition, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate the overall mean and standard deviation across all data
overall_mean <- mean(df$Ambition, na.rm = TRUE)
overall_sd <- sd(df$Ambition, na.rm = TRUE)

# Display the results
print(summary_stats)
print(paste("Overall Mean Ambition:", overall_mean))
print(paste("Overall Standard Deviation:", overall_sd))