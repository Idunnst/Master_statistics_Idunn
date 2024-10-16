rm(list = ls())

# Load the libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)

# Read in data from the Excel file
df <- read_excel("svarV24.xlsx", sheet = "IN2000  Spørreundersøkelse - Vå")

# Convert relevant columns to numeric
df$`Hvor fornøyd er du med stand-up møter?` <- as.numeric(as.character(df$`Hvor fornøyd er du med stand-up møter?`))
df$`Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?` <- as.numeric(as.character(df$`Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?`))
df$`I hvor stor grad har du vært Scrum Master / Team Leder i prosjektperioden?` <- as.numeric(as.character(df$`I hvor stor grad har du vært Scrum Master / Team Leder i prosjektperioden?`))

# Remove outlier where "Hours Spent on Project" is 2030
df <- df %>%
  filter(`Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?` != 2030)

# Now recalculate the mean and standard deviation after removing the outlier
gender_analysis <- df %>%
  group_by(Kjønn) %>%
  summarise(
    mean_hours = mean(`Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?`, na.rm = TRUE),
    sd_hours = sd(`Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?`, na.rm = TRUE),
    
    mean_standup_satisfaction = mean(`Hvor fornøyd er du med stand-up møter?`, na.rm = TRUE),
    sd_standup_satisfaction = sd(`Hvor fornøyd er du med stand-up møter?`, na.rm = TRUE),
    
    mean_scrum_master = mean(`I hvor stor grad har du vært Scrum Master / Team Leder i prosjektperioden?`, na.rm = TRUE),
    sd_scrum_master = sd(`I hvor stor grad har du vært Scrum Master / Team Leder i prosjektperioden?`, na.rm = TRUE)
  )

# Display the result after outlier removal
print(gender_analysis)


# Filter and group the data for team number and gender
df_filtered <- df %>% select("Teamnummer", "Kjønn")

# Calculate gender distribution per team
team_gender_distribution <- df_filtered %>%
  group_by(`Teamnummer`, `Kjønn`) %>%
  summarise(count = n(), .groups = "drop") %>%
  spread(`Kjønn`, count, fill = 0)

# Display the result
print(team_gender_distribution)

# Visualization (optional)
ggplot(df_filtered, aes(x = `Hva er ditt teamnummer?`, fill = `Kjønn`)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution Across Teams", x = "Team Number", y = "Count") +
  theme_minimal()

# Create a team quality score by averaging relevant satisfaction ratings
df <- df %>%
  mutate(twq_score = rowMeans(select(., 
                                     `I det store og hele jobber teamet på en arbeidseffektiv måte`, 
                                     `I det store og hele jobber teamet på en tidseffektiv måte`,
                                     `Teamet holder seg innenfor tidsskjema`,
                                     `Teamet holder seg innenfor planlagt tidsbruk`), na.rm = TRUE))

# Summary of team quality scores by gender
twq_by_gender <- df %>%
  group_by(Kjønn) %>%
  summarise(mean_twq = mean(twq_score, na.rm = TRUE),
            sd_twq = sd(twq_score, na.rm = TRUE))
print(twq_by_gender)

# Primary work function by gender
function_by_gender <- df %>%
  group_by(Kjønn, `Hva var din primære arbeidsfunksjon i teamet?`) %>%
  summarise(count = n())
print(function_by_gender)

# Impact of gender composition on team dynamics
twq_by_composition <- df %>%
  group_by(`Hva er ditt teamnummer?`) %>%
  summarise(mean_twq = mean(twq_score, na.rm = TRUE))
print(twq_by_composition)

# Satisfaction with stand-up meetings and role of Scrum Master
standup_scrum_analysis <- df %>%
  group_by(Kjønn) %>%
  summarise(mean_standup_satisfaction = mean(`Hvor fornøyd er du med stand-up møter?`, na.rm = TRUE),
            scrum_master_count = sum(`Har teamet benyttet Scrum Master / Team Leader i prosjektet?` == "Ja", na.rm = TRUE),
            total_count = n())
print(standup_scrum_analysis)

# Creating ambition levels
df <- df %>%
  mutate(Ambition_Level = case_when(
    `Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?` < 15 ~ 2,
    `Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?` >= 15 & `Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?` < 25 ~ 3,
    `Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?` >= 25 & `Hvor mange timer anslår du at du ukentlig har brukt på prosjektet?` < 35 ~ 4,
    TRUE ~ 5
  ))

# Count the number of men and women per team
gender_composition <- df %>%
  group_by(`Hva er ditt teamnummer?`, Kjønn) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Kjønn, values_from = count, values_fill = 0) %>%
  mutate(Composition = paste0(ifelse(is.na(Kvinne), 0, Kvinne), "W/", ifelse(is.na(Mann), 0, Mann), "M"))

# Combine the ambition levels with gender composition
team_data <- df %>%
  select(`Hva er ditt teamnummer?`, Ambition_Level) %>%
  distinct() %>%
  left_join(gender_composition, by = `Hva er ditt teamnummer?`)

# Count the number of teams by ambition level and gender composition
team_count <- team_data %>%
  group_by(Ambition_Level, Composition) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Composition, values_from = Count, values_fill = 0)

# Display the final table
print(team_count)