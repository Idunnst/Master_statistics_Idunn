# Clean workspace
rm(list = ls())

# Load necessary libraries
library(dplyr)
library(readxl)

# Read in data from the Excel file
df <- read_excel("svarV24.xlsx", sheet = "IN2000  Spørreundersøkelse - Vå")

# Mean for each TWQ factor
df$mean_communication <- rowMeans(df[, c(
  "Det er hyppig kommunikasjon innad i teamet", 
  "Teammedlemmene kommuniserer hyppig i spontane møter, telefon, etc.",
  "Teammedlemmene kommuniserer for det meste direkte og personlig med hverandre",
  "Kommunikasjonsflyten er IKKE primært sentrert rundt enkelte personer",
  "Relevante ideer og informasjon om teamarbeidet deles åpent blant alle teammedlemmene",
  "Det er svært sjelden at relevant informasjon blir holdt tilbake fra andre teammedlemmer",
  "I teamet er det få konflikter om åpenhet rundt informasjonsflyten",
  "Teammedlemmene synes at de får informasjon fra andre teammedlemmer i tide",
  "Teammedlemmene synes at de får nøyaktig nok informasjon fra de andre teammedlemmene",
  "Teammedlemmene synes at informasjonen de får fra de andre teammedlemmene er nyttig"
)], na.rm = TRUE)

df$mean_coordination <- rowMeans(df[, c(
  "Teamets deloppgaver er godt organisert", 
  "Teamet har forstått målene for alle deloppgavene", 
  "Teamet har akseptert målene for alle deloppgavene",
  "Det er IKKE motstridende interesser i teamet når det gjelder deloppgaver/delmål"
)], na.rm = TRUE)

df$mean_mutual_support <- rowMeans(df[, c(
  "Teammedlemmene utfyller og understøtter hverandres arbeid", 
  "Hvis konflikter dukker opp, blir de løst enkelt og raskt", 
  "Diskusjoner og uenigheter blir behandlet/tatt opp konstruktivt",
  "Forslag og bidrag fra teammedlemmene blir respektert",
  "Forslag fra teammedlemmene blir diskutert og videreutviklet",
  "Til tross for eventuelle uenigheter, oppnår teamet enighet i viktige spørsmål",
  "Teamet samarbeider godt"
)], na.rm = TRUE)

df$mean_effort <- rowMeans(df[, c(
  "Hvert teammedlem yter full innsats i teamarbeidet", 
  "Hvert teammedlem gir teamarbeidet høyest prioritet", 
  "Teamet er sterkt engasjert i arbeidet sitt", 
  "Det er sjelden konflikter i teamet om det enkelte teammedlemmets arbeidsinnsats"
)], na.rm = TRUE)

df$mean_cohesion <- rowMeans(df[, c(
  "Teamarbeidet er viktig for teamet", 
  "Det er viktig for teammedlemmene å være en del av teamet", 
  "Samarbeidet i teamet har hatt positiv betydning for meg", 
  "Teammedlemmene er sterkt knyttet til teamarbeidet", 
  "Alle teammedlemmene er fullt integrerte i teamet",
  "Det er få personlige konflikter i teamet",
  "Det er gjensidig sympati blant medlemmene i teamet",
  "Det er godt samhold i teamet",
  "Teammedlemmene er stolte av å være en del av teamet",
  "Hvert teammedlem føler sterkt ansvar for teamet"
)], na.rm = TRUE)

df$mean_balance_contrib <- rowMeans(df[, c(
  "Teamet anerkjenner spesielle egenskaper (sterke og svake sider) hos de enkelte teammedlemmene", 
  "Teammedlemmene har de nødvendige egenskapene for å oppnå teamets mål", 
  "Ulik arbeidsinnsats av teammedlemmer fører sjelden til konflikter i teamet"
)], na.rm = TRUE)

df$TWQ_Mean <- rowMeans(df[, c(
  "mean_communication", 
  "mean_coordination", 
  "mean_mutual_support", 
  "mean_effort", 
  "mean_cohesion", 
  "mean_balance_contrib"
)], na.rm = TRUE)

df$TWQ_SD <- apply(df[, c(
  "mean_communication", 
  "mean_coordination", 
  "mean_mutual_support", 
  "mean_effort", 
  "mean_cohesion", 
  "mean_balance_contrib"
)], 1, sd, na.rm = TRUE)

# Manually assign the real team compositions
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

# Merge the real team compositions into df based on Teamnummer
df <- df %>%
  left_join(real_team_compositions, by = "Teamnummer")

# Group by Teamnummer and calculate the TWQ average for each team, including the team composition
df_grouped_by_team <- df %>%
  group_by(Teamnummer, Composition) %>%
  summarise(
    Comm_Mean = mean(mean_communication, na.rm = TRUE),
    Coord_Mean = mean(mean_coordination, na.rm = TRUE),
    MutSup_Mean = mean(mean_mutual_support, na.rm = TRUE),
    Effort_Mean = mean(mean_effort, na.rm = TRUE),
    Cohes_Mean = mean(mean_cohesion, na.rm = TRUE),
    BalContrib_Mean = mean(mean_balance_contrib, na.rm = TRUE),
    TWQ_Mean = mean(TWQ_Mean, na.rm = TRUE),
    TWQ_SD = sd(TWQ_SD, na.rm = TRUE),
    .groups = "drop"
  )

# Display the final TWQ results grouped by team number and composition
print(df_grouped_by_team)
