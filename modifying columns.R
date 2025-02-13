library(cfbfastR)
library(tidyverse)

# Add home/away columns ---------------------------------------------------
aac_gi <- cfbd_game_info(year = 2024, conference = "AAC", season_type = "both")
aac_df <- read.csv("C:\\Users\\valen\\OneDrive\\Documents\\R_twitter\\aac_attendance.csv")
group_five_attendance <- read.csv("~/R_twitter/group_five_attendance.csv")

# select: week, season_type, neutral_site, conference_game, venue, home_team, home_conference, home_points, away_team, away_conference, away_points
utsa_aac <- aac_df %>% filter(School == "UTSA") %>% select(1, 2)
utsa_df <- aac_gi %>% 
  filter(home_team == "UTSA" | away_team == "UTSA") %>% 
  arrange(desc(season_type)) %>% 
  select(game_id, week, season_type, neutral_site, conference_game, venue, home_team, home_conference, home_points, away_team, away_conference, away_points) %>%
  cbind(utsa_aac)

aac_schools <- aac_df$School %>% unique()
aac_df$School[aac_df$School == "Army West Point"] <- "Army"
aac_df$School[aac_df$School == "ECU"] <- "East Carolina"

group_five_df <- data.frame(game_id = as.numeric(),
                            Date = as.character(),
                            week = as.numeric(),
                            season_type = as.character(),
                            neutral_site = as.logical(),
                            conference_game = as.logical(),
                            venue = as.character(),
                            home_team = as.character(),
                            home_conference = as.character(),
                            home_points = as.numeric(),
                            away_team = as.character(),
                            away_conference = as.character(),
                            away_points = as.numeric(),
                            Attendance = as.numeric()
)

for (i in seq_along(aac_schools)){
  team_df <- aac_df %>% 
    filter(School == aac_schools[[i]]) %>% 
    select(1, 2)
  team_df_gi <- aac_gi %>% 
    filter(home_team == aac_schools[[i]] | away_team == aac_schools[[i]]) %>%
    arrange(desc(season_type)) %>%
    select(game_id, week, season_type, neutral_site, conference_game, venue, home_team, home_conference, home_points, away_team, away_conference, away_points) %>%
    cbind(team_df)
  group_five_df <- rbind(group_five_df, team_df_gi)
}

idk_df <- group_five_df %>% distinct(game_id, .keep_all = TRUE)












