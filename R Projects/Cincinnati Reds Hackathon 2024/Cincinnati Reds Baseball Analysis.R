library(readxl)
library(dplyr)
library(ggplot2)
library(ds4psy)

# Load data
savant_pitch_level <- read.csv("~/Downloads/savant_pitch_level.csv")
fangraphs_season_level <- read.csv("~/Downloads/fangraphs_season_level.csv")

# Data Preprocessing
strikeouts_per_inning <- 
  fangraphs_season_level %>%
  group_by(PlayerId, NameASCII) %>%
  filter(IP >= 50) %>%
  summarise(
    Season = toString(Season),
    Role = ifelse(length(unique(Role)) >= 2, "Swingman", toString(unique(Role))),
    G = sum(G),
    `% Games Played` = round(100 * G / (n() * 162), 1),
    IP = sum(IP),
    `% Innings Pitches` = round(100 * IP / (G*9), 1),
    SO = sum(SO),
    `SO/IP` = SO / IP
  ) %>%
  arrange(desc(`SO/IP`))

# Data Visualization
aggregate_strikeouts_per_inning_by_role <-strikeouts_per_inning %>% 
  group_by(Role) %>% 
  summarise(`# of Players` = n(),
    `SO/IP`=sum(SO)/sum(IP))

ggplot(aggregate_strikeouts_per_inning_by_role, 
       aes(x=Role, y=`SO/IP`, fill=Role)) +
  geom_bar(stat = 'identity', width = 0.6, colour='black', show.legend = F) +
  ylim(0, 1.2) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Total Strikeouts per Innings Pitched for Each Role") +
  geom_text(aes(label = round(`SO/IP`, digits = 4)), nudge_y = 0.05) +
  theme_ds4psy()

aggregate_innings_pitched_per_game <- strikeouts_per_inning %>%
  group_by(Role) %>%
  summarise(`# of Players` = n(),
    `IP/G` = sum(IP)/sum(G)
  )

ggplot(aggregate_innings_pitched_per_game, 
       aes(x=Role, y=`IP/G`, fill=Role)) +
  geom_bar(stat = 'identity', width = 0.6, colour='black', show.legend = F) +
  ylim(0, 6) +
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Total Innings Pitched per Game for Each Role") +
  geom_text(aes(label = round(`IP/G`, digits = 2)), nudge_y = 0.25) +
  theme_ds4psy()

# Close games analysis of relievers vs starters
# savant_pitch_level_subset <- savant_pitch_level %>%
#   filter(pitcher %in% sample(unique(savant_pitch_level$pitcher), 
#                              0.2*length(unique(savant_pitch_level$pitcher)))) 

all_close_game_situations <- savant_pitch_level %>%
  mutate(score_differential = home_score - away_score) %>%
  filter(score_differential <= 2 & score_differential >= -2) 

number_of_events_per_player_per_game <- all_close_game_situations %>%
  group_by(pitcher, player_name, game_date, home_team) %>%
  #filter(n()>=50) %>%
  summarise(
    Role = toString(unique(role_key)),
    Events = n()
  ) %>%
  arrange(desc(Events), game_date, player_name)
  
diff_in_events_between_SPandRP <- number_of_events_per_player_per_game %>%
  group_by(game_date, home_team) %>%
  summarise(
    `# of Starters` = length(player_name[Role == "SP"]),
    SP_name = toString(player_name[Role == "SP"]),
    `Total SP_events` = sum(Events[Role == "SP"]),
    `Avg SP_events` = `Total SP_events` / `# of Starters`,
    `# of Relievers` = length(player_name[Role == "RP"]),
    RP_name = toString(player_name[Role == "RP"]),
    `Total RP_events` = sum(Events[Role == "RP"]),
    `Avg RP_events` = `Total RP_events` / `# of Relievers`
  ) %>%
  mutate(`Avg SP_events` = ifelse(is.nan(`Avg SP_events`), 0, `Avg SP_events`),
         `Avg RP_events` = ifelse(is.nan(`Avg RP_events`), 0, `Avg RP_events`),
         `Events Differential` = `Avg SP_events` - `Avg RP_events`) %>%
  arrange(`Events Differential`)

ggplot(diff_in_events_between_SPandRP, 
       aes(as.numeric(x = row.names(diff_in_events_between_SPandRP)),
           y=`Events Differential`)) +
  geom_point() +
  geom_segment(aes(x=0,xend=3127.9673,y=54.34639,yend=54.34639), linetype = 'dashed', color = 'blue') +
  geom_segment(aes(x=3127.9673,xend=3127.9673,y=0,yend=54.34639), linetype = 'dashed', color = 'blue') +
  geom_point(aes(x=3127.9673, y=54.34639), color='black', fill='red', shape=21, size = 4) +
  xlab('Game #') +
  ggtitle('Difference in Average Events between Starting and Relief Pitchers\nin Each Game (2021-23)') +
  theme_ds4psy()

# How many events per pitcher per game
events_per_pitcher_perGame <- savant_pitch_level %>%
  group_by(game_date, pitcher, player_name, home_team) %>%
  summarise(`# of events` = n()) %>%
  group_by(pitcher, player_name) %>%
  summarise(avg_events = mean(`# of events`)) %>%
  arrange(desc(avg_events))
mean(events_per_pitcher_perGame$avg_events)










