library(tidyverse)
library(ggplot2)
library(dplyr)

team_appearances <- read.csv("data/team_appearances.csv")
standings <- read.csv("data/tournament_standings.csv")

appearances <- team_appearances %>%
  mutate(team_name = str_replace(team_name, "West Germany", "Germany")) %>% 
  group_by(team_name) %>% 
  summarise(count = n_distinct(tournament_id)) %>% 
  arrange(-count)

mapdata <- map_data("world") %>% 
  filter(region!="Antarctica")

mapdata <- left_join(mapdata, appearances, by=c("region"='team_name'))

mapdata <- mapdata %>% 
  mutate(region = str_replace(region, "UK", subregion)) %>% 
  mutate(region = str_replace(region, "Great Britain", "England"))

mapdata1 <- mapdata %>% mutate(count = replace_na(count, 0))

goals <- team_appearances %>% 
  group_by(team_name, tournament_name) %>% 
  summarise(goals_for = sum(goals_for), goals_against = sum(goals_against)) %>% 
  mutate(goals_dif = goals_for-goals_against)

goals_all <- team_appearances %>%
  mutate(team_name = str_replace(team_name, "West Germany", "Germany")) %>%
  mutate(team_name = str_replace(team_name, "United States", "USA")) %>% 
  group_by(team_name) %>% 
  summarise(goals_for = sum(goals_for), goals_against = sum(goals_against)) %>% 
  mutate(goals_dif = goals_for-goals_against)
  
mapdatagoals <- left_join(mapdata, goals_all, by=c("region"='team_name'))


ggplot(mapdatagoals, aes( x=long, y=lat, group=group)) +
  geom_polygon(aes(fill = goals_for), color = "black") + 
  scale_fill_gradientn(name = "Liczba bramek", colors = c("#deebf7","#4292c6","#2171b5","#0959aa","#08519c","#0b3f8e","#093577","#08306b")) +
  xlab("") + ylab("") +
  theme_minimal() +
  coord_fixed(ratio=1.15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="left")

mapdatagoals_europe <- subset(
  mapdatagoals, region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                              "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                              "Croatia", "Cyprus", "Czech Republic","Denmark","Estonia","Finland", 
                              "France","Georgia", "Germany", "Greece","Hungary", 
                              "Ireland", "Italy","Kazakhstan", "Kosovo", "Latvia","Liechtenstein", 
                              "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                              "Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                              "Russia","San Marino","Serbia","Slovakia","Slovenia","Spain",
                              "Sweden","Switzerland","Turkey","Ukraine","England","Vatican")) %>% 
  filter(long>0 | lat<62)
  

ggplot(mapdatagoals_europe, aes( x=long, y=lat, group=group)) +
  geom_polygon(aes(fill = goals_for), color = "black") +
  scale_fill_gradientn(name = "Liczba bramek", colors = c("#deebf7","#4292c6","#2171b5","#0959aa","#08519c","#0b3f8e","#093577","#08306b")) +
  xlab("") + ylab("") +
  theme_minimal() +
  coord_fixed(ratio=1.55, xlim = c(-7.9, 42.9), ylim = c(36.6, 70)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 14, family = "serif"))

