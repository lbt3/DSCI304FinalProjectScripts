setwd("~/Documents/Rice_Junior_Year/Second_Semester/DSCI_304")
library(dplyr)

#Nbastats from https://www.kaggle.com/drgilermo/nba-players-stats
Playerstates <- read.csv("Players.csv")
Playerswstartyear <- read.csv("player_data.csv")
MainlandStatenames <- read.csv("MainlandStates.csv")

Playerstates <- subset(Playerstates, Playerstates$birth_state %in% MainlandStatenames$State)
Playerspost2000 <- subset(Playerswstartyear, Playerswstartyear$year_start >= 2000)

USPlayerspost2000 <- merge(Playerspost2000, Playerstates, by.x = 'name', by.y = 'Player')
USPlayerspost2000grouped <- group_by(USPlayerspost2000, USPlayerspost2000$birth_state)
Statetotals <- summarise(USPlayerspost2000grouped, count = n())
names(Statetotals)[names(Statetotals) == "USPlayerspost2000$birth_state"] <- "State"
names(Statetotals)[names(Statetotals) == "count"] <- "NBA Player Count"

`%ni%` = Negate(`%in%`)

Notrepresentedstates <- subset(MainlandStatenames, MainlandStatenames$State %ni% Statetotals$State)

Notrepresentedstates <- cbind(Notrepresentedstates$State, c(0, 0, 0))
Notrepresentedstates <- as.data.frame(Notrepresentedstates)
names(Notrepresentedstates)[names(Notrepresentedstates) == "V1"] <- "State"
names(Notrepresentedstates)[names(Notrepresentedstates) == "V2"] <- "NBA Player Count"
#based on subset of NBA players from 2000-2017 (remember to qualify this)
NBA_Player_Count_by_State <- rbind(Statetotals, Notrepresentedstates)
NBA_Player_Count_by_State$`NBA Player Count` <- as.numeric(NBA_Player_Count_by_State$`NBA Player Count`)

#Actually making the map now
library(tidyr)
library(zoo)
library(readxl)
library(lubridate)
library(ggplot2)
library(scales)
library(sp)
library(rgdal)
library(cowplot)
library(ggpubr)
library(maps)
library(sf)
library(GISTools)

#Only for mainland states (its not as if Hawaii and Alaska are NBA powerhouses, after all)
usamap <- st_read("States")
usamap <- usamap[-c(28, 40, 48, 49, 50, 52, 56),]
usamap_Player_totals <- merge(usamap, NBA_Player_Count_by_State, by.x = "NAME", by.y = "State")

map.theme<-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 panel.background=element_blank(),panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),plot.background=element_blank(),
                 plot.title = element_text(hjust = 0.5, size = 12),
                 plot.subtitle = element_text(size = 8, hjust = 0.5),
                 legend.title = element_text(size = 7))

Player_total_map <- ggplot(data = usamap_Player_totals) + 
  geom_sf(aes(fill = `NBA Player Count`)) + map.theme +
  scale_fill_continuous(high = "#FEF001", low = "#F00505") +
  ggtitle("Total NBA Players From Each state", 
          subtitle = "From players who entered the league from 2000-2017")

Player_total_map

