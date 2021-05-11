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
library(dplyr)
library(plotly)
setwd("~/Documents/Rice_Junior_Year/Second_Semester/DSCI_304")

## Interactive (hopefully) map
#This one is really really throwing me for a loop right now
#https://www.sciencebase.gov/catalog/item/51bf5940e4b0eb321c798ec9
usacanmap <- st_read("usacanada")
usacanmap <- usacanmap[-c(1:9, 11:17, 76),]
#https://geodata.lib.berkeley.edu/catalog/stanford-bx729wr3020
citiesmap <- st_read("US Cities")
Divisions <- read_excel("NBA Divisions.xlsx")
NBAcities <- citiesmap[c(456, 495, 597, 674, 1840, 3047, 3972, 6061, 7831, 13661,
                         14558, 15100, 15150, 15458, 18791, 19442, 19943, 20430, 20492,
                         20493, 20570, 20927, 21385, 21493, 21545, 22222, 37396,
                         38128, 38129),]
NBAcities <- merge(NBAcities, Divisions, by.x = "NAME", by.y = "City")
#have to add in Toronto data manually because couldn't find a shape file that had both
#US cities as well as Canadian cities
TorontoSF <- data.frame(latitude = 43.6532000000001, longitude = -79.3882000000001,
                        max = 40, Tm = "HOU", Date = "2016-03-06", Team_Result = "W (+6)",
                        Opp = "TOR", Division = "Atlantic")
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

#Maybe add in scores so that it is not just a margin of victory?
HardenGames <- read_excel("Gamelogs Harden.xlsx")
HardenGamesplayed <- subset(HardenGames, is.na(HardenGames$MP) == FALSE)
HardenGamesgrouped <- group_by(HardenGamesplayed, Opp)
MaxGames <- summarise(HardenGamesgrouped, max = max(PTS))
MaxGames <- merge(MaxGames, HardenGamesplayed, by.x = c("Opp", "max"), by.y = c("Opp", "PTS"))
HoustonGame <- c("HOU", 29, NA, NA, "2021-03-03", NA, "BRK", "@", "W (+18)", rep(NA, 22))
DetGame <- c("DET", 44, NA, NA, "2021-03-26", NA, "BRK", "@", "W (+2)", rep(NA, 22))
GSWGame <- c("GSW", 45, NA, NA, "2015-05-25", NA, "HOU", NA, "W (+13)", rep(NA, 22))
MaxGames <- rbind(MaxGames, HoustonGame, DetGame, GSWGame)
MaxGames <- MaxGames[-c(5, 6, 11, 12, 14, 17, 22, 33, 34),]
NBAcities <- merge(NBAcities, MaxGames, by.x = "Abbrev", by.y = "Opp")

Maxmap <- ggplot(usacanmap) + geom_sf() + 
  geom_point(data = NBAcities, aes(color = Division, x = LONGITUDE, y = LATITUDE, 
                                text = paste("Harden scored", max, "for",
                                             Tm, "\n on", substr(Date, 1, 10), "against",
                                             Abbrev))) + 
  scale_color_manual(values = c("red", "green", "blue", "orchid", "cyan", "yellow")) +
  geom_point(data = TorontoSF, mapping = aes(color = Division, x = longitude, y = latitude, 
                                             text = paste("Harden scored", max, "for",
                                                          Tm, "\n on", substr(Date, 1, 10), "against",
                                                          Opp))) +
  map.theme +
  ggtitle("Harden's Career High against Every Team in the NBA")

Maxmap

ggplotly(Maxmap, tooltip = "text")
