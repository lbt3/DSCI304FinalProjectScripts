packages = c("shiny", "tidyverse", "hexbin")
install.packages(packages, repos = "https://cran.rstudio.com/")
library(shiny)
runGitHub("ballr", "toddwschneider")
library(ggplot2)

#MAKE SURE TO CREDIT ballr PACKAGE AND toddwschneider!!!!!!!!

HardenID <- find_player_id_by_name("James Harden")
#Getting Data for multiple seasons
Hardenshots1415 <- fetch_shots_by_player_id_and_season(HardenID, "2014-15")
Hardenshots1415 <- Hardenshots1415$player
Hardenshots1415_filter <- subset(Hardenshots1415, Hardenshots1415$shot_zone_basic != "Restricted Area")

Hardenshots1516 <- fetch_shots_by_player_id_and_season(HardenID, "2015-16")
Hardenshots1516 <- Hardenshots1516$player
Hardenshots1516_filter <- subset(Hardenshots1516, Hardenshots1516$shot_zone_basic != "Restricted Area")

Hardenshots1617 <- fetch_shots_by_player_id_and_season(HardenID, "2016-17")
Hardenshots1617 <- Hardenshots1617$player
Hardenshots1617_filter <- subset(Hardenshots1617, Hardenshots1617$shot_zone_basic != "Restricted Area")

Hardenshots1718 <- fetch_shots_by_player_id_and_season(HardenID, "2017-18")
Hardenshots1718 <- Hardenshots1718$player
Hardenshots1718_filter <- subset(Hardenshots1718, Hardenshots1718$shot_zone_basic != "Restricted Area")

Hardenshots1819 <- fetch_shots_by_player_id_and_season(HardenID, "2018-19")
Hardenshots1819 <- Hardenshots1819$player
Hardenshots1819_filter <- subset(Hardenshots1819, Hardenshots1819$shot_zone_basic != "Restricted Area")

Hardenshots19_20 <- fetch_shots_by_player_id_and_season(HardenID, "2019-20")
Hardenshots19_20 <- Hardenshots19_20$player
Hardenshots19_20_filter <- subset(Hardenshots19_20, Hardenshots19_20$shot_zone_basic != "Restricted Area")

Harden1415_graph <- generate_heatmap_chart(Hardenshots1415_filter, plot_court())
Harden1516_graph <- generate_heatmap_chart(Hardenshots1516_filter, plot_court())
Harden1617_graph <- generate_heatmap_chart(Hardenshots1617_filter, plot_court())
Harden1718_graph <- generate_heatmap_chart(Hardenshots1718_filter, plot_court())
Harden1819_graph <- generate_heatmap_chart(Hardenshots1819_filter, plot_court())
Harden19_20_graph <- generate_heatmap_chart(Hardenshots19_20_filter, plot_court())

themeforall <- theme(plot.title = element_text(size = 12, hjust = 0.5), 
                     plot.subtitle = element_text(size = 9, hjust = 0.5),
                     legend.key.size = unit(0.5, 'cm'), 
                     legend.key.width = unit(0.25, 'cm'), 
                     legend.title = element_text(size=8),
                     legend.text = element_text(size=8))

scaleforall <-  scale_fill_viridis_c( "Shot Frequency",
                                      limits = c(0, 1),
                                      breaks = c(0, 1),
                                      labels = c("lower", "higher"),
                                      option = "inferno",
                                      guide = guide_colorbar(barwidth = 9))
Harden1415_graph <- Harden1415_graph + 
  ggtitle("James Harden", subtitle = "2014-15 Regular Season \n Houston Rockets") + 
  themeforall +
  scaleforall

Harden1516_graph <- Harden1516_graph + 
  ggtitle("James Harden", subtitle = "2015-16 Regular Season \n Houston Rockets") + 
  themeforall +
  scaleforall

Harden1617_graph <- Harden1617_graph + 
  ggtitle("James Harden", subtitle = "2016-17 Regular Season \n Houston Rockets") + 
  themeforall +
  scaleforall

Harden1718_graph <- Harden1718_graph + 
  ggtitle("James Harden", subtitle = "2017-18 Regular Season \n Houston Rockets") + 
  themeforall +
  scaleforall

Harden1819_graph <- Harden1819_graph + 
  ggtitle("James Harden", subtitle = "2018-19 Regular Season \n Houston Rockets") + 
  themeforall +
  scaleforall

Harden19_20_graph <- Harden19_20_graph + 
  ggtitle("James Harden", subtitle = "2019-20 Regular Season \n Houston Rockets") + 
  themeforall +
  scaleforall


Harden1415_graph
Harden1516_graph
Harden1617_graph
Harden1718_graph
Harden1819_graph
Harden19_20_graph

library(magick)

Harden1415_graph_photo <- image_read("/Users/lucastyler/Documents/Rice_Junior_Year/Second_Semester/DSCI_304/Harden Chart 1415.png")
Harden1516_graph_photo <- image_read("/Users/lucastyler/Documents/Rice_Junior_Year/Second_Semester/DSCI_304/Harden Shot Chart 1516.png")
Harden1617_graph_photo <- image_read("/Users/lucastyler/Documents/Rice_Junior_Year/Second_Semester/DSCI_304/Harden Shot Chart 1617.png")
Harden1718_graph_photo <- image_read("/Users/lucastyler/Documents/Rice_Junior_Year/Second_Semester/DSCI_304/Harden Shot Chart 1718.png")
Harden1819_graph_photo <- image_read("/Users/lucastyler/Documents/Rice_Junior_Year/Second_Semester/DSCI_304/Harden Shot Chart 1819.png")
Harden19_20_graph_photo <- image_read("/Users/lucastyler/Documents/Rice_Junior_Year/Second_Semester/DSCI_304/Harden Shot Chart 1920.png")

Shot_chart_progression <- image_resize(c(Harden1415_graph_photo, Harden1516_graph_photo, Harden1617_graph_photo,
               Harden1718_graph_photo, Harden1819_graph_photo, Harden19_20_graph_photo)
             , '660x334!') %>%
  image_morph() %>%
  image_animate(optimize = TRUE, delay = 10)

Shot_chart_progression

image_write(Shot_chart_progression, "Hardenshotprogression.gif")
