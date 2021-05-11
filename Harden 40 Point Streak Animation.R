setwd("~/Documents/Rice_Junior_Year/Second_Semester/DSCI_304")
library(readxl)
library(ggplot2)
library(gganimate)
library(lubridate)

Streakdata <- read_xlsx("Harden 40 Point Streak.xlsx")
Streakdata$order<-seq(from=1,to=5, by=1)

#Change color of OT games
Streak_Base <- ggplot(data = Streakdata, mapping = aes(x = Date)) +
  geom_line(color="red", mapping = aes(y = Harden_Points)) + 
  geom_line(mapping = aes(y = Next_Highest_Player_Score)) + 
  geom_point(color="red", aes(y = Harden_Points)) + 
  geom_point(aes(y = Next_Highest_Player_Score)) +
  expand_limits(y = 0) +
  labs(y = "Points Scored") + 
  geom_label(mapping = aes(y = Next_Highest_Player_Score, 
                          label = paste(Next_Highest_Player, "scored", Next_Highest_Player_Score,
                                        "\n vs.", Next_Highest_Opponent,"on", Date),
                          vjust = 1.25), size = 2.5) +
  geom_label(mapping = aes(y = Harden_Points, 
                           label = paste("Harden scored", Harden_Points,
                                         "\n vs.", Opponent,"on", Date),
                           vjust = -0.25), size = 2.5) + 
  geom_hline(yintercept = 40)+
  ggtitle("James Harden's 40 Point Streak") +
  theme(plot.title = element_text(hjust = 0.5))

Streak_anim <- Streak_Base + transition_reveal(Date)

Streak_animated <- animate(Streak_anim, renderer=gifski_renderer())

Streak_animated
#Place in broader context of 30 point streak
