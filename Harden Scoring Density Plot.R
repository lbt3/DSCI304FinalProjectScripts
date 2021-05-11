#Can't use bbr because the game logs are not html for some reason
setwd("~/Documents/Rice_Junior_Year/Second_Semester/DSCI_304")

library(readxl)
library(ggplot2)
library(dplyr)

HardenGames <- read_excel("Gamelogs Harden.xlsx")
HardenGamesplayed <- subset(HardenGames, is.na(HardenGames$MP) == FALSE)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#no home/away splits
DensityNoSplits <- ggplot(data = HardenGamesplayed, mapping = aes(x = PTS)) +
  geom_density(fill = "red") + expand_limits(x = 0, y = 0) +
  geom_vline(xintercept = mean(HardenGamesplayed$PTS), linetype = "dashed") +
  geom_text(aes(x = mean(PTS), label = "Average PPG", y = 0.04), angle = 270, 
            vjust = -0.5, hjust = -1) + 
  ggtitle("Density Plot of James Harden's Points in Every Game from 2014-2020") + 
  labs(x = "Points", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept = getmode(PTS)), linetype = "dashed") +
  geom_text(aes(x = getmode(PTS), label = "Mode", y = 0.04), angle = 270, 
            vjust = -0.5, hjust = -1)

DensityNoSplits
