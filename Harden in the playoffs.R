setwd("~/Documents/Rice_Junior_Year/Second_Semester/DSCI_304")
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)

playoffstats <- read_excel("Playoff Stats.xlsx")
#again filtering out small sample sizes
playoffstats <- subset(playoffstats, playoffstats$MP > 5 & playoffstats$MP_total > 48)
playoffstatsgrouped <- group_by(playoffstats, Player)

totalgames <- summarise(playoffstatsgrouped,
                        totalgames = sum(G))

playoffstatsgrouped <- left_join(playoffstatsgrouped, totalgames, by = "Player")
#Weight each season of playoffs correctly
playoffstatsgrouped$sznweight <- (playoffstatsgrouped$G)/(playoffstatsgrouped$totalgames)
playoffstatsgrouped$PTSweightedavg <- (playoffstatsgrouped$PTS)*(playoffstatsgrouped$sznweight)
playoffstatsgrouped$TSPweightedavg <- (playoffstatsgrouped$TSP)*(playoffstatsgrouped$sznweight)


playoffimpstats <- summarise(playoffstatsgrouped,
                          totalVORP = sum(VORP),
                          avgPPG = sum(PTSweightedavg),
                          avgTSP = sum(TSPweightedavg))

playoffimpstats$avgPPG <- round(playoffimpstats$avgPPG, 1)
playoffimpstats$avgTSP <- round(playoffimpstats$avgTSP, 3)
playoffimpstats$isithim <- ifelse(playoffimpstats$Player == "James Harden", 1, 0)
playoffimpstats$isithim <- as.factor(playoffimpstats$isithim)
playoffimpstatsVORPordered <- playoffimpstats[order(-playoffimpstats$totalVORP),]
playofftop15VORP <- playoffimpstatsVORPordered[1:15,]

VORPplayoffs <- ggplot(playofftop15VORP, mapping = aes(y = reorder(Player,totalVORP), 
                                                    x = totalVORP, fill = isithim)) +
  geom_bar(stat = "identity") + 
  ggtitle("Total VORP in Playoffs from 2014-2020") +
  labs(x = "Total VORP") + 
  theme(axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = c("black", "red")) + 
  geom_text(mapping = aes(label = totalVORP), colour = "white", hjust = 1.25)

VORPplayoffs

PointsvEffPlayoffs <- ggplot(playoffimpstats, mapping = aes(y = avgTSP, x = avgPPG, text = Player, color = isithim)) +
  geom_point() + theme(legend.position = "none", plot.title = element_text(size = 10, hjust = 0.5)) +
  ggtitle("Points Per Game vs. True Shooting Percentage \n in the Playoffs 2014-2020") +
  labs(x = "PPG", y = "TS%") + scale_color_manual(values = c("black", "red"))

ggplotly(PointsvEffPlayoffs, tooltip = c("text", "x", "y"))
