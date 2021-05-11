setwd("~/Documents/Rice_Junior_Year/Second_Semester/DSCI_304/Project")
library(readxl)
library(ggplot2)

MVPvotes <- read_excel("MVP Votes.xlsx")
#Reading in all empty rows for some reason, has never happened before
MVPvotes <- MVPvotes[1:70,]
library(dplyr)
MVPvotesgrouped <- group_by(MVPvotes, MVPvotes$Player)
names(MVPvotesgrouped)[names(MVPvotesgrouped) == "Pts Won"] <- "Pts_won"
MVPtotal <- summarize(MVPvotesgrouped, totalvotes = sum(Pts_won))
names(MVPtotal)[names(MVPtotal) == "MVPvotes$Player"] <- "Player"
MVPtotal <- MVPtotal[order(-MVPtotal$totalvotes),]
MVPtotaltop10 <- MVPtotal[1:10,]
MVPtotaltop10$isithim <- ifelse(MVPtotaltop10$Player == "James Harden", 1, 0)
MVPtotaltop10$isithim <- as.factor(MVPtotaltop10$isithim)
ggplot(MVPtotaltop10, mapping = aes(y = reorder(Player, totalvotes), 
                                    x = totalvotes, fill = isithim)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Points Won in MVP Voting 2014-2020") +
  labs(x = "Total Points Won") + 
  theme(axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values = c("black", "red")) + 
  geom_text(mapping = aes(label = totalvotes), colour = "white", hjust = 1.25)
