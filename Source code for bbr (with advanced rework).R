#Clear Workspace
dev.off()
cat("\014")
rm(list=ls())
set.seed(18552)

#Thank you god for this package
library(bbr)

maybe_as_numeric <- function(x) {
  # tries to make numeric columns numeric (from char)
  numeric_x <- suppressWarnings(as.numeric(x))
  if (!all(is.na(numeric_x))) x <- numeric_x
  x
}

empty_string_to_na <- function(x) {
  # sometimes (especially old datasets), empty values are ""
  if (class(x) == "character") {
    res <- ifelse(x == "", NA, x)
  } else {
    res <- x
  }
  res
}

clean_colnames <- function(df) {
  # clean up column names for a data frame
  stopifnot(is.data.frame(df))
  df <- df[!(names(df) == "Rk")] # remove "Rank" column
  names(df) <- gsub("\\.", "_pct", names(df))
  names(df) <- gsub("X2", "two_", names(df))
  names(df) <- gsub("X3", "three_", names(df))
  names(df) <- tolower(names(df))
  df
}

parse_season_table <- function(table) {
  duplicated_header_rows <- table$Rk == "Rk"
  table <- table[!duplicated_header_rows, ]
  converted <- lapply(table, maybe_as_numeric)
  converted <- lapply(converted, empty_string_to_na)
  df <- as.data.frame(converted, stringsAsFactors = FALSE)
  df <- clean_colnames(df)
  df
}

get_season_advanced <- function(year) {
  newest_year <- 1 + as.numeric(format(Sys.Date(), "%Y"))
  if (year < 1947 | year > newest_year) {
    stop("Data are only available after 1947 and up to the present.")
  }
  url <- paste0("http://www.basketball-reference.com/leagues/NBA_",
                year,
                "_advanced.html")
  
  html <- xml2::read_html(url)
  node <- rvest::html_node(html, "table")
  table <- rvest::html_table(node, header = TRUE)
  d <- parse_season_table(table)
  d$start_year <- year - 1
  d$end_year <- year
  d
}

#always exclude columns 19 and 24 from these data sets
clean_advanced <- function(advdata) {
  advdata <- advdata[, -c(19, 24)]
  return(advdata)
}


#This is the actual graphing
library(plotly)
library(crosstalk)

X2019_2020_adv_stats <- get_season_advanced(2020)
X2019_2020_adv_stats <- clean_advanced(X2019_2020_adv_stats)
#Exclude those who played less than 50 minutes
X2019_2020_adv_stats <- subset(X2019_2020_adv_stats, X2019_2020_adv_stats$mp > 50)
usgvorp2019_20 <- ggplot(data = X2019_2020_adv_stats, 
                         mapping = aes(usg_pct, vorp, text = paste('Player:', player))) + 
  geom_point() + geom_vline(xintercept = 25) + geom_hline(yintercept = 2.5)

ggplotly(usgvorp2019_20)
#This clearly works for just the 2019 season

X_2018_2019_adv_stats <- get_season_advanced(2019)
X_2018_2019_adv_stats <- clean_advanced(X_2018_2019_adv_stats)
X_2018_2019_adv_stats <- subset(X_2018_2019_adv_stats, X_2018_2019_adv_stats$mp > 50)

X2017_2018_adv_stats <- get_season_advanced(2018)
X2017_2018_adv_stats <- clean_advanced(X2017_2018_adv_stats)
X2017_2018_adv_stats <- subset(X2017_2018_adv_stats, X2017_2018_adv_stats$mp > 50)

X2016_2017_adv_stats <- get_season_advanced(2017)
X2016_2017_adv_stats <- clean_advanced(X2016_2017_adv_stats)
X2016_2017_adv_stats <- subset(X2016_2017_adv_stats, X2016_2017_adv_stats$mp > 50)

X2015_2016_adv_stats <- get_season_advanced(2016)
X2015_2016_adv_stats <- clean_advanced(X2015_2016_adv_stats)
X2015_2016_adv_stats <- subset(X2015_2016_adv_stats, X2015_2016_adv_stats$mp > 50)

X2014_2015_adv_stats <- get_season_advanced(2015)
X2014_2015_adv_stats <- clean_advanced(X2014_2015_adv_stats)
X2014_2015_adv_stats <- subset(X2014_2015_adv_stats, X2014_2015_adv_stats$mp > 50)

X2014thru2020advstats <- rbind(X2014_2015_adv_stats,X2015_2016_adv_stats, 
                               X2016_2017_adv_stats,X2017_2018_adv_stats, 
                               X_2018_2019_adv_stats,X2019_2020_adv_stats)

X2014thru2020advstatsgrouped <- group_by(X2014thru2020advstats, X2014thru2020advstats$player)
VORPvUSG2014_20 <- summarise(X2014thru2020advstatsgrouped,
                             Avg_USG_pct = mean(usg_pct),
                             Total_VORP = sum(vorp))

VORPvUSG2014_20$Total_VORP <- round(VORPvUSG2014_20$Total_VORP, 2)
VORPvUSG2014_20$Avg_USG_pct <- round(VORPvUSG2014_20$Avg_USG_pct, 2)

names(VORPvUSG2014_20)[names(VORPvUSG2014_20) == "X2014thru2020advstats$player"] <- "Player"
VORPvUSG2014_20$isithim <- ifelse(VORPvUSG2014_20$Player == "James Harden", 1, 0)
VORPvUSG2014_20$isithim <- as.factor(VORPvUSG2014_20$isithim)

VORPvUSGagg_graph <- ggplot(data = VORPvUSG2014_20, 
                         mapping = aes(Avg_USG_pct, Total_VORP, color = isithim,
                                       text = paste('Player:', Player))) + geom_point() +
  labs(x = "Average Usage Percentage", y = "Total VORP", title = "VORP v. USG% from 2014-2020") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") + 
  scale_colour_manual(values = c("black", "red"))
#If anything, change how the tooltip displays AVG USG % and Total VORP (i.e. no underscores).
#Also remove "isithim" from the tooltip

ggplotly(VORPvUSGagg_graph, tooltip = c("text", "x", "y"))

#Graph for Top 25 VORP in this span
#Do this because VORP is a cumulative statistics
TotalVORPsorted <- VORPvUSG2014_20[order(VORPvUSG2014_20$Total_VORP, decreasing = TRUE),]
Top25VORP2014_20 <- TotalVORPsorted[1:25,c("Player", "Total_VORP", "isithim")]

TotalVORPGraph <- ggplot(data = Top25VORP2014_20, mapping = aes(x = Total_VORP, 
                                              y = reorder(Player, Total_VORP), 
                                              fill = isithim,
                                              text = paste('Player:', Player))) + 
  geom_bar(stat = 'identity') +
  labs(x = "Total VORP", y = 'Player', title = "Total VORP \n from 2014/15 Season through 2019/2020 Season") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1), 
        legend.position = "none",
        axis.title.y = element_blank()) + 
  scale_fill_manual(values = c("black", "red")) +
  geom_text(mapping = aes(label = Total_VORP), colour = "white", hjust = 1.25)

TotalVORPGraph

#vjust not taken into account with plotly for some reason
ggplotly(TotalVORPGraph, tooltip = "x")

#Graph for USG rate vs true shooting percentage
USGvsTS14_20 <- summarise(X2014thru2020advstatsgrouped,
                          Avg_USG_pct = mean(usg_pct),
                          Avg_TS_pct = mean(ts_pct),
                          Total_games = sum(g))
USGvsTS14_20$Avg_USG_pct <- round(USGvsTS14_20$Avg_USG_pct, 2)
USGvsTS14_20$Avg_TS_pct <- round(USGvsTS14_20$Avg_TS_pct, 2)
names(USGvsTS14_20)[names(USGvsTS14_20) == "X2014thru2020advstats$player"] <- "Player"
#remove players with small sample size to get rid of unfairly high TS%
USGvsTS14_20thresh <- subset(USGvsTS14_20, USGvsTS14_20$Total_games > 20)
#Going to distinguish between centers and noncenters
Centers <- subset(X2014thru2020advstats, X2014thru2020advstats$pos == "C")
Noncenters <- subset(X2014thru2020advstats, X2014thru2020advstats$pos != "C")
`%ni%` = Negate(`%in%`)
#Get rid of players who also play forward
ExcCenters <- subset(Centers, Centers$player %ni% Noncenters$player)
USGvsTS14_20threshwpos <- left_join(USGvsTS14_20thresh, ExcCenters, by = c("Player" = "player"))
USGvsTS14_20threshwpos <- USGvsTS14_20threshwpos[,c(1:5)]
USGvsTS14_20threshwpos <- unique.data.frame(USGvsTS14_20threshwpos)
USGvsTS14_20threshwpos$pos <- ifelse(is.na(USGvsTS14_20threshwpos$pos), 
                                     "Not Exclusively Center", "Exclusively Center")
USGvsTS14_20threshwpos$pos <- as.factor(USGvsTS14_20threshwpos$pos)

#Where should I add the ablines?Want to do that still
USGvsTSgraph <- ggplot(data = USGvsTS14_20threshwpos, 
       mapping = aes(x = Avg_USG_pct, y = Avg_TS_pct, color = pos,
                     text = paste('Player:', Player))) + 
  geom_point() + 
  ggtitle("True Shooting Percentage vs. Usage Rate", 
          subtitle = "Based on Averaging Each Season's Value 2014-2020") +
  labs(x = "Average Usage %", y = "Average True Shooting %") +
  scale_color_discrete(name = "Position")

USGvsTSgraph

#Should I have titles that say 2014/15 - 2019/20 Instead of 2014-2020?
#Maybe fix the legend here? Looks pretty good though
ggplotly(USGvsTSgraph, tooltip = c("text", "y", "x")) %>% 
  layout(title = list(text = paste0('True Shooting Percentage vs. Usage Rate',
                                    '<br>',
                                    '<sup>',
                                    "Based on Averaging Each Season's Value 2014-2020",
                                    '</sup>'),
                      x = 10))
