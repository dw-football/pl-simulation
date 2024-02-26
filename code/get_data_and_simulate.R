

#################################
#    MAIN CODE ------------------
#    Data came from http://www.football-data.co.uk/englandm.php
#################################

# Read E0.csv that contains all games played so far
# setwd("C:/Users/dwarren/Google Drive/Computing/R/SoccerSimulation")
plInput <- read_csv("data/e0.csv")
# and then narrow to only stats we use
pl <- select(plInput, HomeTeam, AwayTeam, FTR, FTHG, FTAG) 

## Test out Big 6 head to head contests and show who reffed
# big6 <- c("Liverpool", "Man City", "Man United", "Chelsea", "Arsenal", 
#           "Tottenham")
# big6h2h = filter(pl, HomeTeam %in% big6 & AwayTeam %in% big6)
# count(big6h2h, Referee, sort = TRUE)

# set # of simulations
iSim <- 5000

## Add games played but not yet in .csv file
## Can also add games we want to "force" to certain outcomes as scenarios
pl <- add.game(pl, "Aston Villa", 4, "Nott'm Forest", 2)
pl <- add.game(pl, "Brighton", 1, "Everton", 1)
pl <- add.game(pl, "Crystal Palace", 3, "Burnley", 0)
pl <- add.game(pl, "Man United", 1, "Fulham", 2)
pl <- add.game(pl, "Bournemouth", 0, "Man City", 1)
pl <- add.game(pl, "Arsenal", 4, "Newcastle", 1)

leagueTable <- create.league.table(pl)

# Remove 10 points for Everton!
leagueTable$Points[leagueTable$Team == "Everton"] <- 
  leagueTable$Points[leagueTable$Team == "Everton"] - 10

sortedLeagueTable <- create.sorted.league.table(leagueTable)

## graph of who's played whom and what's left
seasonMatrix <- ggplot(pl, aes(x=AwayTeam, y=HomeTeam, color=FTR)) +
  geom_point(size=2) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggtitle("Premier League 2022-23 Results So Far") +
  scale_color_manual(name = "Result", 
                     labels = c("Away win", "Draw", "Home win"),
                     values = c("red", "black","green")) +
  labs(x = "Away Team", y = "Home Team")
seasonMatrix

remMatches <- determine.remaining.matches(pl, leagueTable)

## The neutralize flag, if turned on, allows me to change the model
## such that every team has same chance of beating, drawing, or losing
## to every other.  It's a good model check and shows (by comparison)
## the impact of prior results on the model
allScores <- simulate.many.seasons(remMatches, iSim, neutralize=FALSE)
# allScores <- simulate.many.seasons(remMatches, iSim, neutralize=TRUE)
allSims <- calc.points.and.rank(allScores, leagueTable, iSim)

relegation <- create.finishing.odds.table(allSims, 17, ">")
# For Championship
# relegation <- create.finishing.odds.table(allSims, 21, ">")
relegation

# run this if want to permutate on a single game
# permutatedFinish <- permutate.a.result(allScores, leagueTable, iSim, "Chelsea",
#                                       "Nott'm Forest", 17, ">")
permutatedFinish <- permutate.a.result(allScores, leagueTable, iSim,
                                      "Liverpool", "Man City", 2, "<")
permutatedFinish <- permutate.a.result(allScores, leagueTable, iSim,
                                       "Man City", "Arsenal", 2, "<")

# run this if want to see how 2 teams did against each other
check.individual.game(allScores, iSim, "Liverpool", "Man City")
check.individual.game(allScores, iSim, "Man City", "Arsenal")

# run this to see odds of teams finishing ahead of each other
check.comparative.rankings(allSims, iSim, "Arsenal", "Tottenham")
# check.comparative.rankings(allSims, iSim, "Liverpool", "Brighton")
# check.comparative.rankings(allSims, iSim, "Southampton", "Leeds")

t <- create.538.table(allSims, sortedLeagueTable)
p <- print.formatted.538(t)
p

p <- plot.relegation.odds("West Ham", allSims, iSim)
p <- plot.relegation.odds("Everton", allSims, iSim)
p
# # pA <- plot.relegation.odds("Aston Villa", allSims, iSim)
# # pB <- plot.relegation.odds("Bournemouth", allSims, iSim)
# 
teams_to_plot <- c("Everton", "Leicester", "Leeds")
# teams_to_plot <- c("West Ham", "Crystal Palace", "Bournemouth", "Wolves")
plots <- lapply(teams_to_plot, plot.relegation.odds, allSims, iSim)
nTeams <- length(plots)
nc <- ceiling(sqrt(nTeams))
grid.arrange(grobs=plots, ncol=nc)
gr <- arrangeGrob(grobs = plots, ncol=nc)
ggsave("relegation.png", gr, width=8, height=6, units="in")

cl_teams <- c("Newcastle", "Man United", "Tottenham", "Aston Villa", "Liverpool",
              "Brighton")
top4_odds <- lapply(cl_teams, check.placement.odds, allSims, iSim, 4)
europa_teams <- c("West Ham", "Tottenham", "Everton", "Arsenal")
top6_odds <- lapply(europa_teams, check.placement.odds, allSims, iSim, 6)
top7_odds <- lapply(europa_teams, check.placement.odds, allSims, iSim, 7)

# teams_to_plot <- c("Newcastle", "Man United", "Tottenham", "Aston Villa",
#                   "Liverpool", "Brighton")
teams_to_plot <- c("Tottenham", "Liverpool", "Brighton")
plots <- lapply(teams_to_plot, plot.points.vs.rank, allSims, iSim)
nTeams <- length(plots)
nc <- ceiling(sqrt(nTeams))
grid.arrange(grobs=plots, ncol=nc)
gr <- arrangeGrob(grobs = plots, ncol=nc)
ggsave("top of table.png", gr, width=8, height=6, units="in")

plot.points.vs.rank("Liverpool", allSims, iSim)
plot.points.vs.rank("Tottenham", allSims, iSim)
plot.points.vs.rank("West Ham", allSims, iSim)

# # each team's probabilities per position
table(allSims$Team, allSims$Rank)/iSim

## to plot relegation over time; need to build different function
## ggplot(rel, aes(x = Gameweek, y = Percent, color = Team)) + 
##    geom_text_repel(aes(label=Team),color = "black", size=3) + 
##    scale_x_discrete(breaks = c("28.9","29"), labels = c("28.9","29"))


###
###
### Other things I'd like to be able to do:
### 1)
### set/change odds of individual games
### and rerun simulation
### 2) 
### calculate game 'importance' (by running simulation if team wins/draws/loses 
### affecting ultimate standing 1-4, 5-7, 17/18)
