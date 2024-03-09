###
### ANALYZE THE DATA in allSims and leagueTable and allScores
###

## Read in saved data we need
## this brings in all_sims and all_scores
load("./data/sim_data.Rdata")
num_sims <- max(all_sims$SimNo)

#this brings in league_table and sorted_league_table
league_table <- read.csv("./data/league_table.csv")
sorted_league_table <- read.csv("./data/sorted_league_table.csv")

t <- create.538.table(all_sims, sorted_league_table)
print.formatted.538(t)

relegation <- create.finishing.odds.table(all_sims, 17, ">")
# For Championship
# relegation <- create.finishing.odds.table(all_sims, 21, ">")
relegation


# run this if want to permutate on a single game
# permutated_finish <- permutate.a.result(all_scores, league_table, num_sims,
#                                        "Chelsea",
#                                       "Nott'm Forest", 17, ">")
permutated_finish <- permutate.a.result(all_scores, league_table, num_sims,
                                       "Sheffield United", "Arsenal", 2, "<")
permutated_finish <- permutate.a.result(all_scores, league_table, num_sims,
                                        "Aston Villa", "Tottenham", 5, "<")
permutated_finish <- permutate.a.result(all_scores, league_table, num_sims,
                                        "Liverpool", "Man City", 2, "<")
permutated_finish <- permutate.a.result(all_scores, league_table, num_sims,
                                        "Crystal Palace", "Luton", 17, ">")


# run this if want to see how 2 teams did against each other
check.individual.game(all_scores, num_sims, "Liverpool", "Man City")
check.individual.game(all_scores, num_sims, "Man City", "Arsenal")

# run this to see odds of teams finishing ahead of each other
check.comparative.rankings(all_sims, num_sims, "Arsenal", "Tottenham")
# check.comparative.rankings(all_sims, num_sims, "Liverpool", "Brighton")
# check.comparative.rankings(all_sims, num_sims, "Southampton", "Leeds")

p <- plot.relegation.odds("West Ham", all_sims, num_sims)
p <- plot.relegation.odds("Luton", all_sims, num_sims)
p
# # pA <- plot.relegation.odds("Aston Villa", all_sims, num_sims)
# # pB <- plot.relegation.odds("Bournemouth", all_sims, num_sims)
# 

teams_to_plot <- c("Luton", "Nott'm Forest", "Everton")
# , "Nott'm Forest", "Crystal Palace", "Brentford")
# teams_to_plot <- c("West Ham", "Crystal Palace", "Bournemouth", "Wolves")
plots <- lapply(teams_to_plot, plot.relegation.odds, all_sims, num_sims)
num_teams <- length(plots)
nc <- ceiling(sqrt(num_teams))
grid.arrange(grobs=plots, ncol=nc)
gr <- arrangeGrob(grobs = plots, ncol=nc)
ggsave("relegation.png", gr, width=8, height=6, units="in")

cl_teams <- c("Newcastle", "West Ham")
top4_odds <- lapply(cl_teams, check.placement.odds, all_sims, num_sims, 4)
europa_teams <- c("West Ham", "Tottenham", "Everton", "Arsenal")
top6_odds <- lapply(europa_teams, check.placement.odds, all_sims, num_sims, 6)
top7_odds <- lapply(europa_teams, check.placement.odds, all_sims, num_sims, 7)

# teams_to_plot <- c("Newcastle", "Man United", "Tottenham", "Aston Villa",
#                   "Liverpool", "Brighton")
teams_to_plot <- c("Liverpool", "Man City", "Arsenal")
plots <- lapply(teams_to_plot, plot.points.vs.rank, all_sims, num_sims)
num_teams <- length(plots)
nc <- ceiling(sqrt(num_teams))
grid.arrange(grobs=plots, ncol=nc)
gr <- arrangeGrob(grobs = plots, ncol=nc)
ggsave("top of table.png", gr, width=8, height=6, units="in")

plot.points.vs.rank("Liverpool", all_sims, num_sims)
plot.points.vs.rank("Man City", all_sims, num_sims)
plot.points.vs.rank("Arsenal", all_sims, num_sims)
plot.points.vs.rank("Brighton", all_sims, num_sims)
plot.points.vs.rank("Chelsea", all_sims, num_sims)

plot.points.vs.rank("Tottenham", all_sims, num_sims)
plot.points.vs.rank("West Ham", all_sims, num_sims)

# # each team's probabilities per position
table(all_sims$Team, all_sims$Rank)/num_sims

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
