

#################################
#    MAIN CODE ------------------
#    Data came from http://www.football-data.co.uk/englandm.php
#################################


# Read E0.csv that contains all games played so far
# setwd("C:/Users/dwarren/Google Drive/Computing/R/SoccerSimulation")
pl_input <- read_csv("data/e0.csv")
# and then narrow to only stats we use
pl <- select(pl_input, HomeTeam, AwayTeam, FTR, FTHG, FTAG) 

## Test out Big 6 head to head contests and show who reffed
# big6 <- c("Liverpool", "Man City", "Man United", "Chelsea", "Arsenal", 
#           "Tottenham")
# big6h2h = filter(pl, HomeTeam %in% big6 & AwayTeam %in% big6)
# count(big6h2h, Referee, sort = TRUE)

## Add games played but not yet in .csv file
# ## Can also add games we want to "force" to certain outcomes as scenarios
# 

## SATURDAY
# pl <- add.game(pl, "Fulham", 0, "Man City", 4)
# pl <- add.game(pl, "Bournemouth", 1, "Brentford", 2)	
# pl <- add.game(pl, "Everton", 1, "Sheffield United", 0)
# pl <- add.game(pl, "Newcastle", 1, "Brighton", 1)
# pl <- add.game(pl, "Tottenham", 2, "Burnley", 1)
# pl <- add.game(pl, "West Ham", 3, "Luton", 1)
# pl <- add.game(pl, "Wolves", 1, "Crystal Palace", 3)
# pl <- add.game(pl, "Nott'm Forest", 2, "Chelsea", 3)

## SUNDAY
# pl <- add.game(pl, "Man United", 0, "Arsenal", 1)
# 
## MONDAY
# pl <- add.game(pl, "Aston Villa", 3, "Liverpool", 3)
# 
## TUESDAY
# pl <- add.game(pl, "Tottenham", 0, "Man City", 2)
# 
## WEDNESDAY
# pl <- add.game(pl, "Brighton", 1, "Chelsea", 2)
# pl <- add.game(pl, "Man United", 3, "Newcastle", 2)

league_table <- create.league.table(pl)

# Remove 8 points for Everton!
#league_table$Points[league_table$Team == "Everton"] <- 
#  league_table$Points[league_table$Team == "Everton"] - 8
# Remove 4 points for Forest!
#league_table$Points[league_table$Team == "Nott'm Forest"] <- 
#  league_table$Points[league_table$Team == "Nott'm Forest"] - 4

sorted_league_table <- create.sorted.league.table(league_table)

## graph of who's played whom and what's left
season_matrix <- ggplot(pl, aes(x=AwayTeam, y=HomeTeam, color=FTR)) +
  geom_point(size=2) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggtitle("Premier League 2023-24 Results So Far") +
  scale_color_manual(name = "Result", 
                     labels = c("Away win", "Draw", "Home win"),
                     values = c("red", "black","green")) +
  labs(x = "Away Team", y = "Home Team")
season_matrix

rem_matches <- determine.remaining.matches(pl, league_table)
write.csv(league_table, file = "./data/league_table.csv")
write.csv(sorted_league_table, file = "./data/sorted_league_table.csv")
write.csv(rem_matches, file = "./data/rem_matches.csv")

