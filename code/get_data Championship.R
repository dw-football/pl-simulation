

#################################
#    MAIN CODE ------------------
#    Data came from http://www.football-data.co.uk/englandm.php
#################################


# Read E1.csv that contains all games played so far
# setwd("C:/Users/dwarren/Google Drive/Computing/R/SoccerSimulation")
ch_input <- read_csv("data/e1.csv")
# and then narrow to only stats we use
ch <- select(ch_input, HomeTeam, AwayTeam, FTR, FTHG, FTAG) 

## Test out Big 6 head to head contests and show who reffed
# big6 <- c("Liverpool", "Man City", "Man United", "Chelsea", "Arsenal", 
#           "Tottenham")
# big6h2h = filter(pl, HomeTeam %in% big6 & AwayTeam %in% big6)
# count(big6h2h, Referee, sort = TRUE)

## Add games played but not yet in .csv file
# ## Can also add games we want to "force" to certain outcomes as scenarios
# 

league_table <- create.league.table(ch)

sorted_league_table <- create.sorted.league.table(league_table)

## graph of who's played whom and what's left
season_matrix <- ggplot(ch, aes(x=AwayTeam, y=HomeTeam, color=FTR)) +
  geom_point(size=2) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggtitle("Championship 2023-24 Results So Far") +
  scale_color_manual(name = "Result", 
                     labels = c("Away win", "Draw", "Home win"),
                     values = c("red", "black","green")) +
  labs(x = "Away Team", y = "Home Team")
season_matrix

rem_matches <- determine.remaining.matches(ch, league_table)
write.csv(league_table, file = "./data/league_table.csv")
write.csv(sorted_league_table, file = "./data/sorted_league_table.csv")
write.csv(rem_matches, file = "./data/rem_matches.csv")

