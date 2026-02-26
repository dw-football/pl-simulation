
source("code/elo_calcs.R")
source("code/soccer_sim_functions.R")

# Read in a table that gives superliga teams and elo ratings
superliga_teams <- read_csv("data/superliga elo.csv")


# Can either create a blank league table here...
league_table <- data.frame(
  Team = superliga_teams$Team,
  Elo = superliga_teams$elo,
  Wins = 0,
  Draws = 0,
  Losses = 0,
  Points = 0,
  GD = 0,
  TGS = 0,
  TGC = 0,
  HGS = 0,
  HGC = 0,
  AGS = 0,
  AGC = 0
)

# ...or read csv that contains all games played so far
den_input <- read_csv("data/Denmark 2024.08.11.csv")
# and then narrow to only stats we use
den <- select(den_input, HomeTeam = Home, AwayTeam = Away, FTR = Res, 
              FTHG = HG, FTAG = AG) 
league_table <- create.league.table(den)
league_table <- league_table %>% 
  left_join(superliga_teams %>% select(Team, Elo), by = "Team")

# for a blank start of season
# rem_matches <- determine.remaining.matches(den, league_table)
# Create the set of 22 matches and expected goals from the elo ratings
# unplayed <- just.create.unplayed.matches(s_league_table)


completed <- paste(den$HomeTeam, den$AwayTeam, sep = " - ")
unplayed <- just.create.unplayed.matches(league_table, completed)
unplayed_with_EG <- add.expected.goals.from.elo(unplayed, league_table)
  
num_sims <- 5000
all_scores <- simulate.many.seasons(unplayed_with_EG, num_sims, FALSE)
all_sims <- calc.points.and.rank(all_scores, league_table, num_sims)

dt_all_sims <- as.data.table(all_sims)
dt_rank_table <- dcast(dt_all_sims, Team ~ Rank, value.var = "Rank", fun.aggregate = function(x) length(x) / num_sims)
plot.points.vs.rank("Vejle", all_sims, num_sims)
