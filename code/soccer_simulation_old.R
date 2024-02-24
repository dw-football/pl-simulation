# need library for data frames and piping
library(dplyr)
library(tidyverse)
library(scales)
library(ggplot2)
library(gridExtra)


###
# Create.League.Table()
###
create.league.table <- function(league) {
  teams = unique(c(league$HomeTeam, league$AwayTeam)) %>% sort
  
  # homeTable is league table of home teams
  homeTable <- league %>%
    group_by(HomeTeam) %>%
    summarise(P = length(FTR),
              Wins = sum(FTHG > FTAG),
              Losses = sum(FTHG < FTAG),
              Draws = sum(FTHG == FTAG),
              Pts = sum((FTHG > FTAG) * 3 + (FTHG == FTAG) * 1),
              GS = sum(FTHG),
              GC = sum(FTAG)) %>%
    ungroup()
  
  # awayTable is league table of away teams
  awayTable <- league %>%
    group_by(AwayTeam) %>%
    summarise(P = length(FTR),
              Wins = sum(FTHG < FTAG),
              Losses = sum(FTHG > FTAG),
              Draws = sum(FTHG == FTAG),
              Pts = sum((FTHG < FTAG) * 3 + (FTHG == FTAG) * 1),
              GS = sum(FTAG),
              GC = sum(FTHG)) %>%
    ungroup()
  
  #Create a data frame of full league table
  leagueTable <- data.frame(Team = teams,
                            Wins =  homeTable$Wins + awayTable$Wins,
                            Draws = homeTable$Draws + awayTable$Draws,
                            Losses = homeTable$Losses + awayTable$Losses,
                            Points = homeTable$Pts + awayTable$Pts,
                            GD = (homeTable$GS + awayTable$GS) - (homeTable$GC + awayTable$GC),
                            TGS = (homeTable$GS + awayTable$GS) / (homeTable$P + awayTable$P),
                            TGC = (homeTable$GC + awayTable$GC) / (homeTable$P + awayTable$P), 
                            HGS = homeTable$GS / homeTable$P,
                            HGC = homeTable$GC / homeTable$P,
                            AGS = awayTable$GS / awayTable$P,
                            AGC = awayTable$GC / awayTable$P,
                            stringsAsFactors = FALSE)
}


###
# Create.Sorted.League.Table() 
###
create.sorted.league.table <- function(leagueTable) {
  # sort by points (need to reverse order here for traditional view)
  sortedLeagueTable <- 
    leagueTable[order(-leagueTable$Points, -leagueTable$GD), ] %>% 
    mutate(Played = Wins + Draws + Losses,
           Scored = TGS * Played,
           Conceded = TGC * Played,) %>%
    select(Team, Played, Wins, Draws, Losses, 
           Points, GD, Scored, Conceded,
           HGS, HGC, AGS, AGC)
}


determine.remaining.matches <- function(league, leagueTable) {
  # create list of Matches already played in complMatch
  complMatch = paste(league$HomeTeam, league$AwayTeam, sep = " - ")
  # and alphabetic list of teams in the league
# teams <- unique(c(league$HomeTeam,league$AwayTeam)) %>% sort
  teams <- sort(leagueTable$Team)
 
  # create list of remaining games
  # this one creates a kind of 'expected goals' by game for use in simulation
  # In past simulations (done in Excel), I used betting odds instead of EG
  # Is there a way to look those up? ****
  remMatches <- expand.grid(HomeTeam = teams, AwayTeam = teams, 
                            stringsAsFactors = FALSE) %>%
    filter(HomeTeam != AwayTeam) %>%
    mutate(Match = paste(HomeTeam, AwayTeam, sep = " - ")) %>%
    filter(!(Match %in% complMatch)) %>%
    select(-Match) %>%
    mutate(HG = mean(league$FTHG),
           AG = mean(league$FTAG),
           TG = (mean(league$FTHG) + mean(league$FTAG)) / 2) %>%
    right_join(subset(leagueTable, select = -c(Wins, Draws, Losses, Points, GD, TGS, TGC, AGS, AGC)), 
               by = c("HomeTeam" = "Team")) %>%
    right_join(subset(leagueTable, select = -c(Wins, Draws, Losses, Points, GD, TGS, TGC, HGS, HGC)), 
               by = c("AwayTeam" = "Team")) %>%
    setNames(c("HomeTeam", "AwayTeam", "HG", "AG", "TG",
               "GS.by.H", "GC.by.H", "GS.by.A", "GC.by.A")) %>%
# old calc
    #    mutate(ExpHG = (GS.by.H / TG) * (GC.by.A / TG) * (HG / TG) * TG,
#           ExpAG = (GS.by.A / TG) * (GC.by.H / TG) * (AG / TG) * TG) %>%
    mutate(ExpHG = (GS.by.H / HG) * GC.by.A,
           ExpAG = (GS.by.A / AG) * GC.by.H) %>%
    ungroup()
}


####
# SIMULATE SEASON Function -----------
# This is the function that simulates a season numSims times
####
simulate.season <- function(remMatches, 
                            leagueTable, 
                            numSims = 100, 
                            T1 = NULL, 
                            T2 = NULL,
                            permutate = FALSE) {

  # and alphabetic list of teams in the league
#  teams <- unique(c(league$HomeTeam,league$AwayTeam)) %>% sort
  teams <- sort(leagueTable$Team)
  n <- length(teams)
  numDraws <- 0
  gamesRemaining <- nrow(remMatches)
  
  # collect the results here
#  ifelse (permutate,
#          allSims <- data.frame(Team = rep(teams, numSims),
#                                SimNo = rep(1:numSims, each = n),
#                                Pts = rep(NA, n * numSims), #will also be home team win
#                                GD = rep(NA, n * numSims),
#                                Rank = rep(NA, n * numSims),
#                                PtsD = rep(NA, n * numSims),
#                                GDD = rep(NA, n * numSims),
#                                RankD = rep(NA, n * numSims),
#                                PtsA = rep(NA, n * numSims),
#                                GDA = rep(NA, n * numSims),
#                                RankA = rep(NA, n * numSims)),
          allSims <- data.frame(Team = rep(teams, numSims),
                                SimNo = rep(1:numSims, each = n),
                                Pts = rep(NA, n * numSims),
                                GD = rep(NA, n * numSims),
                                Rank = rep(NA, n * numSims))
          
  pb <- winProgressBar(title = "Running Simulations", 
                       label = "Simulating ... 0% done", 
                       min = 0, 
                       max = numSims, 
                       initial = 0)
  
  T1Wins <- 0
  Draws <- 0
  T2Wins <- 0
  T1Goals <- 0
  T2Goals <- 0
  T1Above <- 0
  T2Above <- 0
  
  for (i in 1:numSims){
    
    ## *** Would have to add code within the simulation to check things like
    ## Did Arenal finish ahead of Spurs?
    ## or also, what were odds of team A beating team B?
    ##  
    tmp <- remMatches %>% 
      mutate(x1 = rpois(gamesRemaining, lambda = remMatches$ExpHG), 
             x2 = rpois(gamesRemaining, lambda = remMatches$ExpAG), 
             HPts = 3 * (x1 > x2) + 1 * (x1 == x2),
             APts = 3 * (x1 < x2) + 1 * (x1 == x2),
             Draws = (x1 == x2))
    
    ## find a match in the remaining matches
    if (!is.null(T1)) {
      game <- filter(tmp, HomeTeam == T1 & AwayTeam == T2)
      gameNo <- which(tmp$HomeTeam == T1 & tmp$AwayTeam == T2)
#      if (i==1) print(paste("Game #", gameNo, ":", i, T1, game$x1, T2, game$x2))
      
      T1Goals = T1Goals + game$x1
      T2Goals <- T2Goals + game$x2
      ifelse(game$x1 > game$x2, T1Wins <- T1Wins + 1,
             ifelse(game$x1 < game$x2, 
                    T2Wins <- T2Wins + 1,
                    Draws <- Draws + 1))
    }

    ## just here to check on if simulation is creating appropriate # of Draws  
    numDraws <- numDraws + sum(tmp$Draws)
    
    res <- leagueTable %>% 
      select(Points, GD) + tmp %>% 
      group_by(HomeTeam) %>% 
      summarise(Pts = sum(HPts), GD = sum(x1) - sum(x2)) %>% 
      select(Pts, GD) + tmp %>% 
      group_by(AwayTeam) %>% 
      summarise(Pts = sum(APts), GD = sum(x2) - sum(x1)) %>% 
      select(Pts, GD) 
    
    allSims[(n * (i-1) + 1):(n * i), c("Pts", "GD")] <- res
  
    res$PGD <- res$Points + (res$GD - min(res$GD) + 1) / 
                             max((res$GD - min(res$GD) + 1) + 1)
    ranking <- rank(-res$PGD, ties.method = "random")
    allSims[(n * (i-1) + 1):(n * i), c("Rank")] <- ranking
    
    if(!is.null(T1)) {
      T1Row = which(teams == T1)
      T2Row = which(teams == T2)
      ifelse(ranking[T1Row] < ranking[T2Row], 
             T1Above <- T1Above + 1,
             T2Above <- T2Above + 1)
    }
    
    info <- sprintf("Simulating ... %d%% done", round((i/numSims)*100))
    setWinProgressBar(pb, i, label = info)  
  }

  close(pb)
  if (!is.null(T1)) {
    print(paste(T1, T1Wins, T2, T2Wins, "Draws", Draws))
    print(paste("Goals:", T1, T1Goals, T2, T2Goals))
    print(paste(T1, "finishes above", T1Above, T2, T2Above))
  }
  print(paste("Draw % for all teams", numDraws / numSims / gamesRemaining))
  return (allSims)
}


#######################
# Create Finishing Odds function ---------
#
# returns sorted filtered list of teams above, below, or at a certain league position
#######################
create.finishing.odds.table <- function(allSims, placement, operator) {
  ifelse(operator == "==",
         rt <- filter(allSims, Rank == placement),
         ifelse (operator == "<",
                 rt <- filter(allSims, Rank < placement),
                 rt <- filter(allSims, Rank > placement))
  )
  rt <- rt %>% 
    group_by(Team) %>% 
    summarize(Count = n(), Percent= Count / iSim * 100) %>% 
    arrange(desc(Count))
}


#######################
# Plot Relegation Odds  ---------
#
# plots a team's points & relegations possibilities
#######################
plot.relegation.odds <- function(team, allSims, numSims) {
  # placement for one team
  # allSims %>% filter(Team == team) %>% select(Rank) %>% table/iSim
  # points distribution for one team
  # allSims %>% filter(Team == team) %>% select(Pts) %>% table/iSim
  teamSim <- allSims %>% filter(Team == team) %>% mutate(relegated = Rank > 17)
  teamResultsTable <- teamSim %>% 
    group_by(Pts) %>% 
    summarize(PtsLikelihood = n() / numSims, RelLikelihood = sum(relegated) / n())
  # Show team's average points
  teamMeanPts = mean(teamSim$Pts)
  # Show team's odds of relegation
  teamRelegationOdds = sum(teamSim$relegated) / numSims * 100
  # Show reults in graph
  teamResultsTable <- mutate(teamResultsTable, 
                             NonRelOdds = PtsLikelihood * (1-RelLikelihood))
  g <- ggplot(teamResultsTable, aes(x = Pts)) +
    geom_col(aes(y=PtsLikelihood, fill = "PtsLikelihood")) +
    geom_col(aes(y=NonRelOdds, fill = "NonRelOdds")) +
    labs(title = paste(team, "Final Placement Distribution"),
         subtitle = paste(teamMeanPts, "mean Points;", teamRelegationOdds, 
                          "% odds of relegation under", iSim, "seasons simulation"),
         caption = "Source:  dw simulation model") +
    scale_fill_manual(name="",
                      labels = c("Staying Up in %", "Relegation in %"),
                      values = c("NonRelOdds"="Green", "PtsLikelihood"="Red")) +
    scale_x_continuous("Points in Final Table",
                       breaks = seq(min(teamResultsTable$Pts),max(teamResultsTable$Pts), 
                                    by = 1)) +
    scale_y_continuous(labels = percent_format(accuracy=1),
                       breaks = seq(0, max(teamResultsTable$PtsLikelihood), by = 0.02)) +
    theme(panel.grid.minor = element_blank(), legend.position = "none",
          axis.text.x = element_text(color = "grey20", size = 8, face = "plain"))
        
  return(g)
}



#######################
# Plot Points vs Rank  ---------
#
# plots a team's points & ranking
#######################
plot.points.vs.rank <- function(team, allSims, numSims) {
  teamSim <- allSims %>% filter(Team == team) 
  teamResultsByPointsOnly <- 
    teamSim %>% 
    group_by(Pts) %>% 
    summarize(PtsLikelihood = n()/numSims)
  teamResultsTable <- 
    teamSim %>% 
    group_by(Pts, Rank) %>% 
    summarize(PtsLikelihood = n()/numSims)
  teamMeanPts = mean(teamSim$Pts)
  g <- ggplot(teamResultsTable, aes(x=Pts, y=PtsLikelihood, fill=Rank)) +
    geom_col() +
    geom_text(aes(label = Rank),
              position = position_stack(vjust=.5),
              size = 2) +
    labs(title = paste(team, "Final Placement Distribution"),
         subtitle = paste(teamMeanPts, "mean Points;",  
                          "under",iSim, "seasons simulation"),
         caption = "Source:  dw simulation model") +
    #    scale_fill_manual(name="",
    #                      labels = c("Staying Up in %", "Relegation in %"),
    #                      values = c("NonRelOdds"="Green", "PtsLikelihood"="Red")) +
    #    scale_fill_gradientn(colours = c("green", "yellow","red"),
    #                         limits=c(1,20)) +
    scale_fill_gradientn(colours = c("green", "yellow", "orange", "red"),
                         limits = c(1, 20)) +
    #     scale_fill_continuous(type = "viridis") +
    # geom_smooth(aes(color = Rank, fill = Rank), method = "lm") + 
    # scale_color_viridis(discrete = TRUE, option = "D")+
    # scale_fill_viridis(discrete = TRU1) +
    scale_x_continuous("Points in Final Table",
                       breaks = seq(min(teamResultsTable$Pts), 
                                    max(teamResultsTable$Pts), 
                                    by = 1)) +
    scale_y_continuous(labels = percent_format(accuracy=1),
                       breaks = seq(0, max(teamResultsByPointsOnly$PtsLikelihood), by = 0.02)) +
    theme(panel.grid.minor = element_blank(), legend.position = "none",
          axis.text.x = element_text(color = "grey20", size = 8, face = "plain"))
  
  return(g)
}


################################
#  Add.Game function ------------
#  Add a game not yet in the downloaded file to the league results list
#################################
add.game <- function(league, homeTeam, homeGoals, awayTeam, awayGoals) {
  ifelse (homeGoals > awayGoals, result <- "H",
          ifelse (homeGoals == awayGoals, result <- "D",
                  result <- "A"))
  league <- add_row(league, HomeTeam = homeTeam, AwayTeam = awayTeam,
                    FTR = result, FTHG = homeGoals, FTAG = awayGoals)
}


#################################
#    MAIN CODE ------------------
#    Data came from http://www.football-data.co.uk/
#################################

# Read E0.csv that contains all games played so far
plInput <- read_csv("e0.csv")
# and then narrow to only stats we use
pl <- select(plInput, HomeTeam, AwayTeam, FTR, FTHG, FTAG) 

## Test out Big 6 head to head contests and show who reffed
# big6 <- c("Liverpool", "Man City", "Man United", "Chelsea", "Arsenal", "Tottenham")
# 
# big6h2h = filter(pl, HomeTeam %in% big6 & AwayTeam %in% big6)
# count(big6h2h,Referee, sort = TRUE)

# set # of simulations
iSim <- 5000

# Add games played but not yet in .csv file
# Can also add games we want to "force" to certain outcomes as scenarios
#
# Monday's game
pl <- add.game(pl, "Man City", 5, "Burnley", 0)
pl <- add.game(pl, "Leicester", 0, "Brighton", 0)
pl <- add.game(pl, "Tottenham", 2, "West Ham", 0)

pl <- add.game(pl, "Man United", 3, "Sheffield United", 0)
pl <- add.game(pl, "Newcastle", 1, "Aston Villa", 1)
pl <- add.game(pl, "Norwich", 0, "Everton", 1)
pl <- add.game(pl, "Wolves", 1, "Bournemouth", 0)
pl <- add.game(pl, "Liverpool", 4, "Crystal Palace", 0)

pl <- add.game(pl, "Burnley", 1, "Watford", 0)
pl <- add.game(pl, "Southampton", 0, "Arsenal", 2)
pl <- add.game(pl, "Chelsea", 2, "Man City", 1)

# next week
# pl <- add.game(pl, "West Ham", 0, "Chelsea", 0)


leagueTable <- create.league.table(pl)
sortedLeagueTable <- create.sorted.league.table(leagueTable)
view(sortedLeagueTable)

## graph of who's played whom and what's left
seasonMatrix <- ggplot(pl, aes(x=AwayTeam, y=HomeTeam, color=FTR)) +
  geom_point(size=2) +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Premier League 2019-20 Results So Far") +
  scale_color_manual(name = "Result", 
                     labels = c("Away win", "Draw", "Home win"),
                     values = c("red", "black","green")) +
  labs(x = "Away Team", y = "Home Team")

remMatches <- determine.remaining.matches(pl, leagueTable)

set.seed(1234)
# allSims <- simulate.season(remMatches, leagueTable, iSim)
allSims <- simulate.season(remMatches, leagueTable, iSim, "West Ham", "Chelsea")

# # # # Example of simulating 3 times for W, L, D of certain game
# pl = add.game(pl, "Tottenham", 0, "West Ham", 1)
# gamesPlayed = nrow(pl)
# set.seed(1234)
# df.win <- simulate.season(remMatches, leagueTable, iSim)
# 
# pl[gamesPlayed, 3] <- "H"
# pl[gamesPlayed, 4] <- 1
# pl[gamesPlayed, 5] <- 0
# set.seed(1234)
# df.lose <- simulate.season(remMatches, leagueTable, iSim)
# 
# pl[gamesPlayed, 3] = "D"
# pl[gamesPlayed, 4] = 1
# pl[gamesPlayed, 5] = 1
# set.seed(1234)
# df.draw <- simulate.season(remMatches, leagueTable, iSim)
# look at some stats

# Odds of relegation by team
relegation <- create.finishing.odds.table(allSims, 17, ">")
relegation

## to plot relegation over time; need to build different function
## ggplot(rel, aes(x = Gameweek, y = Percent, color = Team)) + 
##    geom_text_repel(aes(label=Team),color = "black", size=3) + 
##    scale_x_discrete(breaks = c("28.9","29"), labels = c("28.9","29"))

# odds of league winner
# allSims %>% filter(Rank == 1) %>% select(Team) %>% table/iSim
winner <- create.finishing.odds.table(allSims, 1, "==")
winner

# odds of top 4, 5
# allSims %>% filter(Rank < 5) %>% select(Team) %>% table/iSim
top4 <- create.finishing.odds.table(allSims, 5, "<")
top5 <- create.finishing.odds.table(allSims, 6, "<")
top4
top5
top8 <- create.finishing.odds.table(allSims, 8, "<")

p <- plot.relegation.odds("West Ham", allSims, iSim)
p
pA <- plot.relegation.odds("Aston Villa", allSims, iSim)
pB <- plot.relegation.odds("Bournemouth", allSims, iSim)

# p <- lapply(plot.relegation.odds, relegation$Team, allSims, iSim)
# each team's probabilities per position
# table(allSims$Team, allSims$Rank)/iSim

###
###
### Other things I'd like to be able to do:
### 1)
### set/change odds of individual games
### and rerun simulation
### 2) 
### calculate game 'importance' (by running simulation if team wins/draws/loses 
### affecting ultimate standing 1-4, 5-7, 17/18)