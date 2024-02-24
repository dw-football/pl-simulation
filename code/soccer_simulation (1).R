# need library for data frames and piping
library(dplyr)
library(tidyverse)
library(reshape2)
library(scales)
library(ggplot2)
library(gridExtra)
library(formattable)
library(htmltools)
library(webshot)

options(dplyr.summarise.inform = FALSE)

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
                            TGS = (homeTable$GS + awayTable$GS), # / (homeTable$P + awayTable$P),
                            TGC = (homeTable$GC + awayTable$GC), # / (homeTable$P + awayTable$P), 
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
  leagueTable %>% mutate(Played = Wins + Draws + Losses,
                         Scored = TGS,
                         Conceded = TGC) %>%
    arrange(-Points, -GD, -Scored) %>%
    #  browser()
    #  slt <- slt[order(-slt$Points, -slt$GD, -slt$Scored)]  
    #  browser()
    select(Team, Played, Wins, Draws, Losses, 
           Points, GD, Scored, Conceded,
           HGS, HGC, AGS, AGC)
}


determine.remaining.matches <- function(league, leagueTable) {
  # create list of Matches already played in complMatch
  complMatch = paste(league$HomeTeam, league$AwayTeam, sep = " - ")
#  browser()
  # and alphabetic list of teams in the league
# teams <- unique(c(league$HomeTeam,league$AwayTeam)) %>% sort
  teams <- sort(leagueTable$Team)
  numTeams <- length(teams)
  numMatches <- numTeams * (numTeams - 1)
  print(paste("numteams = ", numTeams, "numMatches = ", numMatches))
 
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
    inner_join(subset(leagueTable, select = -c(Wins, Draws, Losses, Points, GD, TGS, TGC, AGS, AGC)), 
               by = c("HomeTeam" = "Team")) %>%
    inner_join(subset(leagueTable, select = -c(Wins, Draws, Losses, Points, GD, TGS, TGC, HGS, HGC)), 
               by = c("AwayTeam" = "Team")) %>%
    setNames(c("HomeTeam", "AwayTeam", "HG", "AG", "TG",
               "GS.by.H", "GC.by.H", "GS.by.A", "GC.by.A")) %>%
# old calc
    #    mutate(ExpHG = (GS.by.H / TG) * (GC.by.A / TG) * (HG / TG) * TG,
#           ExpAG = (GS.by.A / TG) * (GC.by.H / TG) * (AG / TG) * TG) %>%
    mutate(ExpHG = (GS.by.H / HG) * GC.by.A,
           ExpAG = (GS.by.A / AG) * GC.by.H) %>%
    ungroup()

  numRemMatches = nrow(remMatches)
  print(paste("numRemMatches =", numRemMatches))
  remMatches %>% mutate(MatchNo = row_number() + numMatches - numRemMatches) %>%
    select(MatchNo, everything())
}


summmarize.one.season.results <- function(leagueTable, scores) {
  soFar <- leagueTable %>% select(Team, Points, TGS, TGC)
  HomeNew <- scores %>% 
    group_by(HomeTeam) %>%
    summarise(Points = 3 * sum(HG > AG) + 1 * sum(HG == AG), 
              TGS = sum(HG),
              TGC = sum(AG)) %>%
    rename(Team = HomeTeam)
  AwayNew <- scores %>%
    group_by(AwayTeam) %>% 
    summarise(Points = 3 * sum(AG > HG) + 1 * sum(HG == AG), 
              TGS = sum(AG),
              TGC = sum(HG)) %>%
    rename(Team = AwayTeam)
  res <- bind_rows(soFar, bind_rows(HomeNew, AwayNew)) %>%
    group_by(Team) %>%
    summarise(Pts = sum(Points), GS = sum(TGS), GC = sum(TGC)) %>%
    ungroup()
  
  # res <- leagueTable %>% 
  #   select(Points, TGS, TGC) + scores %>% 
  #   group_by(HomeTeam) %>% 
  #   summarise(Pts = 3 * sum(HG > AG) + 1 * sum(HG == AG), 
  #             GS = sum(HG),
  #             GC = sum(AG)) %>% 
  #   select(Pts, GS, GC) + scores %>% 
  #   group_by(AwayTeam) %>% 
  #   summarise(Pts = 3 * sum(AG > HG) + 1 * sum(HG == AG), 
  #             GS = sum(AG),
  #             GC = sum(HG)) %>% 
  #   select(Pts, GS, GC)

  res$GD <- res$GS - res$GC
  res <- arrange(res, Pts, GD) %>% 
    mutate(Rank=n():1) %>% 
    arrange(Team) %>%
    select(Pts, GD, GS, GC, Rank)
#
#  res$PGD <- res$Points + (res$GD - min(res$GD) + 1) /
#    max((res$GD - min(res$GD) + 1) + 1)
#  res$Rank <-rank(-res$PGD, ties.method = "random") # rewrite this for speed?
#  res <- select(res, Pts, GD, GS, GC, Rank) 
}

simulate.one.season <-function(remMatches, neutralize=FALSE) {
  if (neutralize) {
    homeExpGoals <- 1
    awayExpGoals <- 1
  } else {
    homeExpGoals <- remMatches$ExpHG
    awayExpGoals <- remMatches$ExpAG
  }
  
  nGamesRem <- nrow(remMatches)
  scores <- remMatches %>% 
      mutate(HG = rpois(nGamesRem, lambda = homeExpGoals), 
             AG = rpois(nGamesRem, lambda = awayExpGoals)) 
             # HPts = 3 * (HG > AG) + 1 * (HG == AG),
             # APts = 3 * (HG < AG) + 1 * (HG == AG),
             # Draws = (HG == AG))
}

simulate.many.seasons <- function(remMatches,
                                  numSims = 100,
                                  neutralize = FALSE)
{
  n <- nrow(remMatches) # do we need here?
  
  pb <- winProgressBar(title = "Running Simulations", 
                       label = "Simulating ... 0% done", 
                       min = 0, 
                       max = numSims, 
                       initial = 0)
  
   allScores <- data.frame(SimNo = rep(1:numSims, each = n),
                          HomeTeam = rep(remMatches$HomeTeam, numSims),
                          HG = rep(NA, n * numSims),
                          AwayTeam = rep(remMatches$AwayTeam, numSims),
                          AG = rep(NA, n * numSims))
   for (i in 1:numSims){
     scores <- simulate.one.season(remMatches, neutralize)  
     allScores[(n * (i-1) + 1):(n * i), c("HG", "AG")] <- select(scores, HG, AG)
     
     info <- sprintf("Simulating ... %d%% done", round((i/numSims)*100))
     setWinProgressBar(pb, i, label = info)  
   }
   
   close(pb)
   return(allScores)
}

check.individual.game <- function(scores, numSims, Home, Away) {
  games <- filter(scores, HomeTeam == Home & AwayTeam == Away)
  homeWins <- nrow(filter(games, HG > AG))
  awayWins <- nrow(filter(games, HG < AG))
  draws <- nrow(filter(games, HG == AG))
  if (numSims != (homeWins + awayWins + draws))
    print("MAJOR ERROR")
  homeGoals <- sum(games$HG)
  awayGoals <- sum(games$AG)
  print(paste0(Home," wins:  ", homeWins, " (", homeWins/numSims*100, "%)"))
  print(paste0(Away," wins:  ", awayWins, " (", awayWins/numSims*100, "%)"))
  print(paste0("Draws:  ", draws, " (", draws/numSims*100, "%)"))
  print(paste0("HG/game = ", homeGoals/numSims, " AG/game = ",awayGoals/numSims))
}
      
check.comparative.rankings <- function(allSims, numSims, T1, T2) {
  longTeams <- filter(allSims, Team == T1 | Team == T2)
  wideTeams <- dcast(longTeams, SimNo ~ Team, value.var = "Rank")
  T1ahead <- sum(wideTeams[ , 2] < wideTeams[ , 3])
  T2ahead <- sum(wideTeams[ , 3] < wideTeams[ , 2])
  print(paste0(T1, " finishes ahead: ", T1ahead / numSims * 100, "%"))
  print(paste0(T2, " finishes ahead: ", T2ahead / numSims * 100, "%"))
}      


calc.points.and.rank <- function(allScores, 
                                 leagueTable, 
                                 numSims)
{
  teams <- sort(leagueTable$Team)
  n <- length(teams)
  pb <- winProgressBar(title = "Calculating Points/Rank", 
                       label = "Calculating ... 0% done", 
                       min = 0, 
                       max = numSims, 
                       initial = 0)

  # collect the results here
  allSims <- data.frame(Team = rep(teams, numSims),
                        SimNo = rep(1:numSims, each = n),
                        Pts = rep(NA, n * numSims),
                        GD = rep(NA, n * numSims),
                        GS = rep(NA, n * numSims),
                        GC = rep(NA, n * numSims),
                        Rank = rep(NA, n * numSims))

  for (i in 1:numSims) {
    res <- summmarize.one.season.results(leagueTable, filter(allScores, SimNo == i))
    allSims[(n * (i-1) + 1):(n * i), c("Pts", "GD", "GS", "GC", "Rank")] <- res 
    info <- sprintf("Calculating ... %d%% done", round((i/numSims)*100))
    setWinProgressBar(pb, i, label = info)  
  }
  close(pb)
  return (allSims)
}


#######################
# Create Finishing Odds function ---------
#
# returns sorted filtered list of teams above, below, or at a certain league position
#######################
create.finishing.odds.table <- function(allSims, placement, operator) {
  numSims <- nrow(allSims) / length(unique(allSims$Team))

    ifelse(operator == "==",
         rt <- filter(allSims, Rank == placement),
         ifelse (operator == "<",
                 rt <- filter(allSims, Rank < placement),
                 rt <- filter(allSims, Rank > placement))
  )
  rt <- rt %>% 
    group_by(Team) %>% 
    summarize(Count = n(), Percent= Count / numSims * 100) %>% 
    arrange(desc(Count)) %>%
    mutate(Team = as.character(Team))
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
         subtitle = paste0(round(teamMeanPts,1), " mean Points; ", round(teamRelegationOdds,1), 
                          "% odds of relegation -- (", iSim, " sims)"),
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
          axis.text.x = element_text(color = "grey20", size = 8, face = "plain"),
          plot.title = element_text(size=12),
          plot.subtitle = element_text(size=8))
        
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


export_formattable <- function(f, file, width = "100%", height = NULL,
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}


permutate.a.result <- function(allScores, leagueTable, iSim, Home, Away, relegationOrTop4=TRUE) {
  homeWins <- mutate(allScores, 
                     HG = replace(HG, HomeTeam == Home & AwayTeam == Away, 2),
                     AG = replace(AG, HomeTeam == Home & AwayTeam == Away, 1))
  awayWins <- mutate(allScores, 
                     HG = replace(HG, HomeTeam == Home & AwayTeam == Away, 1),
                     AG = replace(AG, HomeTeam == Home & AwayTeam == Away, 2))
  draws <- mutate(allScores, 
                  HG = replace(HG, HomeTeam == Home & AwayTeam == Away, 1),
                  AG = replace(AG, HomeTeam == Home & AwayTeam == Away, 1))
  homeWinSims <- calc.points.and.rank(homeWins, leagueTable, iSim)
  awayWinSims <- calc.points.and.rank(awayWins, leagueTable, iSim)
  drawSims <- calc.points.and.rank(draws, leagueTable, iSim)
  if (relegationOrTop4) {
    relegationH <- create.finishing.odds.table(homeWinSims, 17, ">")
    relegationA <- create.finishing.odds.table(awayWinSims, 17, ">")
    relegationD <- create.finishing.odds.table(drawSims, 17, ">")
    return(c(relegationH, relegationA, relegationD))
  } else {
    top4H <- create.finishing.odds.table(homeWinSims, 5, "<")
    top4A <- create.finishing.odds.table(awayWinSims, 5, "<")
    top4D <- create.finishing.odds.table(drawSims, 5, "<")
    return(c(top4H, top4A, top4D))
  }
}

#################################
#    MAIN CODE ------------------
#    Data came from http://www.football-data.co.uk/
#################################

# Read E0.csv that contains all games played so far
plInput <- read_csv("data/e0.csv")
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

# Tuesday game
pl <- add.game(pl, "Chelsea", 1, "Norwich", 0)

# Wednesday games
pl <- add.game(pl, "Burnley", 1, "Wolves", 1)
pl <- add.game(pl, "Man City", 2, "Bournemouth", 1)
pl <- add.game(pl, "Newcastle", 1, "Tottenham", 3)
pl <- add.game(pl, "Arsenal", 2, "Liverpool", 1)

# Thursday games
pl <- add.game(pl, "Everton", 1, "Aston Villa", 1)
pl <- add.game(pl, "Leicester", 2, "Sheffield United", 0)
pl <- add.game(pl, "Crystal Palace", 0, "Man United", 2)
pl <- add.game(pl, "Southampton", 1, "Brighton", 1)

# Friday game
pl <- add.game(pl, "West Ham", 3, "Watford", 1)

# Gameweek 37

# Saturday game
pl <- add.game(pl, "Norwich", 0, "Burnley", 2)

# Sunday games
pl <- add.game(pl, "Bournemouth", 0, "Southampton", 2)
pl <- add.game(pl, "Tottenham", 3, "Leicester", 0)

# Monday games
pl <- add.game(pl, "Brighton", 0, "Newcastle", 0)
pl <- add.game(pl, "Sheffield United", 0, "Everton", 1)
pl <- add.game(pl, "Wolves", 2, "Crystal Palace", 0)

# Tuesday games
pl <- add.game(pl, "Watford", 0, "Man City", 4)
pl <- add.game(pl, "Aston Villa", 1, "Arsenal", 0)

# Wednesday games
pl <- add.game(pl, "Man United", 1, "West Ham", 1)
pl <- add.game(pl, "Liverpool", 5, "Chelsea", 3)

# Gameweek 38
# pl <- add.game(pl, "West Ham", 0, "Aston Villa", 0)
# pl <- add.game(pl, "Arsenal", 0, "Watford", 0)
# pl <- add.game(pl, "Leicester", 0, Man United", 0)
# pl <- add.game(pl, "Burnley", 0, Brighton", 0)
# pl <- add.game(pl, "Newcastle", 0, "Liverpool", 0)
# pl <- add.game(pl, "Chelsea", 0, "Wolves", 0)
# pl <- add.game(pl, "Crystal Palace", 0, "Tottenham", 0)
# pl <- add.game(pl, "Everton", 0, "Bournemouth", 0)
# pl <- add.game(pl, "Southampton", 0, "Sheffield United", 0)
# pl <- add.game(pl, "Man City", 0, "Norwich", 0)

leagueTable <- create.league.table(pl)
sortedLeagueTable <- create.sorted.league.table(leagueTable)
view(sortedLeagueTable)

## graph of who's played whom and what's left
seasonMatrix <- ggplot(pl, aes(x=AwayTeam, y=HomeTeam, color=FTR)) +
  geom_point(size=2) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggtitle("Premier League 2019-20 Results So Far") +
  scale_color_manual(name = "Result", 
                     labels = c("Away win", "Draw", "Home win"),
                     values = c("red", "black","green")) +
  labs(x = "Away Team", y = "Home Team")
seasonMatrix

remMatches <- determine.remaining.matches(pl, leagueTable)

set.seed(1234)
allScores <- simulate.many.seasons(remMatches, iSim, neutralize=FALSE)
allSims <- calc.points.and.rank(allScores, leagueTable, iSim)

relegation <- create.finishing.odds.table(allSims, 17, ">")
relegation

# run this if want to permutate on a single game
# permutatedFinish <- permutate.a.result(allScores, leagueTable, iSim, "Arsenal", "Watford", TRUE)
permutatedFinish <- permutate.a.result(allScores, leagueTable, iSim, "Liverpool", "Chelsea", FALSE)

# run this if want to see how 2 teams did against each other
# check.individual.game(allScores, iSim, "West Ham", "Watford")

# run this to see odds of teams finishing ahead of each other
check.comparative.rankings(allSims, iSim, "Arsenal", "Tottenham")
 
# odds of league winner
# allSims %>% filter(Rank == 1) %>% select(Team) %>% table/iSim
winner <- create.finishing.odds.table(allSims, 1, "==")
winner

# odds of top 4, 7
# allSims %>% filter(Rank < 5) %>% select(Team) %>% table/iSim
top4 <- create.finishing.odds.table(allSims, 5, "<")
top6 <- create.finishing.odds.table(allSims, 7, "<")
top7 <- create.finishing.odds.table(allSims, 8, "<")
slt <- left_join(sortedLeagueTable, select(top4, c(Team, Top4 = Percent)))
slt <- left_join(slt, select(top6, c(Team, Top6 = Percent)))
slt <- left_join(slt, select(top7, c(Team, Top7 = Percent)))
slt <- left_join(slt, select(relegation, c(Team, Rel = Percent)))

fnc <- function(var, decimal_places) {
  var = sprintf(paste0("%1.",decimal_places,"f"), var)
  var[var=="NA"] = ""
  var
}

vars <- c('Top4', 'Top6', 'Top7', 'Rel')
slt[ , vars] <- mapply(fnc, slt[ , vars], 1)
slt <- slt %>% mutate(Rank = 1:n()) %>%
  select(Rank, Team, Played, Points, GD, Top4, Top6, Top7, Rel)

pos_formatter <- formatter("span", 
                           style = x ~ style(color = ifelse(as.numeric(x)>50, "green", "gray")),
                           #                               x ~ icontext(ifelse(x == "", "remove", "ok")),
                           x ~ sprintf(x))
neg_formatter <- formatter("span", 
                           style = x ~ style(color = ifelse(as.numeric(x)>50, "red", "gray")),
                           #                               x ~ icontext(ifelse(x == "", "remove", "ok")),
                           x ~ sprintf(x))

f1 <- formattable(slt, 
                   align = c("l", "c", "c", "c", "c", "c", "c", "c"),
                   formatters = list(
                     Rank = formatter("span", x ~ sprintf("%.0f", x)),
                     Top4 = pos_formatter,
                     Top6 = pos_formatter,
                     Top7 = pos_formatter,
                     Rel = neg_formatter
                   ))
f1
export_formattable(f1, "table.png")

# p <- plot.relegation.odds("West Ham", allSims, iSim)
# p
# pA <- plot.relegation.odds("Aston Villa", allSims, iSim)
# pB <- plot.relegation.odds("Bournemouth", allSims, iSim)

teams_to_plot <- c("West Ham", "Watford", "Aston Villa", "Bournemouth")
plots <- lapply(teams_to_plot, plot.relegation.odds, allSims, iSim)
nTeams <- length(plots)
nc <- ceiling(sqrt(nTeams))
grid.arrange(grobs=plots, ncol=nc)
gr <- arrangeGrob(grobs = plots, ncol=nc)
ggsave("relegation.png", gr, width=8, height=6, units="in")

# # each team's probabilities per position
# # table(allSims$Team, allSims$Rank)/iSim

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