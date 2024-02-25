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
library(readr)
library(data.table)
library(future)
library(furrr)

plan(multisession)
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

# Code I originally wrote, but was very slow
# summarize.one.season.results <- function(leagueTable, scores) {
#   soFar <- leagueTable %>% select(Team, Points, TGS, TGC)
#   HomeNew <- scores %>%
#     group_by(HomeTeam) %>%
#     summarise(Points = 3 * sum(HG > AG) + 1 * sum(HG == AG),
#               TGS = sum(HG),
#               TGC = sum(AG)) %>%
#     rename(Team = HomeTeam)
#   AwayNew <- scores %>%
#     group_by(AwayTeam) %>%
#     summarise(Points = 3 * sum(AG > HG) + 1 * sum(HG == AG),
#               TGS = sum(AG),
#               TGC = sum(HG)) %>%
#     rename(Team = AwayTeam)
#   res <- bind_rows(soFar, bind_rows(HomeNew, AwayNew)) %>%
#     group_by(Team) %>%
#     summarise(Pts = sum(Points), GS = sum(TGS), GC = sum(TGC)) %>%
#     ungroup()
# 
#   res$GD <- res$GS - res$GC
#   res <- arrange(res, Pts, GD, GS) %>%
#     mutate(Rank=n():1) %>%
#     arrange(Team) %>%
#     select(Pts, GD, GS, GC, Rank)
# }

# Rewritten by Chat GPT-4 2023-05-14 for speed using data tables
summarize.one.season.results <- function(leagueTable, scores) {
  # Convert data frames to data.tables
  leagueTable <- as.data.table(leagueTable)
  scores <- as.data.table(scores)
  
  soFar <- leagueTable[, .(Team, Points, TGS, TGC)]
  
  HomeNew <- scores[, .(Points = 3 * sum(HG > AG) + 1 * sum(HG == AG),
                        TGS = sum(HG),
                        TGC = sum(AG)), by = .(Team = HomeTeam)]
  
  AwayNew <- scores[, .(Points = 3 * sum(AG > HG) + 1 * sum(HG == AG),
                        TGS = sum(AG),
                        TGC = sum(HG)), by = .(Team = AwayTeam)]
  
  res <- rbindlist(list(soFar, HomeNew, AwayNew), fill = TRUE)
  res <- res[, .(Pts = sum(Points, na.rm = TRUE),
                 GS = sum(TGS, na.rm = TRUE),
                 GC = sum(TGC, na.rm = TRUE)), by = Team]
  
  res[, GD := GS - GC]
  res <- res[order(-Pts, -GD, -GS)][, Rank := .I][order(Team)][, .(Pts, GD, GS, GC, Rank)]
  
  return(res)
}




# rewritten by chatGPT 2023-05-14 for speed
# by using data tables instead of data frame
simulate.one.season <- function(remMatches, neutralize = FALSE) {
  setDT(remMatches) # Convert remMatches to a data.table for faster processing
  
  if (neutralize) {
    homeExpGoals <- 1
    awayExpGoals <- 1
  } else {
    homeExpGoals <- remMatches$ExpHG
    awayExpGoals <- remMatches$ExpAG
  }
  
  nGamesRem <- nrow(remMatches)
  scores <- remMatches[, c("HG", "AG") := .(rpois(nGamesRem, lambda = homeExpGoals), 
                                            rpois(nGamesRem, lambda = awayExpGoals))] 
  # HPts = 3 * (HG > AG) + 1 * (HG == AG),
  # APts = 3 * (HG < AG) + 1 * (HG ==),
  # Draws = (HG == AG))
  
  return(scores)
}

simulate.one.game <- function(numSims, ExpHG, ExpAG) {
  # Simulate home and away goals
  homeGoals <- rpois(numSims, lambda = ExpHG)
  awayGoals <- rpois(numSims, lambda = ExpAG)

  # Calculate match outcomes
  homeWins <- sum(homeGoals > awayGoals)
  awayWins <- sum(awayGoals > homeGoals)
  draws <- numSims - (homeWins + awayWins)
  
  # Calculate average goals scored by each team
  avgHomeGoals <- mean(homeGoals)
  avgAwayGoals <- mean(awayGoals)
  
  # Return results
  results <- list(
    HomeWins = homeWins,
    AwayWins = awayWins,
    Draws = draws,
    AvgHomeGoals = avgHomeGoals,
    AvgAwayGoals = avgAwayGoals
  )
  return(results)
}

# Function to simulate matches for each pair and store results in the original data table
simulate.many.games <- function(data, numSims) {
  results <- lapply(1:nrow(data), function(i) {
    result <- simulate.one.game(numSims, ExpHG = data[i, EHG], ExpAG = data[i, EAG])
    return (result)
  })
  
  # Add results back to the original data table
  results_df <- do.call(rbind, results)
  data[, c("HomeWins", "AwayWins", "Draws", "AvgHomeGoals", "AvgAwayGoals") := 
         as.data.table(results_df)]
  
  return(data)
}

simulate.grid.of.poissons <- function(numSims)
{
  initial_data <- expand.grid(EHG = seq(0.25, 4, by = 0.25), EAG = seq(0.25, 4, by = 0.25))
  results_table <- data.table(initial_data)
  # Call the function to simulate matches for each pair and store results in the original data table
  final_results_table <- simulate.many.games(results_table, numSims)
  return(final_results_table)
}



simulate.many.seasons <- function(remMatches,
                                  numSims = 100,
                                  neutralize = FALSE)
{
  n <- nrow(remMatches) # do we need here?
  
#  pb <- winProgressBar(title = "Running Simulations", 
#                       label = "Simulating ... 0% done", 
#                       min = 0, 
#                       max = numSims, 
#                       initial = 0)
  
   allScores <- data.frame(SimNo = rep(1:numSims, each = n),
                          HomeTeam = rep(remMatches$HomeTeam, numSims),
                          HG = rep(NA, n * numSims),
                          AwayTeam = rep(remMatches$AwayTeam, numSims),
                          AG = rep(NA, n * numSims))
   
#   OLD code w/ for loop, replaced by ChatGPT recommended lapply 2023-05-08
#   for (i in 1:numSims){
#     scores <- simulate.one.season(remMatches, neutralize)  
#     allScores[(n * (i-1) + 1):(n * i), c("HG", "AG")] <- select(scores, HG, AG)
#     
#     info <- sprintf("Simulating ... %d%% done", round((i/numSims)*100))
#     setWinProgressBar(pb, i, label = info)  
#   }

   simResults <- lapply(1:numSims, function(i) {
     scores <- simulate.one.season(remMatches, neutralize)
     cbind(scores, SimNo = i)
   })
   allScores <- do.call(rbind, simResults)
      
#   close(pb)
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
  longTeams <- dplyr::filter(allSims, Team == T1 | Team == T2)
  wideTeams <- reshape2::dcast(longTeams, SimNo ~ Team, value.var = "Rank")
  T1ahead <- sum(wideTeams[ , 2] < wideTeams[ , 3])
  T2ahead <- sum(wideTeams[ , 3] < wideTeams[ , 2])
  if (T1 > T2) {
    temp <- T2ahead
    T2ahead <- T1ahead
    T1ahead <- temp
  }
  print(paste0(T1, " finishes ahead: ", T1ahead / numSims * 100, "%"))
  print(paste0(T2, " finishes ahead: ", T2ahead / numSims * 100, "%"))
}      

# Original function I wrote
# replaced by chatGPT version below
# calc.points.and.rank <- function(allScores, 
#                                  leagueTable, 
#                                  numSims)
# {
#   teams <- sort(leagueTable$Team)
#   n <- length(teams)
#   pb <- winProgressBar(title = "Calculating Points/Rank",
#                        label = "Calculating ... 0% done",
#                        min = 0,
#                        max = numSims,
#                        initial = 0)
# 
#   # collect the results here
#   allSims <- data.frame(Team = rep(teams, numSims),
#                         SimNo = rep(1:numSims, each = n),
#                         Pts = rep(NA, n * numSims),
#                         GD = rep(NA, n * numSims),
#                         GS = rep(NA, n * numSims),
#                         GC = rep(NA, n * numSims),
#                         Rank = rep(NA, n * numSims))
# 
#   for (i in 1:numSims) {
#     res <- summarize.one.season.results(leagueTable, filter(allScores, SimNo == i))
#     allSims[(n * (i-1) + 1):(n * i), c("Pts", "GD", "GS", "GC", "Rank")] <- res
#     info <- sprintf("Calculating ... %d%% done", round((i/numSims)*100))
#     setWinProgressBar(pb, i, label = info)
#   }
#   close(pb)
# # 
# #   # Use lapply instead of a for loop
# #   simResults <- lapply(1:numSims, function(i) {
# #     res <- summarize.one.season.results(leagueTable, 
# #                                          filter(allScores, SimNo == i))
# #     cbind(res, Team = teams, SimNo = i)
# #   })
# #   
# #   # Combine the results into a single data frame
# #   allSims <- do.call(rbind, simResults)
#   
#   return (allSims)
# }


calc.points.and.rank <- function(allScores, leagueTable, numSims) {
  teams <- sort(leagueTable$Team)
  n <- length(teams)
  
  simResults <- lapply(1:numSims, function(i) {
# simResults <- parLapply(1:numSims, function(i) {
   res <- summarize.one.season.results(leagueTable, allScores[allScores$SimNo == i,])
    data.frame(Team = rep(teams, 1),
               SimNo = rep(i, n),
               Pts = res$Pts,
               GD = res$GD,
               GS = res$GS,
               GC = res$GC,
               Rank = res$Rank)
  })
  
  simResults <- do.call(rbind, simResults)
  
  return(simResults)
}

# written by chatGPT 2023-05-14 for speed
# uses multiprocessors / parallel processing
# calc.points.and.rank <- function(allScores, leagueTable, numSims) {
#   teams <- sort(leagueTable$Team)
#   n <- length(teams)
#   
#   simResults <- future_map_dfr(1:numSims, 
#                                        .export = c("teams", 
#                                                    "summarize.one.season.results"),
#                                                    function(i) {
#     res <- summarize.one.season.results(leagueTable, allScores[allScores$SimNo == i,])
#     data.frame(Team = rep(teams, 1),
#                SimNo = rep(i, n),
#                Pts = res$Pts,
#                GD = res$GD,
#                GS = res$GS,
#                GC = res$GC,
#                Rank = res$Rank)
#   })
#   
#   return(simResults)
# }

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
plot.points.vs.rank <- function(team, allSims, numSims, numTeams = 20) {

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
                          "under", numSims, "seasons simulation"),
         caption = "Source:  dw simulation model") +
    #    scale_fill_manual(name="",
    #                      labels = c("Staying Up in %", "Relegation in %"),
    #                      values = c("NonRelOdds"="Green", "PtsLikelihood"="Red")) +
    #    scale_fill_gradientn(colours = c("green", "yellow","red"),
    #                         limits=c(1,20)) +
    scale_fill_gradientn(colours = c("green", "yellow", "orange", "red"),
                         limits = c(1, numTeams)) +
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

#######################
# Check Points vs Rank  ---------
#
# plots a team's points & ranking
#######################
check.placement.odds <- function(team, allSims, numSims, spot = 4) {
  teamSim <- allSims %>% filter(Team == team) 
  teamPlace <- 
    teamSim %>% 
    group_by(Pts) %>% 
    summarize(np = n(),
              tplace = sum(Rank < (spot + 1)),
              pct = tplace / np)
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


permutate.a.result <- function(allScores, leagueTable, iSim, Home, Away, 
                               placement, operator) {
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
  
  H <- create.finishing.odds.table(homeWinSims, placement, operator)
  A <- create.finishing.odds.table(awayWinSims, placement, operator)
  D <- create.finishing.odds.table(drawSims, placement, operator)
  return(c(H, A, D))
}

create.538.table <- function(allSims, sortedLeagueTable) {
  
  # odds of league winner
  # allSims %>% filter(Rank == 1) %>% select(Team) %>% table/iSim
  winner <- create.finishing.odds.table(allSims, 1, "==")
  winner
  
  # odds of top 4, 7
  # allSims %>% filter(Rank < 5) %>% select(Team) %>% table/iSim
  slt <- left_join(sortedLeagueTable, select(winner, c(Team, Win = Percent)))
  top4 <- create.finishing.odds.table(allSims, 5, "<")
  top6 <- create.finishing.odds.table(allSims, 7, "<")
  top7 <- create.finishing.odds.table(allSims, 8, "<")
  slt <- left_join(slt, select(top4, c(Team, Top4 = Percent)))
  slt <- left_join(slt, select(top6, c(Team, Top6 = Percent)))
  slt <- left_join(slt, select(top7, c(Team, Top7 = Percent)))
  slt <- left_join(slt, select(relegation, c(Team, Rel = Percent)))
  
  fnc <- function(var, decimal_places) {
    var = sprintf(paste0("%1.",decimal_places,"f"), var)
    var[var=="NA"] = ""
    var
  }
  
  vars <- c('Win', 'Top4', 'Top6', 'Top7', 'Rel')
  slt[ , vars] <- mapply(fnc, slt[ , vars], 1)
  slt <- slt %>% mutate(Rank = 1:n()) %>%
    select(Rank, Team, Played, Points, GD, Win, Top4, Top6, Top7, Rel)
}

print.formatted.538 <- function(t)
{  
  pos_formatter <- formatter("span", 
                             style = x ~ style(color = ifelse(as.numeric(x)>50, "green", "gray")),
                             #                               x ~ icontext(ifelse(x == "", "remove", "ok")),
                             x ~ sprintf(x))
  neg_formatter <- formatter("span", 
                             style = x ~ style(color = ifelse(as.numeric(x)>50, "red", "gray")),
                             #                               x ~ icontext(ifelse(x == "", "remove", "ok")),
                             x ~ sprintf(x))
  
  f1 <- formattable(t, 
                    align = c("l", "c", "c", "c", "c", "c", "c", "c", "c"),
                    formatters = list(
                      Rank = formatter("span", x ~ sprintf("%.0f", x)),
                      Win = pos_formatter,
                      Top4 = pos_formatter,
                      Top6 = pos_formatter,
                      Top7 = pos_formatter,
                      Rel = neg_formatter
                    ))

  export_formattable(f1, "table.png")
  return (f1)
}  

print.maccabi.report = function(sims, title = "Maccabi Report")
{
  print (title)
  
  pts <- table(sims$Pts)
  print ("Points distribution")
  print(pts)
  print(prop.table(pts))
  
  rank <- table(sims$Rank)
  print ("Rank distribution")
  print (rank)
  print(prop.table(rank))

  cross <- table(sims$Pts, sims$Rank)  
  print ("Cross Distribution")
  print (cross)
  print(prop.table(cross, margin = 1))
}


run.maccabi <- function(iSim = 100)
{
# maccSchedule <- read_csv("data/Maccabi 55 schedule 2023.csv")
# maccSchedule <- read_csv("data/Maccabi 55 sched-2.csv")
# maccSchedule <- read_csv("data/Maccabi 55 sched after game 1.csv")
# maccSchedule <- read_csv("data/Maccabi 55 schedule after game 2.csv")
# maccSchedule <- read_csv("data/Maccabi 55 schedule after USA GB.csv")
# maccSchedule <- read_csv("data/Maccabi 55 schedule after ARG MEX.csv")
#  maccSchedule <- read_csv("data/Maccabi 55 schedule after ARG MEX USA GB.csv")
  maccSchedule <- read_csv("data/Maccabi 55 schedule after game 3.csv")
  # maccSchedule <- read_csv("data/Maccabi 55 schedule no USA.csv")
  
  # run all seasons
  # change FALSE -> TRUE to neutralize scores to 1-1
  mScores <- simulate.many.seasons(maccSchedule, iSim, FALSE)
  check.individual.game(mScores, iSim, "Australia", "USA")
  
# maccTable <- read.csv("data/Maccabi blank league table 2023.csv")
# maccTable <- read.csv("data/Maccabi league table USA 1 BRZ 2.csv")
# maccTable <- read.csv("data/Maccabi league table USA draw and others win.csv")
# maccTable <- read.csv("data/Maccabi league table after game 1.csv")
# maccTable <- read.csv("data/Maccabi league table USA 2 ARG 2.csv")
# maccTable <- read.csv("data/Maccabi league table 2023.12.30.csv")
# maccTable <- read.csv("data/Maccabi league table USA 2 GB 0.csv")
# maccTable <- read.csv("data/Maccabi league table USA 0 GB 1.csv")
# maccTable <- read.csv("data/Maccabi league table USA LW.csv")
# maccTable <- read.csv("data/Maccabi league table ARG 3 MEX 1.csv")
# maccTable <- read.csv("data/Maccabi league table ARG 3 MEX 1 USA 2 GB 0.csv")
# maccTable <- read.csv("data/Maccabi league table ARG 3 MEX 1 USA 1 GB 1.csv")
# maccTable <- read.csv("data/Maccabi league table ARG 3 MEX 1 USA 0 GB 1.csv")
  maccTable <- read.csv("data/Maccabi league table after 3 games.csv")
  
  simsAll <- calc.points.and.rank(mScores, maccTable, iSim)
  gold <- create.finishing.odds.table(simsAll, 3, "<")
  print(gold)
  bronze <- create.finishing.odds.table(simsAll, 5, "<")
  print(bronze)
 
#  freqTableAllTeams <- table(pr$Pts, pr$Rank)
   simsUSA <- subset(simsAll, Team == "USA")
   simsARG <- subset(simsAll, Team == "Argentina")
#  freqTableUSA <- table(prUSA$Pts, prUSA$Rank)
   print.maccabi.report(simsAll, "All Teams")
   print.maccabi.report(simsUSA, "USA")
   print.maccabi.report(simsARG, "Argentina")
      
   p <- plot.points.vs.rank("USA", simsAll, iSim, 6)
   print (p)
#  pctTableAllTeams <- prop.table(freqTableAllTeams, margin = 1)
    
#  distTable <- data.table(xtabs(~ Pts + Rank, data = pr))
#  distTable$Pts <- as.integer(distTable$Pts)
#  distTable$Rank <- as.integer(distTable$Rank)
#  freqTableAllTeams <- dcast(distTable, Pts ~ Rank, value.var="N", fun.aggregate = sum)
#  dist
  
}


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
