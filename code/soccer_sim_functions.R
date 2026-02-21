# plan(multisession)
# options(dplyr.summarise.inform = FALSE)

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


just.create.unplayed.matches <- function (league_table, completed_matches = "")
{
  # and alphabetic list of teams in the league
  # teams <- unique(c(league$HomeTeam,league$AwayTeam)) %>% sort
  teams <- sort(league_table$Team)
  num_teams <- length(teams)
  num_matches <- num_teams * (num_teams - 1)
  print(paste("num_teams = ", num_teams, "num_matches = ", num_matches))

  # create list of remaining games
  # this one creates a kind of 'expected goals' by game for use in simulation
  # In past simulations (done in Excel), I used betting odds instead of EG
  # Is there a way to look those up? ****
  rem_matches <- expand.grid(HomeTeam = teams, AwayTeam = teams, 
                             stringsAsFactors = FALSE) %>%
    filter(HomeTeam != AwayTeam) %>%
    mutate(Match = paste(HomeTeam, AwayTeam, sep = " - "))
  
  if (length(completed_matches) > 0 ) {
    rem_matches <- rem_matches %>%
      filter(!(Match %in% completed_matches))
  }
  
  rem_matches <- rem_matches %>%
    select(-Match) %>%
    ungroup()
  
  num_rem_matches = nrow(rem_matches)
  print(paste("numRemMatches =", num_rem_matches))
  rem_matches %>% 
    mutate(MatchNo = row_number() + num_matches - num_rem_matches) %>%
    select(MatchNo, everything())
}

add.expected.goals.from.games.played <- function(unplayed_matches,
                                                 league_table,
                                                 mean_HG,
                                                 mean_AG)
{
  unplayed_matches <- unplayed_matches %>%
    mutate(HG = mean_HG,
           AG = mean_AG,
           TG = (mean_HG + mean_AG) / 2) %>%
    inner_join(subset(league_table, select = -c(Wins, Draws, Losses, Points, GD,
                                                TGS, TGC, AGS, AGC)), 
               by = c("HomeTeam" = "Team")) %>%
    inner_join(subset(league_table, select = -c(Wins, Draws, Losses, Points, GD, 
                                                TGS, TGC, HGS, HGC)), 
               by = c("AwayTeam" = "Team")) %>%
    setNames(c("MatchNo", "HomeTeam", "AwayTeam", "HG", "AG", "TG",
               "GS.by.H", "GC.by.H", "GS.by.A", "GC.by.A")) %>%
    # old calc
    #    mutate(ExpHG = (GS.by.H / TG) * (GC.by.A / TG) * (HG / TG) * TG,
    #           ExpAG = (GS.by.A / TG) * (GC.by.H / TG) * (AG / TG) * TG) %>%
    mutate(ExpHG = (GS.by.H / HG) * GC.by.A,
           ExpAG = (GS.by.A / AG) * GC.by.H)
#   %>% ungroup()
}

# Shouldn't need any more
# create.unplayed.matches <- function(league_table, completed_matches,
#                                     mean_HG, mean_AG) {
#   
#   # and alphabetic list of teams in the league
#   # teams <- unique(c(league$HomeTeam,league$AwayTeam)) %>% sort
#   teams <- sort(league_table$Team)
#   num_teams <- length(teams)
#   num_matches <- num_teams * (num_teams - 1)
#   print(paste("num_teams = ", num_teams, "num_matches = ", num_matches))
#   
#   # create list of remaining games
#   # this one creates a kind of 'expected goals' by game for use in simulation
#   # In past simulations (done in Excel), I used betting odds instead of EG
#   # Is there a way to look those up? ****
#   rem_matches <- expand.grid(HomeTeam = teams, AwayTeam = teams, 
#                             stringsAsFactors = FALSE) %>%
#     filter(HomeTeam != AwayTeam) %>%
#     mutate(Match = paste(HomeTeam, AwayTeam, sep = " - ")) %>%
#     filter(!(Match %in% completed_matches)) %>%
#     select(-Match) %>%
#     mutate(HG = mean_HG,
#            AG = mean_AG,
#            TG = (mean_HG + mean_AG) / 2) %>%
#     inner_join(subset(league_table, select = -c(Wins, Draws, Losses, Points, GD,
#                                                 TGS, TGC, AGS, AGC)), 
#                by = c("HomeTeam" = "Team")) %>%
#     inner_join(subset(league_table, select = -c(Wins, Draws, Losses, Points, GD, 
#                                                 TGS, TGC, HGS, HGC)), 
#                by = c("AwayTeam" = "Team")) %>%
#     setNames(c("HomeTeam", "AwayTeam", "HG", "AG", "TG",
#                "GS.by.H", "GC.by.H", "GS.by.A", "GC.by.A")) %>%
#     # old calc
#     #    mutate(ExpHG = (GS.by.H / TG) * (GC.by.A / TG) * (HG / TG) * TG,
#     #           ExpAG = (GS.by.A / TG) * (GC.by.H / TG) * (AG / TG) * TG) %>%
#     mutate(ExpHG = (GS.by.H / HG) * GC.by.A,
#            ExpAG = (GS.by.A / AG) * GC.by.H) %>%
#     ungroup()
#   
#   num_rem_matches = nrow(rem_matches)
#   print(paste("numRemMatches =", num_rem_matches))
#   rem_matches %>% 
#     mutate(MatchNo = row_number() + num_matches - num_rem_matches) %>%
#     select(MatchNo, everything())
# }

determine.remaining.matches <- function(matches_played, league_table) {
  # create list of Matches already played in complMatch
  completed_matches = paste(matches_played$HomeTeam, matches_played$AwayTeam, 
                            sep = " - ")
  unplayed_matches <- just.create.unplayed.matches(league_table, completed_matches)
  add.expected.goals.from.games.played(unplayed_matches, league_table,
                                       mean(matches_played$FTHG), 
                                       mean(matches_played$FTAG))
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
simulate.many.games <- function(df, numSims) {
  results <- lapply(1:nrow(df), function(i) {
    result <- simulate.one.game(numSims, ExpHG = df$EHG[i], ExpAG = df$EAG[i])
    return (result)
  })
  
  # Add results back to the original data table
  results_df <- do.call(rbind, results)
  data[, c("HomeWins", "AwayWins", "Draws", "AvgHomeGoals", "AvgAwayGoals") := 
         as.data.table(results_df)]
  
  return(data)
}

# Function to simulate matches for each pair and store results in the original data table
simulate.many.games.table <- function(data, numSims) {
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
  final_results_table <- simulate.many.games.table(results_table, numSims)
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

check.comparative.goaldiff <- function(allSims, numSims, T1, T2) {
  longTeams <- dplyr::filter(allSims, Team == T1 | Team == T2)
  wideTeams <- reshape2::dcast(longTeams, SimNo ~ Team, value.var = "GD")
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
create.finishing.odds.table <- function(all_sims, placement, operator) {
  num_sims <- nrow(all_sims) / length(unique(all_sims$Team))
  
  ifelse(operator == "==",
         rt <- filter(all_sims, Rank == placement),
        ifelse (operator == "<",
                 rt <- filter(all_sims, Rank < placement),
                 rt <- filter(all_sims, Rank > placement))
  )
  rt <- rt %>% 
    group_by(Team) %>% 
    summarize(Count = n(), Percent= Count / num_sims * 100) %>% 
    arrange(desc(Count)) %>%
    mutate(Team = as.character(Team))
}

create.finishing.odds.table.chat <- function(all_sims, placement, operator) {

  # Convert to data.table
  dt <- as.data.table(all_sims)
  
  # Get the number of simulations
  num_sims <- nrow(dt) / length(unique(dt$Team))
  
  # Filter based on the operator
  if (operator == "==") {
    rt <- dt[Rank == placement]
  } else if (operator == "<") {
    rt <- dt[Rank < placement]
  } else {
    rt <- dt[Rank > placement]
  }
  
  # Group and summarize
  rt <- rt[, .(Count = .N, Percent = .N / num_sims * 100), by = Team]
  
  # Order and convert Team to character
  rt <- rt[order(-Count), .(Team = as.character(Team), Count, Percent)]
  
  return(rt)
}

#######################
# Plot Relegation Odds  ---------
#
# plots a team's points & relegations possibilities
#####################
plot.relegation.odds <- function(team, all_sims, num_sims) {
  # placement for one team
  # all_sims %>% filter(Team == team) %>% select(Rank) %>% table/num_sims
  # points distribution for one team
  # all_sims %>% filter(Team == team) %>% select(Pts) %>% table/num_sims
  team_sim <- all_sims %>% filter(Team == team) %>% mutate(relegated = Rank > 17)
  team_results_table <- team_sim %>% 
    group_by(Pts) %>% 
    summarize(Pts_likelihood = n() / num_sims, Rel_likelihood = sum(relegated) / n())
  # Show team's average points
  team_mean_pts = mean(team_sim$Pts)
  # Show team's odds of relegation
  team_relegation_odds = sum(team_sim$relegated) / num_sims * 100
  # Show reults in graph
  team_results_table <- mutate(team_results_table, 
                             Non_rel_odds = Pts_likelihood * (1-Rel_likelihood))
  g <- ggplot(team_results_table, aes(x = Pts)) +
    geom_col(aes(y=Pts_likelihood, fill = "PtsLikelihood")) +
    geom_col(aes(y=Non_rel_odds, fill = "NonRelOdds")) +
    labs(title = paste(team, "Final Placement Distribution"),
         subtitle = paste0(round(team_mean_pts,1), " mean Points; ", 
                           round(team_relegation_odds,1), 
                           "% odds of relegation -- (", num_sims, " sims)"),
         caption = "Source:  dw simulation model") +
    scale_fill_manual(name="",
                      labels = c("Staying Up in %", "Relegation in %"),
                      values = c("NonRelOdds"="Green", "PtsLikelihood"="Red")) +
    scale_x_continuous("Points in Final Table",
                       breaks = seq(min(team_results_table$Pts),
                                    max(team_results_table$Pts), 
                                    by = 1)) +
    scale_y_continuous(labels = percent_format(accuracy=1),
                       breaks = seq(0, max(team_results_table$Pts_likelihood), by = 0.02)) +
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
check.placement.odds <- function(team, all_sims, num_sims, spot = 4) {
  teamSim <- all_sims %>% filter(Team == team) 
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
add.game <- function(league, home_team, home_goals, away_team, away_goals) {
  ifelse (home_goals > away_goals, result <- "H",
          ifelse (home_goals == away_goals, result <- "D",
                  result <- "A"))
  league <- add_row(league, HomeTeam = home_team, AwayTeam = away_team,
                    FTR = result, FTHG = home_goals, FTAG = away_goals)
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

######################
#
# Allows you to permutate one of the remaining games (Home v Away) and see how
# various placements with one operator, e.g (c(6, 7,8)) & operator '<')
######################
permutate.a.result.multiple.placements <- function(all_scores, league_table, num_sims, Home, 
                                    Away, placements, operator) {
  # Create a list to store the results for each placement/operator combination
  results <- vector("list", length(placements))
  
  # Mutate the scores for home win, away win, and draw scenarios
  home_wins <- mutate(all_scores, 
                      HG = replace(HG, HomeTeam == Home & AwayTeam == Away, 2),
                      AG = replace(AG, HomeTeam == Home & AwayTeam == Away, 1))
  away_wins <- mutate(all_scores, 
                      HG = replace(HG, HomeTeam == Home & AwayTeam == Away, 1),
                      AG = replace(AG, HomeTeam == Home & AwayTeam == Away, 2))
  draws <- mutate(all_scores, 
                  HG = replace(HG, HomeTeam == Home & AwayTeam == Away, 1),
                  AG = replace(AG, HomeTeam == Home & AwayTeam == Away, 1))
  
  # Calculate the simulations for each scenario
  home_win_sims <- calc.points.and.rank(home_wins, league_table, num_sims)
  away_win_sims <- calc.points.and.rank(away_wins, league_table, num_sims)
  draw_sims <- calc.points.and.rank(draws, league_table, num_sims)
  
  # Iterate over the placements and operators
  idx <- 1
  for (placement in placements) {
#    for (operator in operators) {
      H <- create.finishing.odds.table.chat(home_win_sims, placement, operator)
      A <- create.finishing.odds.table.chat(away_win_sims, placement, operator)
      D <- create.finishing.odds.table.chat(draw_sims, placement, operator)
      
      # Stitch together the H, A, and D data frames
      result <- bind_rows(
        mutate(H, Outcome = "Home Win"),
        mutate(A, Outcome = "Away Win"),
        mutate(D, Outcome = "Draw")
      ) %>%
        pivot_wider(names_from = Outcome, values_from = c(Percent, Count)) %>%
        rename_with(~ c(paste0(Home, " W %"), paste0(Away, " W %"), "Draw %",
                        paste0(Home, " W Count"), paste0(Away, " W Count"), "Draw Count"), 
                    .cols = contains("Percent") | contains("Count")) %>%
        arrange(desc(rowSums(select(., contains("Count")))))
      
      # Store the result in the list
      results[[idx]] <- result
      idx <- idx + 1
#    }
  }
  
  # Return the list of results
  return(results)
}


permutate.a.result <- function(all_scores, league_table, num_sims, Home, Away, 
                               placement, operator) {
  home_wins <- mutate(all_scores, 
                     HG = replace(HG, HomeTeam == Home & AwayTeam == Away, 2),
                     AG = replace(AG, HomeTeam == Home & AwayTeam == Away, 1))
  away_wins <- mutate(all_scores, 
                     HG = replace(HG, HomeTeam == Home & AwayTeam == Away, 1),
                     AG = replace(AG, HomeTeam == Home & AwayTeam == Away, 2))
  draws <- mutate(all_scores, 
                  HG = replace(HG, HomeTeam == Home & AwayTeam == Away, 1),
                  AG = replace(AG, HomeTeam == Home & AwayTeam == Away, 1))
  home_win_sims <- calc.points.and.rank(home_wins, league_table, num_sims)
  away_win_sims <- calc.points.and.rank(away_wins, league_table, num_sims)
  draw_sims <- calc.points.and.rank(draws, league_table, num_sims)
  
  H <- create.finishing.odds.table(home_win_sims, placement, operator)
  A <- create.finishing.odds.table(away_win_sims, placement, operator)
  D <- create.finishing.odds.table(draw_sims, placement, operator)
  
  # Stitch together the H, A, and D data frames
  result <- bind_rows(
    mutate(H, Outcome = "Home Win"),
    mutate(A, Outcome = "Away Win"),
    mutate(D, Outcome = "Draw")
  ) %>%
    pivot_wider(names_from = Outcome, values_from = c(Percent, Count)) %>%
    rename_with(~ c(paste0(Home, " W %"), paste0(Away, " W %"), "Draw %",
                    paste0(Home, " W Count"), paste0(Away, " W Count"), "Draw Count"), 
                .cols = contains("Percent") | contains("Count")) %>%
    arrange(desc(rowSums(select(., contains("Count")))))
    
  return(result)
  
#  return(c(H, A, D))
}


create.538.table <- function(all_sims, sorted_league_table) {
  # odds of league winner
  # all_sims %>% filter(Rank == 1) %>% select(Team) %>% table/iSim
  winner <- create.finishing.odds.table(all_sims, 1, "==")
  winner
  
  # odds of top 4,5,6,7
  # allSims %>% filter(Rank < 5) %>% select(Team) %>% table/iSim
  slt <-
    left_join(sorted_league_table, select(winner, c(Team, Win = Percent)))
  top4 <- create.finishing.odds.table(all_sims, 5, "<")
  top5 <- create.finishing.odds.table(all_sims, 6, "<")
  top6 <- create.finishing.odds.table(all_sims, 7, "<")
  top7 <- create.finishing.odds.table(all_sims, 8, "<")
  top8 <- create.finishing.odds.table(all_sims, 9, "<")
  relegation <- create.finishing.odds.table(all_sims, 17, ">")
  slt <- left_join(slt, select(top4, c(Team, Top4 = Percent)))
  slt <- left_join(slt, select(top5, c(Team, Top5 = Percent)))
  slt <- left_join(slt, select(top6, c(Team, Top6 = Percent)))
  slt <- left_join(slt, select(top7, c(Team, Top7 = Percent)))
  slt <- left_join(slt, select(top8, c(Team, Top8 = Percent)))
  slt <- left_join(slt, select(relegation, c(Team, Rel = Percent)))
  
  fnc <- function(var, decimal_places) {
    var = sprintf(paste0("%1.", decimal_places, "f"), var)
    var[var == "NA"] = ""
    var
  }
  
  vars <- c('Win', 'Top4', 'Top5', 'Top6', 'Top7', 'Top8', 'Rel')
  slt[, vars] <- mapply(fnc, slt[, vars], 1)
  slt <- slt %>% mutate(Rank = 1:n()) %>%
    select(Rank, Team, Played, Points, GD, Win, Top4, Top5, Top6, Top7, Top8, Rel)
}

print.formatted.538 <- function(t)
{  
  pos_formatter <- formatter("span", 
                             style = x ~ style(color = ifelse(as.numeric(x)>50,
                                                              "green", "gray")),
                             #                               x ~ icontext(ifelse(x == "", "remove", "ok")),
                             x ~ sprintf(x))
  neg_formatter <- formatter("span", 
                             style = x ~ style(color = ifelse(as.numeric(x)>50,
                                                              "red", "gray")),
                             #                               x ~ icontext(ifelse(x == "", "remove", "ok")),
                             x ~ sprintf(x))
  
  f1 <- formattable(t, 
                    align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c",
                              "c"),
                    formatters = list(
                      Rank = formatter("span", x ~ sprintf("%.0f", x)),
                      Win = pos_formatter,
                      Top4 = pos_formatter,
                      Top5 = pos_formatter,
                      Top6 = pos_formatter,
                      Top7 = pos_formatter,
                      Top8 = pos_formatter,
                      Rel = neg_formatter
                    ))
  
  export_formattable(f1, "table.png")
  return (f1)
}  

print.538.flextable <- function(t)
{
  
  # define style for border line
  border_style = officer::fp_border(color="black", width=1)
  
  f1 <- flextable(t) %>%
    theme_zebra() %>%
    color(~ as.numeric(GD) < 0, ~ GD, color = "red") %>%
    color(~ as.numeric(Rel) >= 50, ~ Rel, color = "red") %>%
    color(~ as.numeric(Win) >= 50, ~ Win, color = "#4CBB17") %>%
    color(~ as.numeric(Top4) >= 50, ~ Top4, color = "#4CBB17") %>%
    color(~ as.numeric(Top5) >= 50, ~ Top5, color = "#4CBB17") %>%
    color(~ as.numeric(Top6) >= 50, ~ Top6, color = "#4CBB17") %>%
    color(~ as.numeric(Top7) >= 50, ~ Top7, color = "#4CBB17") %>%    
    color(~ as.numeric(Top8) >= 50, ~ Top8, color = "#4CBB17") %>%    
    align(j = 1, align = "right") %>%
    align(j = 2, align = "left") %>%
    align(j = 3:12, align = "right") %>%
    align(part = "header", align = "center") %>%   # Align header row text to center
    width(j = 2, width = 1.3, unit = "in") %>%
    width(j = 3:5, width = 0.5, unit = "in") %>%
    vline(part = "all", j = 5, border = border_style) %>%  # at column 5
    vline(part = "all", j = 11, border = border_style)     # at column 11
  
  save_as_image(f1, path = "table.png")
  
  return(f1)
}


### NOT USING!  Tried for a bit in 2024-03,
### but switched to the easier to use and better flextable above
###
print.kbl.538 <- function(t) {
  # Define formatting functions for conditional coloring
  pos_formatter <- function(x) {
    ifelse(as.numeric(x) > 50,
           cell_spec(x, color = "green"),
           cell_spec(x, color = "gray"))
  }
  
  neg_formatter <- function(x) {
    ifelse(as.numeric(x) > 50,
           cell_spec(x, color = "red"),
           cell_spec(x, color = "gray"))
  }
  
  # Create the kable table
  kable(t, align = "ccccccc") %>%
    kable_classic() %>%
    kable_styling(bootstrap_options = "striped") %>%
#    column_spec(1, bold = T) %>%  # Make the first column bold
#    column_spec(2:6, formatter = "span", format = "f", digits = 0) %>%  # Apply numeric formatting to columns 2-6
#    column_spec(2:6, format = "f", digits = 0) %>%  # Specify 0 decimal places for columns 2-6
#    column_spec(7, width = "5em") %>%  # Adjust column widths
#    column_spec(c("Win", "Top4", "Top5", "Top6", "Top7"), decorator = pos_formatter) %>%  # Apply positive formatter
#    column_spec("Rel", decorator = neg_formatter) %>%  # Apply negative formatter
    save_kable("table.png")  # Save the table as a PNG image
  
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

generate.blank.league.table <- function(num_teams = 24, num_teams_per_group = 4) {
  num_groups <- num_teams / num_teams_per_group
  
  ## Create a list of team names
  team_names <- vector()
  for (group in LETTERS[1:num_groups]) {
    for (team in 1:num_teams_per_group) {
      team_names <- c(team_names, paste0(group, team))
    }
  }
  
  ## Create the league table
  league_table <- data.frame(
    Team = team_names,
    Points = 0,
    TGS = 0,
    TGC = 0
  )
  
  return(league_table)
}



# create schedule of all the group stage games
# make up team names
generate.schedule <- function(num_teams = 24, num_teams_per_group = 4)
{
  num_groups = num_teams / num_teams_per_group
  
    ## Create a list of team names
  team_names <- vector()
  for (group in LETTERS[1:num_groups]) {
    for (team in 1:num_teams_per_group) {
      team_names <- c(team_names, paste0(group, team))
    }
  }
  ## Generate group stage matches
  group_matches <- data.frame()
  for (group in LETTERS[1:num_groups]) {
    group_teams <- team_names[grepl(group, team_names)]
    group_matches <- rbind(group_matches, t(combn(group_teams, 2)))
  }
  colnames(group_matches) <- c("HomeTeam", "AwayTeam")
  ## Append columns with 1 and 1.5
  group_matches <- cbind(group_matches, "ExpHG" = 1, "ExpAG" = 1)
  
  return(group_matches)
}

simulate.euros <- function (iSim = 100)
{
  league_table <- generate.blank.league.table(24, 4)
  schedule <- generate.schedule(24, 4)
  scores <- simulate.many.seasons(schedule, iSim, FALSE)
  
  # run the simulation on this schedule
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
