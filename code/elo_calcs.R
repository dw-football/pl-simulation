library(stats)

# The standard win prob from elo diffs
# Doesn't tell you anything about draws!
calc.win.prob.from.elo <- function(rating_a, rating_b) {
  1 / (1 + 10^((rating_b - rating_a) / 400))
}

# calculates poisson win/loss/draw probs from two xGs
poisson_match_probabilities <- function(lambda_home, lambda_away) {
  max_goals <- 10  # Adjust as needed
  prob_matrix <- outer(0:max_goals, 0:max_goals, 
                       function(h, a) dpois(h, lambda_home) * dpois(a, lambda_away))
  home_win <- sum(prob_matrix[lower.tri(prob_matrix)])
  away_win <- sum(prob_matrix[upper.tri(prob_matrix)])
  draw <- sum(diag(prob_matrix))
  c(home_win, draw, away_win)
}

# to create some standard charts

create.elo.and.xg.charts <- function() {
  # Create a sequence of xG values
  home_xg_seq <- seq(1.4, 0.5, by = -0.1)
  away_xg_seq <- 2.8 - home_xg_seq

  # Calculate probabilities for each combination
  results <- lapply(seq_along(home_xg_seq), function(i) {
    probs <- poisson_match_probabilities(home_xg_seq[i], away_xg_seq[i])
    c(home_xg_seq[i], away_xg_seq[i], probs)
  })

  # Convert to a data frame
  df <- do.call(rbind, results)
  colnames(df) <- c("Home xG", "Away xG", "Home Win", "Draw", "Away Win")
  flextable(as.data.frame(df))

  # Create sequence of Elo differences
  elo_diffs <- seq(0, 200, by = 20)

  # Calculate probabilities for each Elo difference
  results <- lapply(elo_diffs, function(diff) {
     home_win_prob <- calc.win.prob.from.elo(1500 + diff, 1500) 
#    away_win_prob <- calculate_win_probability(1500, 1500 + diff)
     xg_values <- find_xg_values(home_win_prob, 2.8)
     probs <- poisson_match_probabilities(xg_values["home_xg"], xg_values["away_xg"])
     c(diff, probs)
  })

  # Convert to a data frame
  df <- as.data.frame(do.call(rbind, results))
  colnames(df) <- c("Elo Difference", "Home Win", "Draw", "Away Win")

  # Format probabilities as percentages
  df[, 2:4] <- lapply(df[, 2:4], function(x) sprintf("%.2f%%", x * 100))

  # Create the flextable
  ft <- flextable(df)

  # Customize the table
  ft <- ft %>%
  set_header_labels(
    "Elo Difference" = "Elo Difference"
  ) %>%
  theme_vanilla() %>%
  autofit()

  # Display the table
  ft
}


# Given a home win % (away win % being 1 minus home, so it deals with draws correctly)
find_xg_values <- function(home_win_prob, total_xg, tolerance = 0.0001) {
  away_win_prob <- 1 - home_win_prob
  objective <- function(home_xg) {
    away_xg <- total_xg - home_xg
    probs <- poisson_match_probabilities(home_xg, away_xg)
    (probs[1] - home_win_prob)^2 + (probs[3] - away_win_prob)^2
  }
  
  result <- optimize(objective, interval = c(0, total_xg))
  home_xg <- result$minimum
  away_xg <- total_xg - home_xg
  
  c(home_xg = home_xg, away_xg = away_xg)
}

simulate_match <- function(home_elo, away_elo, home_advantage = 100, total_xg = 2.8) {
  home_win_prob <- calc.win.prob.from.elo(home_elo + home_advantage, away_elo)
  xg_values <- find_xg_values(home_win_prob, total_xg)
  
  home_goals <- rpois(1, xg_values["home_xg"])
  away_goals <- rpois(1, xg_values["away_xg"])
  
  return(c(home_goals, away_goals))
}


test.elo <- function()
{
  # Example usage
  home_team_elo <- 1550
  away_team_elo <- 1500
  home_advantage <- 50
  xG_per_game <- 2.5

  # Calculate theoretical probabilities
  theoretical_home_win_prob <- calc.win.prob.from.elo(home_team_elo + home_advantage, away_team_elo)
  theoretical_away_win_prob <- calc.win.prob.from.elo(away_team_elo, home_team_elo + home_advantage)

  cat(sprintf("Theoretical probabilities: Home Win: %.4f, Away Win: %.4f\n",
            theoretical_home_win_prob, theoretical_away_win_prob))

  # Find xG values
  xg_values <- find_xg_values(theoretical_home_win_prob, xG_per_game)
  cat(sprintf("Calculated xG values: Home xG: %.4f, Away xG: %.4f\n",
              xg_values["home_xg"], xg_values["away_xg"]))

  # Verify Poisson probabilities
  poisson_probs <- poisson_match_probabilities(xg_values["home_xg"], xg_values["away_xg"])
  cat(sprintf("Poisson probabilities: Home Win: %.4f, Draw: %.4f, Away Win: %.4f\n",
              poisson_probs[1], poisson_probs[2], poisson_probs[3]))
  
  # Run simulations
  set.seed(123)  # For reproducibility
  num_simulations <- 10000
  results <- replicate(num_simulations, simulate_match(home_team_elo, away_team_elo, 
                                                       home_advantage, xG_per_game))
  
  # Calculate simulated probabilities
  home_wins <- sum(results[1,] > results[2,]) / num_simulations
  draws <- sum(results[1,] == results[2,]) / num_simulations
  away_wins <- sum(results[1,] < results[2,]) / num_simulations
  
  cat(sprintf("Simulated probabilities: Home Win: %.4f, Draw: %.4f, Away Win: %.4f\n",
              home_wins, draws, away_wins))
  
  cat(sprintf("Average score after %d simulations: Home %.2f - %.2f Away\n", 
              num_simulations, mean(results[1,]), mean(results[2,])))
}


find_home_advantage <- function(target_probs, num_simulations = 1000, elo_sd = 100) {
  objective <- function(home_advantage) {
    results <- replicate(num_simulations, {
      home_elo <- rnorm(1, 1500, elo_sd)
      away_elo <- rnorm(1, 1500, elo_sd)
      simulate_match(home_elo, away_elo, home_advantage)
    })
    
    simulated_probs <- rowMeans(results)
    sum((simulated_probs - target_probs)^2)
  }
  
  result <- optimize(objective, interval = c(0, 200))
  result$minimum
}


find.home.advantage.from.league.probs <- function()
{
  # Target probabilities
  target_probs <- c(0.41, 0.26, 0.33)  # Home win, Draw, Away win
  
  # Find optimal home advantage
  optimal_home_advantage <- find_home_advantage(target_probs)
  
  cat(sprintf("Optimal home field advantage: %.2f Elo points\n", optimal_home_advantage))
}

add.expected.goals.from.elo <- function(unplayed_matches, league_table_with_elo) {
  # Ensure the required columns exist in league_table_with_elo
  if (!all(c("Team", "Elo") %in% colnames(league_table_with_elo))) {
    stop("league_table_with_elo must have 'Team' and 'Elo' columns")
  }
  
  # League averages
  leaguewide_xG <- 2.8
  home_advantage <- 0.4
  
  # Add expected goals to unplayed_matches
  unplayed_matches_with_exp_goals <- unplayed_matches %>%
    rowwise() %>%
    mutate(
      HomeElo = league_table_with_elo$Elo[league_table_with_elo$Team == HomeTeam],
      AwayElo = league_table_with_elo$Elo[league_table_with_elo$Team == AwayTeam],
      HomeWinProb = calc.win.prob.from.elo(HomeElo + home_advantage, AwayElo),
      xg_values = list(find_xg_values(HomeWinProb, 2.8)),
      ExpHG = xg_values[[1]],
      ExpAG = xg_values[[2]]
    ) %>%
    select(-HomeElo, -AwayElo, -HomeWinProb, -xg_values) %>%
    ungroup()
  
  return(unplayed_matches_with_exp_goals)
}
