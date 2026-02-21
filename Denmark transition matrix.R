# dw
transition_matrix <- matrix(c(
  0.65, 0.25, 0.07, 0.03,  # Probabilities for starting state a
  0.15, 0.5, 0.25, 0.1,  # Probabilities for starting state b
  0.0, 0.2, 0.5, 0.3,  # Probabilities for starting state c
  0.0, 0.0, 0.6, 0.4   # Probabilities for starting state d
), nrow = 4, byrow = TRUE)
# 
# jared
# transition_matrix <- matrix(c(
#   0.75, 0.25, 0.05, 0,  # Probabilities for starting state a
#   0.3, 0.5, 0.15, 0.05,  # Probabilities for starting state b
#   0.1, 0.3, 0.4, 0.2,  # Probabilities for starting state c
#   0.0, 0.15, 0.55, 0.3   # Probabilities for starting state d
# ), nrow = 4, byrow = TRUE)

# C.P.
# transition_matrix <- matrix(c(
#   0.5, 0.35, 0.15, 0,  # Probabilities for starting state a
#   0.25, 0.45, 0.15, 0.15,  # Probabilities for starting state b
#   0.1, 0.4, 0.2, 0.3,  # Probabilities for starting state c
#   0.0, 0.25, 0.25, 0.5   # Probabilities for starting state d
# ), nrow = 4, byrow = TRUE)

# consensus
# transition_matrix <- matrix(c(
#   0.7, 0.25, 0.05, 0,  # Probabilities for starting state a
#   0.2, 0.5, 0.25, 0.05,  # Probabilities for starting state b
#   0.0, 0.25, 0.45, 0.3,  # Probabilities for starting state c
#   0.0, 0.05, 0.55, 0.4   # Probabilities for starting state d
# ), nrow = 4, byrow = TRUE)

rownames(transition_matrix) <- c("a", "b", "c", "d")
colnames(transition_matrix) <- c("a", "b", "c", "d")

set.seed(123)  # For reproducibility

num_paths <- 10000
num_years <- 10  # Adjust as needed
states <- c("a", "b", "c", "d")

# Economic outcomes for each state (example values)
# dw 
# economic_outcomes <- c(a = 6, b = 3, c = 0, d = -3)
# Jared
# economic_outcomes <- c(a = 8,b = 6, c = 1, d = -2)
# C.P.
# economic_outcomes <- c(a = 12,b = 4, c = 2, d = -2.5)
economic_outcomes <- c(a = 10, b = 4, c = 1, d = 0)

simulate_path <- function(current_state = "c") {
#  current_state <- sample(states, 1)  # Random initial state
  path <- character(num_years)
  for (year in 1:num_years) {
    path[year] <- current_state
#    if (year == 1) {
#     current_state <- "d"
#    } else {
      current_state <- sample(states, 1, prob = transition_matrix[current_state,])
#    }
  }
  return(path)
}

# Run simulation
all_paths <- replicate(num_paths, simulate_path())

# Calculate economic outcomes
calculate_outcomes <- function(path) {
  sapply(path, function(state) economic_outcomes[state])
}

economic_results <- apply(all_paths, 2, calculate_outcomes)

# Average economic outcome per year across all paths
avg_outcome_per_year <- rowMeans(economic_results)

# Total economic outcome for each path
total_outcomes <- colSums(economic_results)

# Summary statistics
summary(total_outcomes)

probs <- seq(0.05, 0.95, by = 0.05)
percentiles <- quantile(total_outcomes, probs)
print(percentiles, digits = 3)
formatted_percentiles <- round(percentiles, 2) 
print(formatted_percentiles)

# Assuming 'all_paths' contains the simulation results

# Calculate total years in each state
state_totals <- table(all_paths)

# Convert to years
years_in_states <- state_totals / num_paths

# Print results
print(years_in_states)
# Assuming 'all_paths' contains the simulation results

# Function to check if a state appears in a path
state_appears <- function(path, state) {
  state %in% path
}

# Check each state for all paths
paths_with_A <- apply(all_paths, 2, state_appears, state = "a")
paths_with_B <- apply(all_paths, 2, state_appears, state = "b")
paths_with_C <- apply(all_paths, 2, state_appears, state = "c")
paths_with_D <- apply(all_paths, 2, state_appears, state = "d")

# Count the number of paths for each state
num_paths_A <- sum(paths_with_A)
num_paths_B <- sum(paths_with_B)
num_paths_C <- sum(paths_with_C)
num_paths_D <- sum(paths_with_D)

# Create a summary table
state_summary <- data.frame(
  State = c("A", "B", "C", "D"),
  Paths = c(num_paths_A, num_paths_B, num_paths_C, num_paths_D),
  Percentage = c(num_paths_A, num_paths_B, num_paths_C, num_paths_D) / num_paths * 100
)

# Print the summary
print(state_summary)
# Plot average economic outcome over time
plot(1:num_years, avg_outcome_per_year, type = "l", 
     xlab = "Year", ylab = "Average Economic Outcome",
     main = "Average Economic Outcome Over Time")