###
### RUN THE ACTUAL SIMULATIONS
###

### We need remMatches and the so far leagueTable already here from the data


# set # of simulations
num_sims <- 5000


## The neutralize flag, if turned on, allows me to change the model
## such that every team has same chance of beating, drawing, or losing
## to every other.  It's a good model check and shows (by comparison)
## the impact of prior results on the model
all_scores <- simulate.many.seasons(rem_matches, num_sims, neutralize=FALSE)
# all_scores <- simulate.many.seasons(rem_matches, num_sims, neutralize=TRUE)
all_sims <- calc.points.and.rank(all_scores, league_table, num_sims)
