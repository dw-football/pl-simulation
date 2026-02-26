# Architecture

## Model overview

A **Dixon-Coles-style attack/defense strength model** (without the low-score correction). Historical home and away goals-per-game are used to compute expected goals (xG) for each remaining fixture. Goals are then drawn from independent Poisson distributions.

### xG formula

```
ExpHG = (HomeTeam_HGS / league_mean_HG) × AwayTeam_AGC
ExpAG = (AwayTeam_AGS / league_mean_AG) × HomeTeam_HGC
```

Where:
- `HGS` = home goals scored per game (team's home attack)
- `HGC` = home goals conceded per game (team's home defense)
- `AGS` = away goals scored per game (team's away attack)
- `AGC` = away goals conceded per game (team's away defense)
- `league_mean_HG` / `league_mean_AG` = season-wide averages

---

## Data flow

```
data/E0.csv
    │
    ▼ get_pl_data.R
create.league.table()          →  data/league_table.csv
create.sorted.league.table()   →  data/sorted_league_table.csv
determine.remaining.matches()  →  data/rem_matches.csv
    │
    ▼ simulate.R
simulate.many.seasons()        → all_scores  (data.table, ~500K rows for 5000 sims)
calc.points.and.rank()         → all_sims    (data.table, numTeams × numSims rows)
                               → data/sim_data.Rdata
    │
    ▼ analyze.R / Premier_League_Simulation.qmd
create.538.table()             → formatted odds table
print.538.flextable()          → table.png
plot.relegation.odds()         → relegation.png
plot.points.vs.rank()          → top of table.png
```

---

## Function reference (`code/soccer_sim_functions.R`)

### Table construction

| Function | Inputs | Output | Notes |
|----------|--------|--------|-------|
| `create.league.table(league)` | `league` data frame (E0.csv columns) | Unsorted league table data frame | Computes HGS, HGC, AGS, AGC per-game rates |
| `create.sorted.league.table(leagueTable)` | league table data frame | Sorted by Pts, GD, GS | Adds Played, Scored, Conceded columns |

### Fixture generation

| Function | Inputs | Output | Notes |
|----------|--------|--------|-------|
| `just.create.unplayed.matches(league_table, completed_matches)` | league table, vector of "Home - Away" strings | data frame of all unfixed fixtures | Used internally |
| `add.expected.goals.from.games.played(unplayed_matches, league_table, mean_HG, mean_AG)` | unplayed fixtures, league table, season means | rem_matches with ExpHG/ExpAG columns | |
| `determine.remaining.matches(matches_played, league_table)` | played matches, league table | rem_matches data frame | Combines the two functions above |

### Simulation

| Function | Inputs | Output | Notes |
|----------|--------|--------|-------|
| `simulate.many.seasons(remMatches, numSims, neutralize)` | remaining matches, N sims, flag | data.table with all simulated scores | Vectorized: draws all goals in 2 `rpois()` calls |
| `calc.points.and.rank(allScores, leagueTable, numSims)` | all_scores, league_table, N | all_sims data.table | Group-by on SimNo; no loop |
| `summarize.one.season.results(leagueTable, scores)` | league table, one sim's scores | per-team Pts/GD/GS/GC/Rank | Called by old loop-based version; not used in current pipeline |
| `simulate.one.season(remMatches, neutralize)` | remaining matches, flag | scores data.table | Called by old loop-based version; not used in current pipeline |

### Analysis

| Function | Inputs | Output | Notes |
|----------|--------|--------|-------|
| `create.finishing.odds.table(all_sims, placement, operator)` | all_sims, position, `"=="` / `"<"` / `">"` | Team/Count/Percent data frame | dplyr version |
| `create.finishing.odds.table.chat(all_sims, placement, operator)` | same | same | data.table rewrite; faster; prefer this one |
| `create.538.table(all_sims, sorted_league_table)` | all_sims, sorted table | formatted table data frame | Calls finishing odds for Win/Top4/Top5/Top6/Top7/Top8/Rel |
| `permutate.a.result(all_scores, league_table, num_sims, Home, Away, placement, operator)` | scores, table, N, teams, zone | wide data frame with H/D/A columns | Forces one match to each outcome and re-ranks |
| `permutate.a.result.multiple.placements(...)` | same + `placements` vector | list of data frames | Loops over multiple zones |
| `rank.games.by.importance(all_scores, league_table, num_sims, rem_matches, zones)` | scores, table, N, fixtures, zone list | data.table sorted by Total importance | Expensive: calls `calc.points.and.rank` 3× per fixture |
| `check.individual.game(scores, numSims, Home, Away)` | all_scores, N, teams | printed summary | Diagnostic |
| `check.comparative.rankings(allSims, numSims, T1, T2)` | all_sims, N, teams | printed head-to-head | |
| `check.placement.odds(team, all_sims, num_sims, spot)` | team name, all_sims, N, zone | per-points breakdown | |

### Output

| Function | Output |
|----------|--------|
| `print.formatted.538(t)` | Saves `table.png` using `formattable` + webshot (older method) |
| `print.538.flextable(t)` | Saves `table.png` using `flextable` (preferred method) |
| `plot.relegation.odds(team, all_sims, num_sims)` | ggplot object: points distribution colored by relegation risk |
| `plot.points.vs.rank(team, allSims, numSims, numTeams)` | ggplot object: points distribution stacked by final rank |

### Utility

| Function | Purpose |
|----------|---------|
| `add.game(league, home_team, home_goals, away_team, away_goals)` | Appends a result to the match data frame before building the table |
| `export_formattable(f, file, ...)` | Exports a formattable widget to PNG via webshot |

### Less-used / alternative-league functions

| Function | Purpose |
|----------|---------|
| `simulate.one.game(numSims, ExpHG, ExpAG)` | Simulates a single fixture N times; returns win/draw/loss counts |
| `simulate.many.games(df, numSims)` | Has a bug: writes to `data` (undefined) instead of `df`. Not in main pipeline. |
| `simulate.grid.of.poissons(numSims)` | Generates a grid of xG pairs and simulates each |
| `generate.blank.league.table(num_teams, num_teams_per_group)` | Creates an empty table for tournament use |
| `generate.schedule(num_teams, num_teams_per_group)` | Creates round-robin group stage schedule |
| `simulate.euros(iSim)` | Skeleton Euro tournament simulation |
| `run.maccabi(iSim)` | Maccabi Games simulation — reads hardcoded CSV files |
| `print.maccabi.report(sims, title)` | Prints points/rank distribution tables |

---

## Key design decisions

### `neutralize = TRUE`

When set, all xG values are forced to 1.0 — every team has equal expected goals in every game. This is a model sanity check. Comparing neutralized vs. actual results shows the predictive signal in the historical strength data.

### `data.table` alongside tidyverse

Performance-critical paths (`simulate.many.seasons`, `calc.points.and.rank`, `create.finishing.odds.table.chat`) use `data.table` group-by operations. Everything else uses dplyr/tidyverse. Do not remove `data.table` imports.

### Point deductions

Applied manually in `get_pl_data.R` after calling `create.league.table()`:

```r
league_table$Points[league_table$Team == "Everton"] <-
  league_table$Points[league_table$Team == "Everton"] - 8
```

### Tiebreakers

Final standings sort by `Pts DESC, GD DESC, GS DESC`. This matches Premier League rules for the first two tiebreakers. Head-to-head is not implemented (would require restructuring `calc.points.and.rank`).

---

## Known issues

1. **`simulate.many.games()` bug**: reads `df` parameter but writes results to `data` (undefined). Not in the main pipeline so it is silent. Do not use this function.

2. **Duplicate function pairs**:
   - `create.finishing.odds.table()` (dplyr) and `create.finishing.odds.table.chat()` (data.table) do the same thing. `permutate.a.result()` calls the dplyr version; `rank.games.by.importance()` calls the data.table version. Prefer `.chat` for speed.
   - `export_formattable()` is defined in both `soccer_sim_functions.R` and `formattable.R`.

3. **`print.kbl.538()`**: dead code, marked as unused. All meaningful lines are commented out.

4. **`simulate.one.season()` / `summarize.one.season.results()`**: these are the pre-refactor loop-based functions. They are no longer called by `simulate.many.seasons()` or `calc.points.and.rank()`. They remain in the file for reference.

---

## Alternative: Elo-based xG (`code/elo_calcs.R`)

A secondary approach used only for `denmark.R`. Converts Elo ratings to win/draw/loss probabilities via a logistic function, then back-solves numerically to find `(ExpHG, ExpAG)` pairs via `find_xg_values()`. Not integrated into the main PL pipeline.
