# pl-simulation

A Monte Carlo simulation model for football (soccer) league seasons. Given match results to date, it simulates all remaining fixtures using Poisson-distributed goal scoring based on historical attack/defense strength, and produces finishing-position probability tables.

Primarily used for the English Premier League. Also supports the Championship, Danish Superliga, and custom tournaments (Maccabi Games).

---

## Prerequisites

**R packages** (installed via `install.packages`):

```r
dplyr, tidyverse, reshape2, scales, ggplot2, gridExtra,
formattable, htmltools, webshot, readr, data.table,
future, furrr, flextable, kableExtra
```

**Quarto** — required only to render the HTML/PDF report.
Install from <https://quarto.org/docs/get-started/>.

---

## Data

Match results come from [football-data.co.uk](http://www.football-data.co.uk/englandm.php).

Download the current season's file and save it as `data/E0.csv`.
For the Championship use `data/E1.csv` and adjust the read path in `code/get_pl_data.R`.

The file must contain at minimum these columns:

| Column | Description |
|--------|-------------|
| `HomeTeam` | Home team name |
| `AwayTeam` | Away team name |
| `FTR` | Full-time result: `H`, `D`, or `A` |
| `FTHG` | Full-time home goals |
| `FTAG` | Full-time away goals |

---

## Workflow

### Phase 1 — Build the league table and remaining fixtures

```r
# Run from the project root in R
source("code/main.R")
```

This sources `library_calls.R` → `soccer_sim_functions.R` → `get_pl_data.R` → `simulate.R` → `analyze.R` in sequence.

`get_pl_data.R` reads `data/E0.csv`, builds the current league table, computes expected goals for all remaining fixtures, and writes:
- `data/league_table.csv`
- `data/sorted_league_table.csv`
- `data/rem_matches.csv`

**Before running**, edit `get_pl_data.R` to:
- Add games played but not yet in the CSV via `add.game()`
- Apply any point deductions directly to `league_table$Points`

### Phase 2 — Run simulations

`simulate.R` (sourced automatically by `main.R`) reads the three CSVs and runs `num_sims` (default 5000) Monte Carlo simulations. Results are saved to `data/sim_data.Rdata`.

To run simulations independently (e.g. after reloading saved data):

```r
source("code/simulate.R")
```

### Phase 3 — Render the report

```bash
quarto render Premier_League_Simulation.qmd
```

The Quarto document loads `sim_data.Rdata` and the CSVs to produce a formatted HTML/PDF report. It does **not** re-run the simulations.

---

## Key outputs

| Output | Description |
|--------|-------------|
| `table.png` | Formatted league odds table (Win / Top4 / Top5 / Top6 / Top7 / Top8 / Rel %) |
| `relegation.png` | Relegation probability chart for selected teams |
| `top of table.png` | Points-vs-rank distribution for selected top teams |
| `data/sim_data.Rdata` | Saved simulation results (`all_scores`, `all_sims`) |

---

## File structure

```
pl-simulation/
├── code/
│   ├── main.R                   # Entry point — sources all phases
│   ├── library_calls.R          # Package imports
│   ├── soccer_sim_functions.R   # All core simulation functions
│   ├── get_pl_data.R            # Data ingestion and league table build
│   ├── simulate.R               # Runs Monte Carlo simulations
│   ├── analyze.R                # Post-simulation analysis and charts
│   ├── elo_calcs.R              # Alternative Elo-based xG method (not in main pipeline)
│   └── denmark.R                # Danish Superliga variant
├── data/
│   ├── E0.csv                   # Source match results (download from football-data.co.uk)
│   ├── league_table.csv         # Generated: current table with xG stats
│   ├── sorted_league_table.csv  # Generated: league table sorted by position
│   ├── rem_matches.csv          # Generated: remaining fixtures with expected goals
│   └── sim_data.Rdata           # Generated: simulation results
├── docs/
│   ├── architecture.md          # Technical architecture and design notes
│   └── data-dictionary.md       # Column schemas for all data files
├── output/                      # Historical output images
├── Premier_League_Simulation.qmd
├── CLAUDE.md                    # Agent instructions (Claude Code)
└── README.md
```

---

## Ad-hoc analysis

After running simulations, you can query results interactively:

```r
# Load saved results
load("./data/sim_data.Rdata")
league_table        <- read.csv("./data/league_table.csv")
sorted_league_table <- read.csv("./data/sorted_league_table.csv")
num_sims            <- max(all_sims$SimNo)

# Main odds table
t <- create.538.table(all_sims, sorted_league_table)
print.538.flextable(t)

# Relegation odds
create.finishing.odds.table(all_sims, 17, ">")

# How does one match result shift the odds?
permutate.a.result(all_scores, league_table, num_sims,
                   "Arsenal", "Chelsea", 4, "<")

# Rank all remaining games by how much they matter
rank.games.by.importance(all_scores, league_table, num_sims, rem_matches)
```

See `code/analyze.R` for more examples.
