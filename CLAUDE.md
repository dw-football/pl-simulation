# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

A football (soccer) league simulation model that simulates remaining matches of a season using Poisson-distributed goal scoring based on historical attack/defense strength. Primarily used for the English Premier League but adaptable to other leagues (Championship, Danish Superliga, Maccabi Games).

## Running the Simulation

The workflow has two phases run from the project root:

**Phase 1 — Data preparation and remaining match generation:**
```r
source("code/main.R")
```
This sources `library_calls.R` → `soccer_sim_functions.R` → `get_pl_data.R` → `simulate.R` in sequence. `get_pl_data.R` reads `data/E0.csv`, builds the league table, computes remaining matches, and writes `data/league_table.csv`, `data/sorted_league_table.csv`, and `data/rem_matches.csv`.

**Phase 2 — Run simulations:**
`simulate.R` reads the CSVs and runs `num_sims` (default 5000) Monte Carlo simulations, saving results to `data/sim_data.Rdata`.

**Phase 3 — Render report:**
```bash
quarto render Premier_League_Simulation.qmd
```
The Quarto doc loads `sim_data.Rdata` and the CSVs to produce the formatted output (does not re-run simulations).

## Data Source

Match results CSV (`data/E0.csv`) comes from [football-data.co.uk](http://www.football-data.co.uk/englandm.php). Key columns used: `HomeTeam`, `AwayTeam`, `FTR`, `FTHG` (full-time home goals), `FTAG` (full-time away goals).

## Architecture

### Core simulation pipeline (`code/soccer_sim_functions.R`)

The simulation model uses historical home/away goals-per-game to compute **expected goals (xG)** for each remaining match:
- `ExpHG = (HomeTeam_HGS / league_mean_HG) * AwayTeam_AGC`
- `ExpAG = (AwayTeam_AGS / league_mean_AG) * HomeTeam_HGC`

Goals are then drawn from `rpois()` with these lambdas. Key functions:

| Function | Purpose |
|---|---|
| `create.league.table()` | Builds unsorted table from match data |
| `create.sorted.league.table()` | Sorts by Pts, GD, GS |
| `determine.remaining.matches()` | Finds unplayed fixtures and attaches xG |
| `simulate.one.season()` | Draws Poisson goals for all remaining matches |
| `simulate.many.seasons()` | Runs N simulations via `lapply` |
| `calc.points.and.rank()` | Aggregates simulated results into final points/rank per team per sim |
| `create.finishing.odds.table()` | Probability a team finishes at/above/below a position |
| `create.538.table()` | Builds the main output table (Win/Top4/Top5/Top6/Top7/Top8/Rel %) |
| `permutate.a.result()` | Shows how a single future match result shifts finishing odds |
| `add.game()` | Manually adds a result not yet in the CSV |

### Alternative expected goals method (`code/denmark/elo_calcs.R`)

A secondary Elo-based approach: converts Elo ratings to win probabilities, then back-solves to xG pairs via `find_xg_values()`. Used by `code/denmark/denmark.R` for the Danish Superliga simulation. Not integrated into the main PL pipeline. Data lives in `data/denmark/`.

### Output

- `print.538.flextable()` / `print.formatted.538()` — formatted league odds table saved to `table.png`
- `plot.points.vs.rank()` — points distribution chart colored by finishing rank
- `plot.relegation.odds()` — relegation probability chart for a specific team

## Shiny App

An interactive Shiny app lives at `app.R` in the project root. It wraps the full simulation pipeline in a multi-league, multi-season UI.

**Run it:**
```r
shiny::runApp("app.R")
```

**Key files added for the app:**

| File | Purpose |
|---|---|
| `app.R` | Full Shiny app (UI + server, ~950 lines) |
| `code/league_configs.R` | League metadata, URL helpers, table/flextable builders |
| `data/extra_games.csv` | Persistent store for manually added results (survives restarts) |
| `data/point_deductions.csv` | Persistent store for manual point deductions |

**Functions in `code/league_configs.R`:**

| Function | Purpose |
|---|---|
| `LEAGUE_CONFIGS` | Named list — 8 leagues, each with `code`, `file`, `num_teams`, `zones` |
| `current_season()` | Returns 4-char season code based on today's date (e.g. `"2526"`) |
| `make_download_url(season, code)` | Builds football-data.co.uk download URL |
| `make.configurable.538.table(all_sims, sorted_lt, zones)` | Generalised 538-style odds table for any league's zone config |
| `make.538.flextable(t, zones)` | Styled flextable from the above; last zone = danger (red), others green |
| `ft_to_html(ft)` | Converts flextable to `htmltools::HTML()` for `renderUI` |

**The 6 tabs:**
1. **League Table** — styled 538-style odds table with PNG download
2. **Relegation Race** — per-team points distribution plots (bottom N pre-selected)
3. **Title / Top Spots** — rank-coloured points plots (top N pre-selected) + zone odds table
4. **Team Focus** — single-team view: rank plot, relegation chart (if >0.5% risk), table row
5. **Match Impact** — pick a fixture; see how each outcome (HW/D/AW) shifts every zone's odds
6. **Extra Games & Adjustments** — add/delete manual results and point deductions; persists to CSV

**Design notes for the app:**
- `app.R` sources `code/library_calls.R`, `code/soccer_sim_functions.R`, `code/league_configs.R` at startup
- `extra_games` and `point_deductions` are `reactiveVal`s seeded from their CSVs on startup; changes write back immediately
- `sim_results` is cleared automatically whenever `all_matches()` changes (user must re-run)
- Match Impact calls `calc.points.and.rank()` exactly 3 times (once per outcome), not once per zone
- Do not modify: `code/soccer_sim_functions.R`, `code/library_calls.R`, `code/main.R`, `code/get_pl_data.R`, `code/simulate.R`

**Build history:** `docs/shiny_todo.md` (checklist) · `docs/shiny_prompts.md` (detailed spec for each step)

---

## Key Conventions

- Function names use dot-separated lowercase: `create.league.table()`, `simulate.many.seasons()`
- `data.table` is used in performance-critical functions (`summarize.one.season.results`, `create.finishing.odds.table.chat`) alongside tidyverse
- The `neutralize = TRUE` flag equalizes all xG to 1.0 (useful as a model sanity check)
- Games can be manually added via `add.game()` or forced to specific outcomes in `get_pl_data.R` before computing remaining matches
- Point deductions (e.g., Everton, Forest) are applied directly to `league_table$Points` after `create.league.table()`
