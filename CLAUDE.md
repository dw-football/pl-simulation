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

An interactive Shiny app at `app.R` wraps the full simulation pipeline in a multi-league, multi-season UI with 6 tabs: League Table, Relegation Race, Title/Top Spots, Team Focus, Match Impact, Extra Games & Adjustments.

**Run it:** `shiny::runApp("app.R")` (or use `run_app.bat` on Windows — sets RSTUDIO_PANDOC and lib path)

**Key files:** `app.R`, `code/league_configs.R`, `data/extra_games.csv`, `data/point_deductions.csv`

**Test Season:** the "Test Season (always runnable)" league (`data/TEST.csv`, a frozen 190-played/190-remaining EPL half-season, `frozen = TRUE` in `LEAGUE_CONFIGS`) always has games to simulate — use it to smoke-test the app in the off-season. The "Download Latest Data" button is blocked for `frozen` leagues so the fixture can't be overwritten.

Do not modify: `code/soccer_sim_functions.R`, `code/library_calls.R`, `code/main.R`, `code/get_pl_data.R`, `code/simulate.R`

## renv / Cross-Machine

- `run_app.bat` self-heals: git pull → **explicit** renv activation → `renv::restore()`. A User-scope `R_PROFILE_USER` (→ `G:/Computing/R/.Rprofile`) hijacked the profile slot and silently defeated renv auto-activation. **Fixed on 2W 2026-06-11** (var unset, defaults moved to a local `~/Documents/.Rprofile`); 520 + laptop queued in the vault's `Claude setup` RESUME list. Until all three are confirmed fixed, keep the explicit `setwd(project); source('renv/activate.R')` pattern in any script doing renv ops — it's a harmless no-op on fixed machines.
- **No Rtools on the work machines (520, laptop)** — every `renv.lock` pin must have a CRAN *Windows binary*. CRAN serves binaries only for current versions; once a pinned version is archived, restore falls back to source → compile → fails. If a fresh restore fails on a batch of "install failed" packages with compiled code, that's this. Fix: re-pin the stale packages to current binary versions and `renv::record()` them (done 2026-06-10 for 54 pins, commit `c66519e`).

Full design notes, reactive logic, tab specs, and `league_configs.R` function reference: see `docs/ARCHITECTURE.md`.

## Related Locations (off-repo)

Code lives here (`~/src/pl-simulation`, git); documents and archived data stay on Google Drive:

- **Archived Maccabi prototype data** → `~/My Drive/Soccer/Maccabi/Data/` — the pre-R Excel Monte Carlo (`Maccabi simulation 2023.xlsx`) + 34 data files removed from this repo 2026-06-07.
- **Old Drive copy** `G:\Soccer\pl-simulation` — DEPRECATED orphan, pending deletion (~2026-06-16, tombstone will point back here). Never edit or run from it.
- **Migration history/plan** → vault note `Personal/tech/Code off Drive - git migration plan.md`.

---

## Key Conventions

- Function names use dot-separated lowercase: `create.league.table()`, `simulate.many.seasons()`
- `data.table` is used in performance-critical functions (`summarize.one.season.results`, `create.finishing.odds.table.chat`) alongside tidyverse
- The `neutralize = TRUE` flag equalizes all xG to 1.0 (useful as a model sanity check)
- Games can be manually added via `add.game()` or forced to specific outcomes in `get_pl_data.R` before computing remaining matches
- Point deductions (e.g., Everton, Forest) are applied directly to `league_table$Points` after `create.league.table()`
