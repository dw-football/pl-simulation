# Data Dictionary

All generated files live in `data/`. The source file must be downloaded manually.

---

## `data/E0.csv` — Source match results (input)

Downloaded from [football-data.co.uk](http://www.football-data.co.uk/englandm.php). Only the columns below are used; the file contains many others (betting odds, shots, etc.) that are ignored.

| Column | Type | Description |
|--------|------|-------------|
| `HomeTeam` | character | Home team name (must be consistent across all rows) |
| `AwayTeam` | character | Away team name |
| `FTR` | character | Full-time result: `"H"` (home win), `"D"` (draw), `"A"` (away win) |
| `FTHG` | integer | Full-time home goals |
| `FTAG` | integer | Full-time away goals |

**Notes:**
- Team names are used as join keys throughout the pipeline. A team name that changes mid-season (e.g. a rename) will create a new team entry.
- Games not yet in the CSV can be added in `get_pl_data.R` via `add.game()`.
- `E1.csv` has the same schema and is used for the Championship.

---

## `data/league_table.csv` — Current season table (generated)

Written by `get_pl_data.R` → `create.league.table()`. One row per team, alphabetically ordered.

| Column | Type | Description |
|--------|------|-------------|
| `Team` | character | Team name |
| `Wins` | integer | Total wins (home + away) |
| `Draws` | integer | Total draws |
| `Losses` | integer | Total losses |
| `Points` | integer | League points (3 × wins + draws). Point deductions applied here. |
| `GD` | integer | Goal difference (TGS − TGC) |
| `TGS` | integer | Total goals scored |
| `TGC` | integer | Total goals conceded |
| `HGS` | double | Home goals scored **per game** |
| `HGC` | double | Home goals conceded **per game** |
| `AGS` | double | Away goals scored **per game** |
| `AGC` | double | Away goals conceded **per game** |

**Notes:**
- `HGS`, `HGC`, `AGS`, `AGC` are the per-game rates used to compute xG for remaining fixtures.
- `TGS` and `TGC` are raw totals used as the base for each simulation's points accumulation.
- The CSV has a row-number index column (unnamed, first column) added by `write.csv()`.

---

## `data/sorted_league_table.csv` — Sorted table (generated)

Written by `get_pl_data.R` → `create.sorted.league.table()`. Same data as `league_table.csv` but sorted by position and with renamed/added columns.

| Column | Type | Description |
|--------|------|-------------|
| `Team` | character | Team name |
| `Played` | integer | Games played (Wins + Draws + Losses) |
| `Wins` | integer | Total wins |
| `Draws` | integer | Total draws |
| `Losses` | integer | Total losses |
| `Points` | integer | League points |
| `GD` | integer | Goal difference |
| `Scored` | integer | Total goals scored (= `TGS` in league_table.csv) |
| `Conceded` | integer | Total goals conceded (= `TGC` in league_table.csv) |
| `HGS` | double | Home goals scored per game |
| `HGC` | double | Home goals conceded per game |
| `AGS` | double | Away goals scored per game |
| `AGC` | double | Away goals conceded per game |

**Notes:**
- Sorted descending by `Points`, then `GD`, then `Scored`.
- This is the table used as the input to `create.538.table()` for the output report.

---

## `data/rem_matches.csv` — Remaining fixtures with expected goals (generated)

Written by `get_pl_data.R` → `determine.remaining.matches()`. One row per unplayed fixture.

| Column | Type | Description |
|--------|------|-------------|
| `MatchNo` | integer | Sequential match number (1-indexed from total fixtures, so early values are already played) |
| `HomeTeam` | character | Home team name |
| `AwayTeam` | character | Away team name |
| `HG` | double | Season-mean home goals (used as denominator baseline; = `mean(FTHG)` over all played games) |
| `AG` | double | Season-mean away goals (= `mean(FTAG)` over all played games) |
| `TG` | double | Average of `HG` and `AG` |
| `GS.by.H` | double | Home team's `HGS` (home goals scored per game) |
| `GC.by.H` | double | Home team's `HGC` (home goals conceded per game) |
| `GS.by.A` | double | Away team's `AGS` (away goals scored per game) |
| `GC.by.A` | double | Away team's `AGC` (away goals conceded per game) |
| `ExpHG` | double | Expected home goals for this fixture: `(GS.by.H / HG) × GC.by.A` |
| `ExpAG` | double | Expected away goals for this fixture: `(GS.by.A / AG) × GC.by.H` |

**Notes:**
- `ExpHG` and `ExpAG` are the Poisson lambda values used in `simulate.many.seasons()`.
- `HG`, `AG`, `TG`, `GS.by.H`, `GC.by.H`, `GS.by.A`, `GC.by.A` are intermediate calculation columns retained for auditability but not used downstream.

---

## `data/sim_data.Rdata` — Simulation results (generated)

Written by `simulate.R` via `save()`. Loading it restores two objects into the R environment.

### `all_scores`

A `data.table` with one row per simulated match. Size: `numSims × nRemaining` rows (typically ~500,000 for 5000 sims mid-season).

| Column | Type | Description |
|--------|------|-------------|
| `SimNo` | integer | Simulation number (1 to `numSims`) |
| `HomeTeam` | character | Home team name |
| `AwayTeam` | character | Away team name |
| `HG` | integer | Simulated home goals (drawn from `rpois(ExpHG)`) |
| `AG` | integer | Simulated away goals (drawn from `rpois(ExpAG)`) |

**Notes:**
- Used by `permutate.a.result()` to override a specific fixture's scores and re-rank.
- Used by `rank.games.by.importance()` similarly.
- Grouped by `SimNo` in `calc.points.and.rank()`.

### `all_sims`

A `data.table` with one row per team per simulation. Size: `numTeams × numSims` rows (e.g. 100,000 for 20 teams × 5000 sims).

| Column | Type | Description |
|--------|------|-------------|
| `SimNo` | integer | Simulation number (1 to `numSims`) |
| `Team` | character | Team name |
| `Pts` | integer | Final points total (base points + simulated points) |
| `GS` | integer | Final goals scored total |
| `GC` | integer | Final goals conceded total |
| `GD` | integer | Final goal difference (`GS - GC`) |
| `Rank` | integer | Final league position (1 = champion, 20 = last) |

**Notes:**
- `Pts`, `GS`, `GC` are cumulative totals: base season stats + simulated remaining matches.
- `Rank` is assigned after sorting each `SimNo` group by `Pts DESC, GD DESC, GS DESC`.
- All finishing-probability functions (`create.finishing.odds.table`, `create.538.table`) operate on this object.

---

## In-memory objects used in `analyze.R`

These are not persisted to disk but are created from the above files during an interactive session.

| Object | Source | Description |
|--------|--------|-------------|
| `league_table` | `read.csv("data/league_table.csv")` | Unsorted table; needed by `calc.points.and.rank()` for base points |
| `sorted_league_table` | `read.csv("data/sorted_league_table.csv")` | Sorted table; needed by `create.538.table()` for display order |
| `num_sims` | `max(all_sims$SimNo)` | Total number of simulations run |
| `t` | `create.538.table(all_sims, sorted_league_table)` | Formatted odds table data frame |
