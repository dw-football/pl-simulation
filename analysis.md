# Codebase Analysis & Performance Notes

## What the code does

The main pipeline is a Monte Carlo simulation of a football season's remaining fixtures using a **Dixon-Coles-style attack/defense strength model** (without the low-score correction). The xG formula is:

```
ExpHG = (HomeTeam's HGS per game / league mean HG) × AwayTeam's AGC per game
ExpAG = (AwayTeam's AGS per game / league mean AG) × HomeTeam's HGC per game
```

Goals are drawn from `rpois()` with those lambdas. This runs 5000 times; the outer loop accumulates ranked final standings, which are then used to produce finishing odds tables and charts.

The files outside the core PL simulation:
- `elo_calcs.R` — an alternative xG method driven by Elo ratings; only used in `denmark.R`
- `denmark.R` — Danish Superliga simulation, uses the Elo-based xG instead of historical strength
- `Denmark transition matrix.R` — a completely separate Markov chain simulation for financial/economic modeling (nothing to do with football)
- `formattable.R` — standalone version of `export_formattable()`, duplicated in `soccer_sim_functions.R`
- `analyze.R` — ad hoc analysis script showing typical post-simulation queries

---

## Design observations

**Things that work well:**
- The attack × defense strength formula is clean and produces sensible xGs
- `data.table` was correctly adopted for the inner loop of `summarize.one.season.results()` — this was a big win
- The `permutate.a.result()` function is the most useful analytical tool; it surfaces what a single match result means for finishing positions
- The `neutralize = TRUE` flag as a model sanity check is a smart diagnostic

**Issues:**

1. `simulate.many.seasons()` pre-allocates `allScores` as a data frame on lines 342–346 and then immediately discards it by overwriting it in the `lapply` block. The pre-allocation does nothing.

2. `simulate.many.games()` has a bug: the function receives `df` as a parameter but writes results into `data` (undefined). This function isn't part of the main pipeline so it goes unnoticed.

3. `create.finishing.odds.table()` and `create.finishing.odds.table.chat()` do the same thing — the dplyr version and a data.table rewrite. The `.chat` version is faster but both exist and are called inconsistently.

4. `export_formattable()` is defined in both `soccer_sim_functions.R` and `formattable.R`.

5. `print.kbl.538()` has multiple commented-out lines and is marked as unused — it's dead code.

---

## Performance: Where the time goes

There are two major bottlenecks, and they compound each other.

### Bottleneck 1: `simulate.many.seasons()` — 5000 separate `rpois()` calls

```r
# current: called 5000 times, each drawing ~100 goals
simulate.one.season <- function(remMatches, neutralize = FALSE) {
  setDT(remMatches)  # <- converts to data.table 5000 times
  ...
  scores <- remMatches[, c("HG", "AG") := .(rpois(nGamesRem, ...), rpois(nGamesRem, ...))]
}
```

`setDT()` converts `remMatches` to a data.table on every single call. `rpois` is also called in a loop. R's `rpois` is vectorized — you can generate all 500,000 goals (5000 sims × ~100 matches × 2) in **two** calls.

### Bottleneck 2: `calc.points.and.rank()` — O(n²) filtering (the big one)

```r
simResults <- lapply(1:numSims, function(i) {
    res <- summarize.one.season.results(leagueTable, allScores[allScores$SimNo == i,])
    ...
})
```

With `numSims = 5000` and `allScores` having ~500,000 rows, the line `allScores[allScores$SimNo == i,]` does a **full linear scan** of 500,000 rows, 5000 times. That's 2.5 billion comparisons. It also calls `as.data.table()` inside `summarize.one.season.results()` 5000 times.

---

## The fix

Replace the entire loop with a single data.table group-by. Since `allScores` already has a `SimNo` column, there's no reason to loop — we can do it all at once.

### New `simulate.many.seasons()`

```r
simulate.many.seasons <- function(remMatches, numSims = 100, neutralize = FALSE) {
  n <- nrow(remMatches)
  if (neutralize) {
    hg <- rpois(n * numSims, 1)
    ag <- rpois(n * numSims, 1)
  } else {
    hg <- rpois(n * numSims, rep(remMatches$ExpHG, numSims))
    ag <- rpois(n * numSims, rep(remMatches$ExpAG, numSims))
  }
  data.table(
    SimNo    = rep(seq_len(numSims), each = n),
    HomeTeam = rep(remMatches$HomeTeam, numSims),
    AwayTeam = rep(remMatches$AwayTeam, numSims),
    HG = hg,
    AG = ag
  )
}
```

### New `calc.points.and.rank()`

```r
calc.points.and.rank <- function(allScores, leagueTable, numSims) {
  dt <- as.data.table(allScores)
  lt <- as.data.table(leagueTable)
  teams <- sort(lt$Team)

  home <- dt[, .(Pts = 3L * sum(HG > AG) + sum(HG == AG),
                 GS  = sum(HG), GC = sum(AG)),
             by = .(SimNo, Team = HomeTeam)]

  away <- dt[, .(Pts = 3L * sum(AG > HG) + sum(HG == AG),
                 GS  = sum(AG), GC = sum(HG)),
             by = .(SimNo, Team = AwayTeam)]

  new_goals <- rbindlist(list(home, away))[
    , .(Pts = sum(Pts), GS = sum(GS), GC = sum(GC)),
    by = .(SimNo, Team)]

  base <- lt[, .(Team, Pts = Points, GS = TGS, GC = TGC)]

  # Replicate base across all sims and combine
  base_all <- base[rep(seq_len(nrow(base)), numSims)
                   ][, SimNo := rep(seq_len(numSims), each = nrow(base))]

  combined <- rbindlist(list(base_all, new_goals))[
    , .(Pts = sum(Pts), GS = sum(GS), GC = sum(GC)),
    by = .(SimNo, Team)]
  combined[, GD := GS - GC]

  setorder(combined, SimNo, -Pts, -GD, -GS)
  combined[, Rank := seq_len(.N), by = SimNo]
  combined[order(SimNo, Team)]
}
```

### Expected speedup

The gains are biggest mid-season when there are many remaining matches. Expected **10–50× faster** depending on how many fixtures remain. `permutate.a.result()` calls `calc.points.and.rank()` three times (once per outcome), so it benefits three-fold from the fix.
