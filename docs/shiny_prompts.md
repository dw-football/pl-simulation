# Shiny App Build — Prompts

Each prompt is a self-contained implementation step. Complete one, test it in
RStudio, then move to the next. See `shiny_todo.md` for the checklist.

---

## Prompt 1 — `code/league_configs.R`: league configs + helpers

Create `code/league_configs.R` containing:

1. **`LEAGUE_CONFIGS`** — a named list with one entry per supported league.
   Each entry has:
   - `code`      — football-data.co.uk code (e.g. `"E0"`)
   - `file`      — local CSV path (e.g. `"data/E0.csv"`)
   - `num_teams` — total teams in the league
   - `zones`     — named list of finishing zones; each zone is
     `list(placement = N, operator = "==" | "<" | ">")`
     matching the semantics of `create.finishing.odds.table()`.
     The **last** zone is always the danger zone (relegation/drop).

   Leagues to include:
   - Premier League (E0, 20 teams): Win, Top4, Top5, Top6, Top7, Top8, Rel (pos > 17)
   - Championship (E1, 24 teams): Win, Promoted (pos < 3), Playoff (pos < 7), Rel (pos > 21)
   - Serie A (I1, 20 teams): Win, Top4, Top6, Rel (pos > 17)
   - Serie B (I2, 20 teams): Win, Promoted (pos < 3), Playoff (pos < 9), Rel (pos > 17)
   - Bundesliga (D1, 18 teams): Win, Top4, Top6, Rel (pos > 16)
   - 2. Bundesliga (D2, 18 teams): Win, Promoted (pos < 3), Playoff (pos == 4), Rel (pos > 16)
   - La Liga (SP1, 20 teams): Win, Top4, Top6, Rel (pos > 17)
   - Ligue 1 (F1, 18 teams): Win, Top3 (pos < 4), Top5 (pos < 6), Rel (pos > 15)

2. **`current_season()`** — returns a 4-character season code based on today's
   date. Month >= 7 → season started this calendar year; month < 7 → started
   last year. Example: returns `"2526"` for the 2025-26 season.

3. **`make_download_url(season_code, league_code)`** — builds a
   football-data.co.uk download URL.
   Example: `make_download_url("2526", "E0")` →
   `"https://www.football-data.co.uk/mmz4281/2526/E0.csv"`

4. **`make.configurable.538.table(all_sims, sorted_lt, zones)`** — generalised
   version of `create.538.table()`. Loops over the zones list, calls
   `create.finishing.odds.table()` for each zone, joins the Percent column onto
   `sorted_lt`, formats values to 1 decimal place (NA → `""`), adds a `Rank`
   column, and returns columns: Rank, Team, Played, Points, GD, [zone cols…].

5. **`make.538.flextable(t, zones)`** — turns the output of
   `make.configurable.538.table()` into a styled flextable object (does NOT
   save to disk). Apply `theme_zebra(odd_body = "#EFEFEF", even_body = "white")`,
   right-align numerics, left-align Team, colour negative GD red, colour good
   zone values >= 50 green, colour danger zone values >= 50 red, draw vertical
   borders after the GD column and after the second-to-last column.
   Convention: last zone in the list = danger zone.

6. **`ft_to_html(ft)`** — converts a flextable to an HTML string suitable for
   `renderUI`. Uses `flextable::save_as_html()` to a temp file then reads it
   back with `readLines()` and wraps in `htmltools::HTML()`.

**Test:** `source("code/league_configs.R")` — no errors.

---

## Prompt 2 — `app.R` skeleton: UI structure, sourcing, 6 empty tabs

Create `app.R` in the project root. No reactivity yet — just the structure.

Startup block (before `ui`):
```r
library(shiny)
library(bslib)
library(DT)
source("code/library_calls.R")
source("code/soccer_sim_functions.R")
source("code/league_configs.R")
```

**UI:** `page_sidebar()` with `bs_theme(bootswatch = "flatly")`.

Sidebar (width = 260):
- `selectInput("league", ...)` — choices from `names(LEAGUE_CONFIGS)`,
  default `"Premier League"`
- `selectInput("season", ...)` — choices `c("2025-26"="2526", "2024-25"="2425",
  "2023-24"="2324")`, default `current_season()`
- `actionButton("download_data", "Download Latest Data", class="btn-primary w-100")`
- `textOutput("data_status")`
- `hr()`
- `sliderInput("num_sims", ...)` — 500 to 20000, default 10000, step 500
- `actionButton("run_sim", "Run Simulation", class="btn-success w-100")`
- `textOutput("sim_status")`
- `hr()`
- `textOutput("match_summary")`

Main area: `navset_tab()` with 6 `nav_panel()` tabs, each containing only a
placeholder `p("Coming soon.")`:
1. League Table
2. Relegation Race
3. Title / Top Spots
4. Team Focus
5. Match Impact
6. Extra Games & Adjustments

**Server:** empty `function(input, output, session) {}`.

**Test:** `shiny::runApp("app.R")` — app opens, sidebar and 6 tabs visible,
no errors.

---

## Prompt 3 — Data download + auto-load: `match_data` reactive, download button, status

Add reactivity for data loading to `app.R`.

1. **`match_data <- reactiveVal(NULL)`**

2. **Auto-load observer** — fires on startup and whenever `input$league` or
   `input$season` changes. Reads the league's local CSV if it exists
   (`dplyr::select(raw, HomeTeam, AwayTeam, FTR, FTHG, FTAG)`), sets
   `match_data(df)`. If file doesn't exist, sets `match_data(NULL)`.

3. **Download button handler** — `observeEvent(input$download_data, ...)`.
   Builds the URL from `make_download_url(input$season, cfg$code)`, calls
   `download.file(..., mode="wb")` inside `withProgress()`. On success, reads
   the file and calls `match_data(df)` plus a success notification. On error,
   shows an error notification (use `tryCatch`).

4. **`output$data_status`** — shows `"N matches loaded\nYYYY-MM-DD HH:MM"` when
   `match_data()` is not NULL (use `file.info()$mtime` for the timestamp).

**Test:** run app → status text appears if CSV already exists; Download button
refreshes it; switching leagues auto-loads the correct file.

---

## Prompt 4 — League table reactives: CSV stores, `all_matches`, `league_tables`, `rem_matches`, sidebar summary

1. **Create persistent CSV stores** at app startup (before `ui`), if they don't
   already exist:
   - `data/extra_games.csv` — columns: HomeTeam (chr), HomeGoals (int),
     AwayTeam (chr), AwayGoals (int)
   - `data/point_deductions.csv` — columns: Team (chr), Points (int),
     Reason (chr)

2. **`extra_games <- reactiveVal(...)`** — read `data/extra_games.csv` on
   startup with explicit `col_types` (HomeTeam=character, HomeGoals=integer,
   AwayTeam=character, AwayGoals=integer). Using explicit col_types prevents
   type-mismatch errors when the file is empty.

3. **`point_deductions <- reactiveVal(...)`** — read `data/point_deductions.csv`
   on startup.

4. **`all_matches <- reactive(...)`** — requires `match_data()`. Starts from
   `match_data()`, then for each row of `extra_games()` calls `add.game()`.
   Returns the augmented data frame.

5. **`league_tables <- reactive(...)`** — requires `all_matches()`. Calls
   `create.league.table()`, then applies each row of `point_deductions()` by
   subtracting points from the matching team, then calls
   `create.sorted.league.table()`. Returns `list(unsorted=lt, sorted=slt)`.

6. **`rem_matches_r <- reactive(...)`** — calls
   `determine.remaining.matches(all_matches(), league_tables()$unsorted)`.

7. **`output$match_summary`** — `"N played · M remaining"` text in the sidebar.

**Test:** run app → sidebar shows match/remaining counts; adding a row to
extra_games.csv and restarting updates the count.

---

## Prompt 5 — Simulation: Run button, `withProgress()`, `sim_results` reactive

1. **`sim_results <- reactiveVal(NULL)`**

2. **Clear observer** — `observe({ all_matches(); sim_results(NULL) })` — clears
   simulation results whenever the underlying data changes, forcing a re-run.

3. **Run simulation handler** — `observeEvent(input$run_sim, ...)`.
   Requires `rem_matches_r()` and `league_tables()`. Inside `withProgress()`:
   - Step 1 (10%): `simulate.many.seasons(rm, num_sims, neutralize=FALSE)`
   - Step 2 (70%): `calc.points.and.rank(all_scores, lt, num_sims)`
   - Step 3 (100%): done.
   Sets `sim_results(list(all_scores=..., all_sims=..., num_sims=...))`.

4. **`output$sim_status`** — shows `"Ready · N sims"` (using Unicode middle dot
   `\u00b7`) when `sim_results()` is not NULL.

**Test:** run app → click Run Simulation → progress bar appears → status updates
to "Ready · 10000 sims"; changing league clears status.

---

## Prompt 6 — Tab 1: `make.configurable.538.table()` + HTML flextable render + PNG download

Wire up the League Table tab using the helpers already defined in
`code/league_configs.R`.

**UI changes:** replace the Tab 1 placeholder with:
```r
nav_panel("League Table",
  br(),
  downloadButton("download_table_png", "Download as PNG"),
  br(), br(),
  div(style = "overflow-x: auto;", uiOutput("table_538"))
)
```

**Server:**

1. **`output$table_538 <- renderUI(...)`** — requires `sim_results()` and
   `league_tables()`. Calls `make.configurable.538.table()`, then
   `make.538.flextable()`, then `ft_to_html()`. Returns the HTML object.

2. **`output$download_table_png <- downloadHandler(...)`** — filename is
   `"{league}_table.png"`. Content function calls `make.configurable.538.table()`,
   `make.538.flextable()`, then `flextable::save_as_image(ft, path=file)`.

**Test:** run sim → League Table tab shows styled zebra table with correct zone
columns per league; negative GD is red; values >= 50 are green/red; Download
PNG saves a clean image.

---

## Prompt 7 — Tabs 2 & 3: Relegation Race + Title/Top Spots checkbox plots

**UI for Tab 2 (Relegation Race):**
```r
fluidRow(
  column(3,
    h5("Select Teams"),
    uiOutput("rel_team_checkboxes"),
    hr(),
    numericInput("rel_cutoff", "Relegation if finish lower than:",
                 value = 17, min = 1, max = 30)
  ),
  column(9,
    downloadButton("download_rel_png", "Save as PNG"),
    br(), br(),
    plotOutput("rel_plot", height = "600px")
  )
)
```

**UI for Tab 3 (Title / Top Spots):**
```r
fluidRow(
  column(3,
    h5("Select Teams"),
    uiOutput("top_team_checkboxes"),
    hr(),
    uiOutput("zone_selector_ui"),
    br(),
    p(em("Zone odds for selected teams:")),
    tableOutput("top_zone_odds")
  ),
  column(9,
    downloadButton("download_top_png", "Save as PNG"),
    br(), br(),
    plotOutput("top_plot", height = "600px")
  )
)
```

**Server — Tab 2:**

1. **`rel_default_cutoff(league_name)`** helper — returns the `placement` value
   from the last zone in `LEAGUE_CONFIGS[[league_name]]$zones`. This is the
   relegation position threshold.

2. **Observer** — `observe({ updateNumericInput(session, "rel_cutoff", value =
   rel_default_cutoff(input$league)) })` — keeps the cutoff in sync when the
   league changes.

3. **`output$rel_team_checkboxes`** — `checkboxGroupInput` of all teams,
   pre-selected = bottom 6 by current table position.

4. **`rel_plots <- reactive(...)`** — for each selected team, builds a ggplot
   showing the points distribution (red bars) with the safe/relegated split
   (green = non-relegated share). Subtitle shows mean pts and relegation %.
   Returns a list of ggplot objects.

5. **`output$rel_plot`** — `gridExtra::grid.arrange()` of `rel_plots()` in a
   square-ish grid.

6. **`output$download_rel_png`** — `gridExtra::arrangeGrob()` + `ggsave()`,
   10×8 inches, 150 dpi.

**Server — Tab 3:**

7. **`output$top_team_checkboxes`** — pre-selected = top 6.

8. **`output$zone_selector_ui`** — `selectInput("top_zone", ...)` populated with
   all zones except the last (danger) zone, using `names(cfg$zones)[-length]`.

9. **`top_plots <- reactive(...)`** — `lapply` over selected teams calling
   `plot.points.vs.rank(team, all_sims, num_sims, num_teams)`.

10. **`output$top_plot`** — `grid.arrange()` of `top_plots()`.

11. **`output$top_zone_odds <- renderTable(...)`** — for the selected zone,
    calls `create.finishing.odds.table()` and filters to selected teams. Shows
    Team and zone % columns, sorted descending. Use `digits=1` to show 1 decimal
    place.

12. **`output$download_top_png`** — same pattern as relegation download.

**Test:** select teams, change cutoff, switch leagues — all plots and labels
update correctly. Zone odds table shows 1 decimal place.

---

## Prompt 8 — Tab 4: Team Focus (combined view + text summary)

**UI:**
```r
nav_panel("Team Focus",
  fluidRow(
    column(3,
      uiOutput("team_focus_selector"),
      br(),
      h5(textOutput("team_focus_summary")),
      uiOutput("team_focus_rel_table_ui")
    ),
    column(9,
      downloadButton("download_focus_png", "Save as PNG"),
      br(), br(),
      uiOutput("team_focus_plots_ui"),
      hr(),
      h5("This team in the league table:"),
      div(style = "overflow-x: auto;", uiOutput("team_focus_table_row"))
    )
  )
)
```

**Server:**

1. **`ordinal_suffix(n)`** helper — returns e.g. `"3rd"`, `"11th"`, `"21st"`.

2. **`output$team_focus_selector`** — `selectInput("focus_team", ...)` from
   sorted table teams.

3. **`output$team_focus_summary`** — text: `"{Team} — currently Nth with X pts
   | most likely finish: Mth | expected final pts: P"`.

4. **`team_rel_data <- reactive(...)`** — computes relegation data for the focused
   team: filters `all_sims`, computes `relegated = Rank > cutoff`, builds `trr`
   summary (Pts_likelihood, Rel_likelihood, Non_rel_odds per points value),
   returns `list(ts, trr, rel_odds, mean_pts, cutoff)`.

5. **`output$team_focus_plots_ui <- renderUI(...)`** — if `rel_odds >= 0.5%`:
   two plots side by side (col(6) each); otherwise one full-width plot (col(12)).

6. **`team_rank_ggplot <- reactive(...)`** — calls `plot.points.vs.rank()`.
   Shared between `renderPlot` and `downloadHandler`.

7. **`team_rel_ggplot <- reactive(...)`** — same bar chart style as Tab 2 but
   for the single focused team. Shared between `renderPlot` and `downloadHandler`.

8. **`output$team_focus_rank_plot`** — renders `team_rank_ggplot()`.

9. **`output$team_focus_rel_plot`** — renders `team_rel_ggplot()` only when
   `rel_odds >= 0.5`.

10. **`output$team_focus_rel_table_ui`** — conditional: if `rel_odds < 0.5`,
    return `NULL`. Otherwise show a `tableOutput("team_focus_rel_table")` with
    heading showing the overall rel %.

11. **`output$team_focus_rel_table`** — table of Final Pts vs Rel % for all
    points values where Rel % > 0. Use `as.integer(Pts)` for the Pts column.
    Threshold for showing this table = 0.5% (same as the chart).

12. **`output$team_focus_table_row`** — builds the full 538 table, filters to
    just the focused team's row, drops zone columns where value is `"0.0"` or
    `""`, renders via `make.538.flextable()` + `ft_to_html()`.

13. **`output$download_focus_png`** — PNG layout:
    - Title grob (full width): team name + current position/pts + likely finish
    - If `rel_odds >= 0.5`: left column (65%) = rank plot + rel plot stacked;
      right column (35%) = relegation pts table (full height)
    - If `rel_odds < 0.5`: single rank plot, 9×6 inches
    - Full layout: 12×10 inches, 150 dpi

**Test:** focus on a top team → 1 plot, no relegation table. Focus on a bottom
team → 2 plots + rel table. PNG downloads look correct and nothing is cut off.

---

## Prompt 9 — Tab 5: Match Impact (permutate all zones into one table)

**UI:**
```r
nav_panel("Match Impact",
  fluidRow(
    column(4,
      uiOutput("impact_team_filter"),
      uiOutput("impact_fixture_selector"),
      br(),
      p(em("Shows how each possible result shifts finishing odds for both
            teams across every zone.")),
      p(em("Note: may take 15-30 seconds to compute."))
    ),
    column(8,
      h5(textOutput("impact_title")),
      DT::DTOutput("impact_table")
    )
  )
)
```

**Server:**

1. **`output$impact_team_filter`** — `selectInput("impact_team", "Filter by team:",
   choices = c("All teams", sorted teams from rem_matches_r()))`.

2. **`output$impact_fixture_selector`** — `selectInput("impact_fixture", ...)`.
   Values are `"HomeTeam||AwayTeam"` (double-pipe separator). Labels are
   `"HomeTeam vs AwayTeam"` (plain, no "Home:"/"Away:" prefixes). When a team
   filter is active, show that team's home fixtures first then away fixtures.

3. **`output$impact_title`** — `"Match Impact: Home vs Away"`.

4. **`output$impact_table <- DT::renderDT(...)`** — the main computation:
   - Parse home/away from `input$impact_fixture`.
   - Build a `force_result(scores, hg, ag)` helper that replaces the goals for
     this specific fixture in `all_scores`.
   - Call `calc.points.and.rank()` exactly 3 times — once per outcome (home win
     2-1, draw 1-1, away win 1-2). Do NOT call it once per zone.
   - For each zone × each of {home team, away team}: get finishing odds under
     each outcome using `create.finishing.odds.table()`. Skip rows where
     max(hw, draw, aw) < 0.5 (irrelevant zones for this team).
   - Build result as a data frame: Zone, Team, "Home Win %", "Draw %",
     "Away Win %". Use nested `lapply` (not a `for` loop inside `lapply`).
   - Render as `DT::datatable()` with `DT::formatRound(..., digits=1)`.
   - Wrap the 3 `calc.points.and.rank()` calls in `withProgress()`.

**Test:** select a fixture → table appears with rows for each relevant zone ×
team. Home win 2-1 / Draw 1-1 / Away win 1-2 columns shown. No "Home:"/"Away:"
prefixes anywhere. Switching fixtures updates correctly.

---

## Prompt 10 — Tab 6: Extra Games & Point Deductions (DT tables, add/delete, wires into reactives)

**UI:**
```r
nav_panel("Extra Games & Adjustments",
  fluidRow(
    column(6,
      h4("Manual Entry Games"),
      p("Add games played but not yet in the downloaded CSV.",
        "Changes update the league table immediately;",
        "re-run simulation to update odds."),
      DT::DTOutput("extra_games_dt"),
      br(),
      h5("Record a Played Fixture"),
      uiOutput("eg_team_filter_ui"),
      uiOutput("eg_fixture_ui"),
      fluidRow(
        column(6, uiOutput("eg_home_goals_ui")),
        column(6, uiOutput("eg_away_goals_ui"))
      ),
      actionButton("add_game_btn", "Add Result", class = "btn-primary")
    ),
    column(6,
      h4("Point Deductions"),
      p("Apply point deductions. Changes update the league table immediately."),
      DT::DTOutput("point_ded_dt"),
      br(),
      h5("Add a Deduction"),
      uiOutput("pd_team_ui"),
      fluidRow(
        column(6, numericInput("pd_points", "Points to deduct", 1, 1, 30)),
        column(6, textInput("pd_reason", "Reason", ""))
      ),
      actionButton("add_ded_btn", "Add Deduction", class = "btn-warning")
    )
  ),
  tags$script(HTML("
    $(document).on('click', '.delete-eg', function() {
      Shiny.setInputValue('delete_eg_row', $(this).data('row'),
                          {priority: 'event'});
    });
    $(document).on('click', '.delete-pd', function() {
      Shiny.setInputValue('delete_pd_row', $(this).data('row'),
                          {priority: 'event'});
    });
  "))
)
```

**Server — Extra Games:**

1. **`output$eg_team_filter_ui`** — same pattern as Match Impact team filter:
   `c("All teams", sorted teams from rem_matches_r())`.

2. **`output$eg_fixture_ui`** — same fixture picker pattern as Match Impact.
   Values = `"HomeTeam||AwayTeam"`, labels = `"HomeTeam vs AwayTeam"`.
   Picks from **remaining** matches only (not free-form team entry).

3. **`output$eg_home_goals_ui`** / **`output$eg_away_goals_ui`** — `renderUI`
   outputting `numericInput` whose label is the team name parsed from
   `input$eg_fixture` (e.g. "Man City Goals" / "Arsenal Goals").

4. **`output$extra_games_dt`** — `DT::renderDT()` with a Delete button column.
   Each button: `<button class="btn btn-danger btn-sm delete-eg"
   data-row="{i}">Delete</button>`. Use `escape=FALSE`.

5. **`observeEvent(input$add_game_btn, ...)`** — parse home/away from
   `input$eg_fixture`, build a new row, `bind_rows()` to existing, write CSV,
   update `extra_games()` reactiveVal, show notification.

6. **`observeEvent(input$delete_eg_row, ...)`** — remove row at the given index,
   write CSV, update reactiveVal.

**Server — Point Deductions:**

7. **`output$pd_team_ui`** — `selectInput` from `teams_list()` reactive (all
   teams in current match data).

8. **`output$point_ded_dt`** — same DT pattern with `.delete-pd` class buttons.

9. **`observeEvent(input$add_ded_btn, ...)`** — add deduction row, write CSV,
   update `point_deductions()`, show notification.

10. **`observeEvent(input$delete_pd_row, ...)`** — remove row, write CSV, update
    reactiveVal.

**Key notes:**
- Both CSV stores persist across app restarts — this is by design.
- Read `extra_games.csv` with explicit `col_types` to prevent type errors on
  empty files: `cols(HomeTeam=col_character(), HomeGoals=col_integer(), ...)`.
- Adding/removing games updates `all_matches()` and `league_tables()` immediately
  but `sim_results` is cleared (user must re-run simulation for updated odds).

**Test:** add a manual game → league table updates, remaining matches count
decreases. Delete the game → reverts. Add a point deduction → table reorders.
Close and reopen app → manual games and deductions persist.
