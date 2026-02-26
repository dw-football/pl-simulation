# Shiny App Build — Task Checklist

Each step is a self-contained prompt in `shiny_prompts.md`.
Complete one, test it, then move to the next.

- [x] **Prompt 1** — `code/league_configs.R`: league configs + `current_season()` helper
- [x] **Prompt 2** — `app.R` skeleton: UI structure, sourcing, 6 empty tabs (no reactivity)
- [x] **Prompt 3** — Data download + auto-load: `match_data` reactive, download button, status
- [x] **Prompt 4** — League table reactives: CSV stores, `all_matches`, `league_tables`, `rem_matches`, sidebar summary
- [x] **Prompt 5** — Simulation: Run button, `withProgress()`, `sim_results` reactive
- [x] **Prompt 6** — Tab 1: `make.configurable.538.table()` + HTML flextable render + PNG download
- [x] **Prompt 7** — Tabs 2 & 3: Relegation Race + Title/Top Spots checkbox plots
- [x] **Prompt 8** — Tab 4: Team Focus (combined view + text summary)
- [x] **Prompt 9** — Tab 5: Match Impact (permutate all zones into one table)
- [x] **Prompt 10** — Tab 6: Extra Games & Point Deductions (DT tables, add/delete, wires into reactives)
