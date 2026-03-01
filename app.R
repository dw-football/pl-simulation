library(shiny)
library(bslib)
library(DT)

source("code/library_calls.R")
source("code/soccer_sim_functions.R")
source("code/league_configs.R")

# ── Create persistent CSV stores if they don't exist yet ─────────────────────

if (!file.exists("data/extra_games.csv")) {
  readr::write_csv(
    data.frame(HomeTeam  = character(), HomeGoals = integer(),
               AwayTeam  = character(), AwayGoals = integer()),
    "data/extra_games.csv"
  )
}
if (!file.exists("data/point_deductions.csv")) {
  readr::write_csv(
    data.frame(Team = character(), Points = integer(), Reason = character()),
    "data/point_deductions.csv"
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Football League Simulator",
  theme = bs_theme(bootswatch = "flatly"),

  # ── Sidebar ────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 260,

    selectInput("league", "League:",
                choices  = names(LEAGUE_CONFIGS),
                selected = "Premier League"),

    selectInput("season", "Season:",
                choices  = c("2025-26" = "2526",
                             "2024-25" = "2425",
                             "2023-24" = "2324"),
                selected = current_season()),

    actionButton("download_data", "Download Latest Data",
                 class = "btn-primary w-100"),

    textOutput("data_status"),

    hr(),

    sliderInput("num_sims", "Simulations:",
                min = 500, max = 20000, value = 10000, step = 500),

    actionButton("run_sim", "Run Simulation",
                 class = "btn-success w-100"),

    textOutput("sim_status"),

    hr(),

    textOutput("match_summary")
  ),

  # ── Main tabs ──────────────────────────────────────────────────────────────
  navset_tab(

    nav_panel("League Table",
      br(),
      downloadButton("download_table_png", "Download as PNG"),
      br(), br(),
      div(style = "overflow-x: auto;", uiOutput("table_538"))
    ),

    nav_panel("Relegation Race",
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
          plotOutput("rel_plot", height = "600px"),
          hr(),
          h5("Relegation odds — all teams at risk:"),
          tableOutput("rel_odds_table")
        )
      )
    ),

    nav_panel("Title / Top Spots",
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
    ),

    nav_panel("Team Focus",
      fluidRow(
        column(3,
          uiOutput("team_focus_selector"),
          br(),
          h5(textOutput("team_focus_summary")),
          uiOutput("team_focus_rel_table_ui")   # points-to-rel table if >5% risk
        ),
        column(9,
          uiOutput("team_focus_plots_ui"),       # conditional 1 or 2 plots
          downloadButton("download_focus_png", "Save as PNG"),
          hr(),
          h5("This team in the league table:"),
          div(style = "overflow-x: auto;", uiOutput("team_focus_table_row"))
        )
      )
    ),

    nav_panel("Match Impact",
      fluidRow(
        column(4,
          uiOutput("impact_team_filter"),
          uiOutput("impact_fixture_selector"),
          uiOutput("impact_watch_ui"),
          br(),
          p(em("Shows how each possible result shifts finishing odds for the two teams, plus any watched teams, across every zone.")),
          p(em("Note: may take 15-30 seconds to compute."))
        ),
        column(8,
          h5(textOutput("impact_title")),
          DT::DTOutput("impact_table")
        )
      )
    ),

    nav_panel("Extra Games & Adjustments",
      fluidRow(
        column(12,
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
        )
      ),
      hr(),
      fluidRow(
        column(12,
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
      # JS bridge: captures delete button clicks and sends row index to Shiny
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
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Reactive state ─────────────────────────────────────────────────────────

  match_data       <- reactiveVal(NULL)
  extra_games      <- reactiveVal(readr::read_csv("data/extra_games.csv",
                                                   col_types = readr::cols(
                                                     HomeTeam  = readr::col_character(),
                                                     HomeGoals = readr::col_integer(),
                                                     AwayTeam  = readr::col_character(),
                                                     AwayGoals = readr::col_integer()
                                                   )))
  point_deductions <- reactiveVal(readr::read_csv("data/point_deductions.csv",
                                                   show_col_types = FALSE))

  # ── Auto-load when league or season changes ────────────────────────────────
  # Fires on startup (initial values) and whenever either input changes.
  # Reads the local CSV if it exists; clears match_data if not.

  observe({
    cfg  <- LEAGUE_CONFIGS[[input$league]]
    file <- cfg$file
    if (file.exists(file)) {
      raw <- readr::read_csv(file, show_col_types = FALSE)
      df  <- dplyr::select(raw, HomeTeam, AwayTeam, FTR, FTHG, FTAG)
      match_data(df)
    } else {
      match_data(NULL)
    }
  })

  # ── Download button ────────────────────────────────────────────────────────

  observeEvent(input$download_data, {
    cfg  <- LEAGUE_CONFIGS[[input$league]]
    url  <- make_download_url(input$season, cfg$code)
    file <- cfg$file

    dir.create("data", showWarnings = FALSE)

    tryCatch({
      withProgress(message = "Downloading data...", value = 0.5, {
        download.file(url, destfile = file, mode = "wb", quiet = TRUE)
      })
      raw <- readr::read_csv(file, show_col_types = FALSE)
      df  <- dplyr::select(raw, HomeTeam, AwayTeam, FTR, FTHG, FTAG)
      match_data(df)
      showNotification(
        paste0("Downloaded ", nrow(df), " matches for ", input$league),
        type = "message", duration = 4
      )
    }, error = function(e) {
      showNotification(
        paste("Download failed:", conditionMessage(e)),
        type = "error", duration = 8
      )
    })
  })

  # ── data_status output ─────────────────────────────────────────────────────

  output$data_status <- renderText({
    req(match_data())
    cfg  <- LEAGUE_CONFIGS[[input$league]]
    info <- file.info(cfg$file)
    paste0(nrow(match_data()), " matches loaded\n",
           format(info$mtime, "%Y-%m-%d %H:%M"))
  })

  # ── all_matches: base data + any extra games appended ─────────────────────

  all_matches <- reactive({
    req(match_data())
    df        <- match_data()
    eg        <- extra_games()
    all_teams <- unique(c(df$HomeTeam, df$AwayTeam))
    if (nrow(eg) > 0) {
      for (i in seq_len(nrow(eg))) {
        if (eg$HomeTeam[i] %in% all_teams && eg$AwayTeam[i] %in% all_teams) {
          df <- add.game(df,
                         eg$HomeTeam[i], eg$HomeGoals[i],
                         eg$AwayTeam[i], eg$AwayGoals[i])
        }
      }
    }
    df
  })

  # ── league_tables: unsorted + sorted, with point deductions applied ────────

  league_tables <- reactive({
    req(all_matches())
    lt <- create.league.table(all_matches())
    pd        <- point_deductions()
    all_teams <- lt$Team
    if (nrow(pd) > 0) {
      for (i in seq_len(nrow(pd))) {
        if (pd$Team[i] %in% all_teams) {
          idx <- lt$Team == pd$Team[i]
          lt$Points[idx] <- lt$Points[idx] - pd$Points[i]
        }
      }
    }
    slt <- create.sorted.league.table(lt)
    list(unsorted = lt, sorted = slt)
  })

  # ── rem_matches: fixtures not yet played ──────────────────────────────────

  rem_matches_r <- reactive({
    req(all_matches(), league_tables())
    determine.remaining.matches(all_matches(), league_tables()$unsorted)
  })

  # ── match_summary sidebar text ────────────────────────────────────────────

  output$match_summary <- renderText({
    req(all_matches(), rem_matches_r())
    paste0(nrow(all_matches()), " played · ",
           nrow(rem_matches_r()), " remaining")
  })

  # ── Simulation ────────────────────────────────────────────────────────────

  sim_results <- reactiveVal(NULL)

  # Clear sim_results whenever underlying match data changes (forces re-run)
  observe({
    all_matches()
    sim_results(NULL)
  })

  observeEvent(input$run_sim, {
    req(rem_matches_r(), league_tables())
    num_sims <- input$num_sims
    rm       <- rem_matches_r()
    lt       <- league_tables()$unsorted

    withProgress(message = "Running simulation...", value = 0, {
      setProgress(0.1, detail = "Simulating seasons...")
      all_scores <- simulate.many.seasons(rm, num_sims, neutralize = FALSE)
      setProgress(0.7, detail = "Calculating standings...")
      all_sims   <- calc.points.and.rank(all_scores, lt, num_sims)
      setProgress(1.0, detail = "Done.")
    })

    sim_results(list(
      all_scores = all_scores,
      all_sims   = all_sims,
      num_sims   = num_sims
    ))
  })

  output$sim_status <- renderText({
    req(sim_results())
    paste0("Ready \u00b7 ", sim_results()$num_sims, " sims")
  })

  # ── Tab 1: League Table ───────────────────────────────────────────────────

  output$table_538 <- renderUI({
    req(sim_results(), league_tables())
    sr  <- sim_results()
    cfg <- LEAGUE_CONFIGS[[input$league]]
    t   <- make.configurable.538.table(sr$all_sims,
                                       league_tables()$sorted,
                                       cfg$zones)
    ft  <- make.538.flextable(t, cfg$zones)
    ft_to_html(ft)
  })

  output$download_table_png <- downloadHandler(
    filename = function() paste0(gsub(" ", "_", input$league), "_table.png"),
    content  = function(file) {
      req(sim_results(), league_tables())
      sr  <- sim_results()
      cfg <- LEAGUE_CONFIGS[[input$league]]
      t   <- make.configurable.538.table(sr$all_sims,
                                         league_tables()$sorted,
                                         cfg$zones)
      ft  <- make.538.flextable(t, cfg$zones)
      flextable::save_as_image(ft, path = file)
    }
  )

  # ── Tab 2: Relegation Race ────────────────────────────────────────────────

  # Helper: get relegation cutoff from the last zone in the league config
  rel_default_cutoff <- function(league_name) {
    zones <- LEAGUE_CONFIGS[[league_name]]$zones
    zones[[length(zones)]]$placement
  }

  # Update the cutoff input whenever the league changes
  observe({
    updateNumericInput(session, "rel_cutoff",
                       value = rel_default_cutoff(input$league))
  })

  output$rel_team_checkboxes <- renderUI({
    req(league_tables())
    teams <- league_tables()$sorted$Team
    pre   <- tail(teams, 6)
    checkboxGroupInput("rel_teams", NULL, choices = teams, selected = pre)
  })

  # Build relegation plot list as a reactive (shared by renderPlot + download)
  rel_plots <- reactive({
    req(sim_results(), input$rel_teams)
    sr     <- sim_results()
    teams  <- input$rel_teams
    cutoff <- input$rel_cutoff

    plot_rel <- function(team) {
      ts  <- dplyr::filter(sr$all_sims, Team == team) %>%
               dplyr::mutate(relegated = Rank > cutoff)
      trr <- ts %>%
               dplyr::group_by(Pts) %>%
               dplyr::summarize(Pts_likelihood = n() / sr$num_sims,
                                Rel_likelihood = sum(relegated) / n(),
                                .groups = "drop")
      mean_pts <- mean(ts$Pts)
      rel_odds <- sum(ts$relegated) / sr$num_sims * 100
      trr <- dplyr::mutate(trr, Non_rel_odds = Pts_likelihood * (1 - Rel_likelihood))
      ggplot2::ggplot(trr, ggplot2::aes(x = Pts)) +
        ggplot2::geom_col(ggplot2::aes(y = Pts_likelihood, fill = "PtsLikelihood")) +
        ggplot2::geom_col(ggplot2::aes(y = Non_rel_odds,   fill = "NonRelOdds")) +
        ggplot2::labs(
          title    = paste(team, "Final Placement Distribution"),
          subtitle = paste0(round(mean_pts, 1), " mean pts · ",
                            round(rel_odds, 1), "% below pos ",
                            cutoff, " (", sr$num_sims, " sims)"),
          caption  = "dw simulation model") +
        ggplot2::scale_fill_manual(
          values = c("NonRelOdds" = "green", "PtsLikelihood" = "red")) +
        ggplot2::scale_x_continuous("Points in Final Table",
          breaks = seq(min(trr$Pts), max(trr$Pts), by = 1)) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = 1)) +
        ggplot2::theme(panel.grid.minor  = ggplot2::element_blank(),
                       legend.position   = "none",
                       plot.title        = ggplot2::element_text(size = 11),
                       plot.subtitle     = ggplot2::element_text(size = 8))
    }
    lapply(teams, plot_rel)
  })

  output$rel_plot <- renderPlot({
    plots <- rel_plots()
    nc    <- max(1, ceiling(sqrt(length(plots))))
    gridExtra::grid.arrange(grobs = plots, ncol = nc)
  })

  output$download_rel_png <- downloadHandler(
    filename = function() paste0(gsub(" ", "_", input$league), "_relegation.png"),
    content  = function(file) {
      plots <- rel_plots()
      nc    <- max(1, ceiling(sqrt(length(plots))))
      gr    <- gridExtra::arrangeGrob(grobs = plots, ncol = nc)
      ggplot2::ggsave(file, gr, width = 10, height = 8, units = "in", dpi = 150)
    }
  )

  output$rel_odds_table <- renderTable({
    req(sim_results(), league_tables())
    sr          <- sim_results()
    cfg         <- LEAGUE_CONFIGS[[input$league]]
    danger_zone <- cfg$zones[[length(cfg$zones)]]
    odds <- create.finishing.odds.table(sr$all_sims,
                                        danger_zone$placement,
                                        danger_zone$operator)
    slt  <- league_tables()$sorted
    odds <- dplyr::left_join(odds, dplyr::select(slt, Team, Points), by = "Team") %>%
              dplyr::filter(Percent >= 0.1) %>%
              dplyr::arrange(dplyr::desc(Percent)) %>%
              dplyr::select(Team, Points, Percent) %>%
              dplyr::mutate(Points = as.integer(Points), Percent = round(Percent, 1))
    names(odds) <- c("Team", "Points", "Relegation %")
    odds
  }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 1)

  # ── Tab 3: Title / Top Spots ──────────────────────────────────────────────

  output$top_team_checkboxes <- renderUI({
    req(league_tables())
    teams <- league_tables()$sorted$Team
    pre   <- head(teams, 6)
    checkboxGroupInput("top_teams", NULL, choices = teams, selected = pre)
  })

  output$zone_selector_ui <- renderUI({
    cfg        <- LEAGUE_CONFIGS[[input$league]]
    zone_names <- names(cfg$zones)
    good_zones <- zone_names[-length(zone_names)]   # drop danger zone
    selectInput("top_zone", "Highlight zone:", choices = good_zones)
  })

  # Build top-spots plot list as a reactive (shared by renderPlot + download)
  top_plots <- reactive({
    req(sim_results(), input$top_teams)
    sr  <- sim_results()
    cfg <- LEAGUE_CONFIGS[[input$league]]
    lapply(input$top_teams, plot.points.vs.rank,
           sr$all_sims, sr$num_sims, cfg$num_teams)
  })

  output$top_plot <- renderPlot({
    plots <- top_plots()
    nc    <- max(1, ceiling(sqrt(length(plots))))
    gridExtra::grid.arrange(grobs = plots, ncol = nc)
  })

  output$download_top_png <- downloadHandler(
    filename = function() paste0(gsub(" ", "_", input$league), "_top_spots.png"),
    content  = function(file) {
      plots <- top_plots()
      nc    <- max(1, ceiling(sqrt(length(plots))))
      gr    <- gridExtra::arrangeGrob(grobs = plots, ncol = nc)
      ggplot2::ggsave(file, gr, width = 10, height = 8, units = "in", dpi = 150)
    }
  )

  # Zone odds table: shows each selected team's % chance for the chosen zone
  output$top_zone_odds <- renderTable({
    req(sim_results(), input$top_teams, input$top_zone)
    sr       <- sim_results()
    cfg      <- LEAGUE_CONFIGS[[input$league]]
    z        <- cfg$zones[[input$top_zone]]
    odds     <- create.finishing.odds.table(sr$all_sims, z$placement, z$operator)
    selected <- dplyr::filter(odds, Team %in% input$top_teams) %>%
                  dplyr::select(Team, Percent) %>%
                  dplyr::mutate(Percent = round(Percent, 1)) %>%
                  dplyr::arrange(dplyr::desc(Percent))
    names(selected) <- c("Team", paste0(input$top_zone, " %"))
    selected
  }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 1)

  # ── Tab 4: Team Focus ─────────────────────────────────────────────────────

  ordinal_suffix <- function(n) {
    suffix <- dplyr::case_when(
      n %% 100 %in% 11:13 ~ "th",
      n %% 10  == 1       ~ "st",
      n %% 10  == 2       ~ "nd",
      n %% 10  == 3       ~ "rd",
      TRUE                ~ "th"
    )
    paste0(n, suffix)
  }

  output$team_focus_selector <- renderUI({
    req(league_tables())
    teams <- league_tables()$sorted$Team
    selectInput("focus_team", "Select Team:", choices = teams)
  })

  output$team_focus_summary <- renderText({
    req(sim_results(), input$focus_team, league_tables())
    team     <- input$focus_team
    slt      <- league_tables()$sorted
    sr       <- sim_results()
    cur_pos  <- which(slt$Team == team)
    cur_pts  <- slt$Points[cur_pos]
    team_sim <- dplyr::filter(sr$all_sims, Team == team)
    most_likely_rank  <- as.integer(names(which.max(table(team_sim$Rank))))
    mean_final_pts    <- round(mean(team_sim$Pts), 1)
    paste0(team, " — currently ", ordinal_suffix(cur_pos),
           " with ", cur_pts, " pts",
           " | most likely finish: ", ordinal_suffix(most_likely_rank),
           " | expected final pts: ", mean_final_pts)
  })

  # Shared reactive: relegation data for the focused team
  team_rel_data <- reactive({
    req(sim_results(), input$focus_team)
    sr     <- sim_results()
    cutoff <- rel_default_cutoff(input$league)
    team   <- input$focus_team
    ts     <- dplyr::filter(sr$all_sims, Team == team) %>%
                dplyr::mutate(relegated = Rank > cutoff)
    trr    <- ts %>%
                dplyr::group_by(Pts) %>%
                dplyr::summarize(Pts_likelihood = n() / sr$num_sims,
                                 Rel_likelihood = sum(relegated) / n(),
                                 .groups = "drop") %>%
                dplyr::mutate(Non_rel_odds = Pts_likelihood * (1 - Rel_likelihood))
    rel_odds <- sum(ts$relegated) / sr$num_sims * 100
    list(ts = ts, trr = trr, rel_odds = rel_odds,
         mean_pts = mean(ts$Pts), cutoff = cutoff)
  })

  # Conditional plot layout: 2 plots if rel risk >= 0.5%, else 1 full-width
  output$team_focus_plots_ui <- renderUI({
    req(team_rel_data())
    if (team_rel_data()$rel_odds >= 0.5) {
      fluidRow(
        column(6, plotOutput("team_focus_rank_plot", height = "350px")),
        column(6, plotOutput("team_focus_rel_plot",  height = "350px"))
      )
    } else {
      fluidRow(
        column(12, plotOutput("team_focus_rank_plot", height = "350px"))
      )
    }
  })

  # Rank plot as a reactive ggplot object (reused by renderPlot + download)
  team_rank_ggplot <- reactive({
    req(sim_results(), input$focus_team)
    sr  <- sim_results()
    cfg <- LEAGUE_CONFIGS[[input$league]]
    plot.points.vs.rank(input$focus_team, sr$all_sims, sr$num_sims, cfg$num_teams)
  })

  # Rel plot as a reactive ggplot object (reused by renderPlot + download)
  team_rel_ggplot <- reactive({
    req(team_rel_data())
    d   <- team_rel_data()
    trr <- d$trr
    ggplot2::ggplot(trr, ggplot2::aes(x = Pts)) +
      ggplot2::geom_col(ggplot2::aes(y = Pts_likelihood, fill = "PtsLikelihood")) +
      ggplot2::geom_col(ggplot2::aes(y = Non_rel_odds,   fill = "NonRelOdds")) +
      ggplot2::labs(
        title    = paste(input$focus_team, "Relegation Risk"),
        subtitle = paste0(round(d$mean_pts, 1), " mean pts · ",
                          round(d$rel_odds, 1), "% below pos ",
                          d$cutoff, " (", sim_results()$num_sims, " sims)"),
        caption  = "dw simulation model") +
      ggplot2::scale_fill_manual(
        values = c("NonRelOdds" = "green", "PtsLikelihood" = "red")) +
      ggplot2::scale_x_continuous("Points in Final Table",
        breaks = seq(min(trr$Pts), max(trr$Pts), by = 1)) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1)) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     legend.position  = "none",
                     plot.title       = ggplot2::element_text(size = 11),
                     plot.subtitle    = ggplot2::element_text(size = 8))
  })

  output$team_focus_rank_plot <- renderPlot({ team_rank_ggplot() })
  output$team_focus_rel_plot  <- renderPlot({ req(team_rel_data()$rel_odds >= 0.5)
                                              team_rel_ggplot() })

  output$download_focus_png <- downloadHandler(
    filename = function() paste0(gsub(" ", "_", input$focus_team), "_focus.png"),
    content  = function(file) {
      req(team_rel_data(), team_rank_ggplot())
      d         <- team_rel_data()
      rank_plot <- team_rank_ggplot()

      # Title grob (summary line)
      sr        <- sim_results()
      slt       <- league_tables()$sorted
      cur_pos   <- which(slt$Team == input$focus_team)
      cur_pts   <- slt$Points[cur_pos]
      team_sim  <- dplyr::filter(sr$all_sims, Team == input$focus_team)
      mlr       <- as.integer(names(which.max(table(team_sim$Rank))))
      mean_pts  <- round(mean(team_sim$Pts), 1)
      title_str <- paste0(input$focus_team,
                          "  |  currently ", ordinal_suffix(cur_pos),
                          " with ", cur_pts, " pts",
                          "  |  most likely finish: ", ordinal_suffix(mlr),
                          "  |  expected final pts: ", mean_pts)
      title_grob <- grid::textGrob(title_str,
                                   gp = grid::gpar(fontsize = 13, fontface = "bold"))

      if (d$rel_odds >= 0.5) {
        # Layout: title (full width) / [plots stacked left | table right]
        rel_plot <- team_rel_ggplot()
        tbl_data <- d$trr %>%
          dplyr::mutate(`Final Pts` = as.integer(Pts),
                        `Rel %`     = round(Rel_likelihood * 100, 1)) %>%
          dplyr::filter(`Rel %` > 0) %>%
          dplyr::arrange(`Final Pts`) %>%
          dplyr::select(`Final Pts`, `Rel %`)
        tbl_grob   <- gridExtra::tableGrob(
                        tbl_data, rows = NULL,
                        theme = gridExtra::ttheme_default(base_size = 10))
        plots_col  <- gridExtra::arrangeGrob(rank_plot, rel_plot, ncol = 1)
        bottom_row <- gridExtra::arrangeGrob(plots_col, tbl_grob,
                                             ncol = 2, widths = c(0.65, 0.35))
        g <- gridExtra::arrangeGrob(title_grob, bottom_row,
                                    ncol = 1, heights = c(0.06, 0.94))
        ggplot2::ggsave(file, g, width = 12, height = 10, units = "in", dpi = 150)
      } else {
        # Single full-width rank plot
        g <- gridExtra::arrangeGrob(title_grob, rank_plot,
                                    ncol = 1, heights = c(0.08, 0.92))
        ggplot2::ggsave(file, g, width = 9, height = 6, units = "in", dpi = 150)
      }
    }
  )

  # Points-to-relegation table: shown whenever rel chart is shown (>= 0.5%)
  output$team_focus_rel_table_ui <- renderUI({
    req(team_rel_data())
    if (team_rel_data()$rel_odds < 0.5) return(NULL)
    tagList(
      hr(),
      h6(paste0("Relegation risk by final points (",
                 round(team_rel_data()$rel_odds, 1), "% overall):")),
      tableOutput("team_focus_rel_table")
    )
  })

  output$team_focus_rel_table <- renderTable({
    req(team_rel_data())
    req(team_rel_data()$rel_odds >= 0.5)
    d <- team_rel_data()
    tbl <- d$trr %>%
      dplyr::mutate(Rel_pct = round(Rel_likelihood * 100, 1)) %>%
      dplyr::filter(Rel_pct > 0) %>%
      dplyr::arrange(Pts) %>%
      dplyr::select(Pts, Rel_pct) %>%
      dplyr::mutate(Pts = as.integer(Pts))
    names(tbl) <- c("Final Pts", "Rel %")
    tbl
  }, striped = TRUE, spacing = "xs", digits = 1)

  # Table row: suppress zone columns where this team has ~0% odds
  output$team_focus_table_row <- renderUI({
    req(sim_results(), input$focus_team, league_tables())
    sr    <- sim_results()
    cfg   <- LEAGUE_CONFIGS[[input$league]]
    t_538 <- make.configurable.538.table(sr$all_sims,
                                         league_tables()$sorted,
                                         cfg$zones)
    row       <- dplyr::filter(t_538, Team == input$focus_team)
    zone_cols <- names(cfg$zones)
    # Keep only zones where value is not "0.0" and not blank
    active_zones <- zone_cols[!(row[, zone_cols] %in% c("0.0", ""))]
    row          <- dplyr::select(row, Rank, Team, Played, Points, GD,
                                  dplyr::all_of(active_zones))
    ft <- make.538.flextable(row, cfg$zones[active_zones])
    ft_to_html(ft)
  })

  # ── Tab 5: Match Impact ───────────────────────────────────────────────────

  output$impact_team_filter <- renderUI({
    req(rem_matches_r())
    teams <- sort(unique(c(rem_matches_r()$HomeTeam, rem_matches_r()$AwayTeam)))
    selectInput("impact_team", "Filter by team:",
                choices = c("All teams", teams),
                selected = "All teams")
  })

  output$impact_fixture_selector <- renderUI({
    req(rem_matches_r(), input$impact_team)
    rm <- rem_matches_r()

    if (input$impact_team == "All teams") {
      # Sort by home team then away team alphabetically
      rm <- dplyr::arrange(rm, HomeTeam, AwayTeam)
      labels <- paste(rm$HomeTeam, "vs", rm$AwayTeam)
    } else {
      team <- input$impact_team
      home_fix <- dplyr::filter(rm, HomeTeam == team) %>%
                    dplyr::arrange(AwayTeam)
      away_fix <- dplyr::filter(rm, AwayTeam == team) %>%
                    dplyr::arrange(HomeTeam)
      # Label home fixtures as "Home: Team vs Opp", away as "Away: Opp vs Team"
      rm     <- dplyr::bind_rows(home_fix, away_fix)
      labels <- paste(rm$HomeTeam, "vs", rm$AwayTeam)
    }

    vals <- paste(rm$HomeTeam, rm$AwayTeam, sep = "||")
    selectInput("impact_fixture", "Fixture:",
                choices = setNames(vals, labels))
  })

  output$impact_watch_ui <- renderUI({
    req(league_tables())
    teams <- league_tables()$sorted$Team
    selectizeInput("impact_watch", "Also watch:",
                   choices  = teams,
                   selected = NULL,
                   multiple = TRUE,
                   options  = list(placeholder = "Add a team..."))
  })

  # Clear watching teams when league changes
  observe({
    input$league
    updateSelectizeInput(session, "impact_watch", selected = character(0))
  })

  output$impact_title <- renderText({
    req(input$impact_fixture)
    parts <- strsplit(input$impact_fixture, "\\|\\|")[[1]]
    paste("Match Impact:", parts[1], "vs", parts[2])
  })

  output$impact_table <- DT::renderDT({
    req(sim_results(), input$impact_fixture, league_tables())
    sr    <- sim_results()
    cfg   <- LEAGUE_CONFIGS[[input$league]]
    lt    <- league_tables()$unsorted
    parts <- strsplit(input$impact_fixture, "\\|\\|")[[1]]
    home  <- parts[1]
    away  <- parts[2]

    # Compute standings for all 3 outcomes — only 3 calls to calc.points.and.rank
    force_result <- function(scores, hg, ag) {
      dplyr::mutate(scores,
        HG = replace(HG, HomeTeam == home & AwayTeam == away, hg),
        AG = replace(AG, HomeTeam == home & AwayTeam == away, ag))
    }
    withProgress(message = "Computing match impact...", value = 0, {
      setProgress(0.1, detail = "Home win scenario...")
      hw_sims <- calc.points.and.rank(force_result(sr$all_scores, 2, 1), lt, sr$num_sims)
      setProgress(0.4, detail = "Draw scenario...")
      dr_sims <- calc.points.and.rank(force_result(sr$all_scores, 1, 1), lt, sr$num_sims)
      setProgress(0.7, detail = "Away win scenario...")
      aw_sims <- calc.points.and.rank(force_result(sr$all_scores, 1, 2), lt, sr$num_sims)
      setProgress(1.0, detail = "Building table...")
    })

    # Dynamic column names using actual team names
    home_col <- paste0(home, " Win")
    away_col <- paste0(away, " Win")

    # For each zone × team, get odds under each outcome
    get_pct <- function(sims, placement, operator, team) {
      odds <- create.finishing.odds.table(sims, placement, operator)
      row  <- dplyr::filter(odds, Team == team)
      if (nrow(row) == 0) return(0)
      round(row$Percent, 1)
    }

    rows <- dplyr::bind_rows(lapply(names(cfg$zones), function(zone_name) {
      z <- cfg$zones[[zone_name]]
      dplyr::bind_rows(lapply(c(home, away, input$impact_watch), function(team) {
        hw <- get_pct(hw_sims, z$placement, z$operator, team)
        dr <- get_pct(dr_sims, z$placement, z$operator, team)
        aw <- get_pct(aw_sims, z$placement, z$operator, team)
        if (max(hw, dr, aw) < 0.5) return(NULL)
        df <- data.frame(Zone = zone_name, Team = team, hw, dr, aw)
        names(df) <- c("Zone", "Team", home_col, "Draw", away_col)
        df
      }))
    }))

    DT::datatable(rows,
      rownames = FALSE,
      options  = list(
        pageLength = 30,
        dom = 't',
        drawCallback = JS("function(settings) {
          var api = this.api();
          var nodes = api.rows({page: 'current'}).nodes();
          var last_zone = '';
          api.column(0, {page: 'current'}).data().each(function(zone, i) {
            if (zone !== last_zone && last_zone !== '') {
              $(nodes).eq(i).find('td').css('border-top', '2px solid #555');
            }
            last_zone = zone;
          });
        }")
      ),
      caption = paste(home, "vs", away, "— odds shift by result")
    ) %>%
      DT::formatRound(c(home_col, "Draw", away_col), digits = 1)

  }, server = FALSE)

  # ── Tab 6: Extra Games & Adjustments ─────────────────────────────────────

  # Teams from currently loaded match data
  teams_list <- reactive({
    req(match_data())
    sort(unique(c(match_data()$HomeTeam, match_data()$AwayTeam)))
  })

  # ── Extra Games ───────────────────────────────────────────────────────────

  output$eg_team_filter_ui <- renderUI({
    req(rem_matches_r())
    teams <- sort(unique(c(rem_matches_r()$HomeTeam, rem_matches_r()$AwayTeam)))
    selectInput("eg_team_filter", "Filter by team:",
                choices = c("All teams", teams), selected = "All teams")
  })

  output$eg_fixture_ui <- renderUI({
    req(rem_matches_r(), input$eg_team_filter)
    rm   <- rem_matches_r()
    team <- input$eg_team_filter
    if (team == "All teams") {
      rm <- dplyr::arrange(rm, HomeTeam, AwayTeam)
    } else {
      home_fix <- dplyr::filter(rm, HomeTeam == team) %>% dplyr::arrange(AwayTeam)
      away_fix <- dplyr::filter(rm, AwayTeam == team) %>% dplyr::arrange(HomeTeam)
      rm <- dplyr::bind_rows(home_fix, away_fix)
    }
    vals   <- paste(rm$HomeTeam, rm$AwayTeam, sep = "||")
    labels <- paste(rm$HomeTeam, "vs", rm$AwayTeam)
    selectInput("eg_fixture", "Fixture:", choices = setNames(vals, labels))
  })

  output$eg_home_goals_ui <- renderUI({
    label <- if (!is.null(input$eg_fixture) && nchar(input$eg_fixture) > 0)
               strsplit(input$eg_fixture, "\\|\\|")[[1]][1]
             else "Home Goals"
    numericInput("eg_home_goals", paste(label, "Goals"), 0, 0, 20)
  })

  output$eg_away_goals_ui <- renderUI({
    label <- if (!is.null(input$eg_fixture) && nchar(input$eg_fixture) > 0)
               strsplit(input$eg_fixture, "\\|\\|")[[1]][2]
             else "Away Goals"
    numericInput("eg_away_goals", paste(label, "Goals"), 0, 0, 20)
  })

  output$extra_games_dt <- DT::renderDT({
    df <- extra_games()
    if (nrow(df) == 0) {
      return(DT::datatable(df, rownames = FALSE,
                           options = list(dom = 't'),
                           caption = "No extra games added yet."))
    }
    df$Delete <- paste0(
      '<button class="btn btn-danger btn-sm delete-eg" data-row="',
      seq_len(nrow(df)), '">Delete</button>')
    DT::datatable(df, escape = FALSE, rownames = FALSE,
                  selection = "none",
                  options = list(dom = 't', pageLength = 50))
  }, server = FALSE)

  observeEvent(input$add_game_btn, {
    req(input$eg_fixture)
    parts <- strsplit(input$eg_fixture, "\\|\\|")[[1]]
    home  <- parts[1]
    away  <- parts[2]
    new_row <- data.frame(
      HomeTeam  = home,
      HomeGoals = as.integer(input$eg_home_goals),
      AwayTeam  = away,
      AwayGoals = as.integer(input$eg_away_goals)
    )
    updated <- dplyr::bind_rows(extra_games(), new_row)
    readr::write_csv(updated, "data/extra_games.csv")
    extra_games(updated)
    showNotification(
      paste0("Recorded: ", home, " ", input$eg_home_goals,
             " \u2013 ", input$eg_away_goals, " ", away,
             ". Re-run simulation to update odds."),
      type = "message", duration = 5)
  })

  observeEvent(input$delete_eg_row, {
    idx     <- as.integer(input$delete_eg_row)
    updated <- extra_games()[-idx, , drop = FALSE]
    readr::write_csv(updated, "data/extra_games.csv")
    extra_games(updated)
    showNotification("Game removed. Re-run simulation to update odds.",
                     type = "message", duration = 4)
  })

  # ── Point Deductions ──────────────────────────────────────────────────────

  output$pd_team_ui <- renderUI({
    selectInput("pd_team", "Team:", choices = teams_list())
  })

  output$point_ded_dt <- DT::renderDT({
    df <- point_deductions()
    if (nrow(df) == 0) {
      return(DT::datatable(df, rownames = FALSE,
                           options = list(dom = 't'),
                           caption = "No deductions applied yet."))
    }
    df$Delete <- paste0(
      '<button class="btn btn-danger btn-sm delete-pd" data-row="',
      seq_len(nrow(df)), '">Delete</button>')
    DT::datatable(df, escape = FALSE, rownames = FALSE,
                  selection = "none",
                  options = list(dom = 't', pageLength = 50))
  }, server = FALSE)

  observeEvent(input$add_ded_btn, {
    req(input$pd_team)
    new_row <- data.frame(
      Team   = input$pd_team,
      Points = as.integer(input$pd_points),
      Reason = input$pd_reason
    )
    updated <- dplyr::bind_rows(point_deductions(), new_row)
    readr::write_csv(updated, "data/point_deductions.csv")
    point_deductions(updated)
    showNotification(
      paste0("Deducted ", input$pd_points, " pts from ", input$pd_team,
             ". Re-run simulation to update odds."),
      type = "message", duration = 5)
  })

  observeEvent(input$delete_pd_row, {
    idx     <- as.integer(input$delete_pd_row)
    updated <- point_deductions()[-idx, , drop = FALSE]
    readr::write_csv(updated, "data/point_deductions.csv")
    point_deductions(updated)
    showNotification("Deduction removed. Re-run simulation to update odds.",
                     type = "message", duration = 4)
  })

}

shinyApp(ui, server)
