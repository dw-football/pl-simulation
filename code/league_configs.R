###
### league_configs.R
###
### League configuration for the Shiny app.
### Each entry defines one league: download code, local file path,
### number of teams, and "zones" used to build the configurable 538 table.
###
### Zone semantics match create.finishing.odds.table():
###   operator "==" → finishes exactly at position N
###   operator "<"  → finishes above position N  (rank < N)
###   operator ">"  → finishes below position N  (rank > N)
###

LEAGUE_CONFIGS <- list(

  "Premier League" = list(
    code      = "E0",
    file      = "data/E0.csv",
    num_teams = 20,
    zones     = list(
      Win  = list(placement = 1,  operator = "=="),
      Top4 = list(placement = 5,  operator = "<"),
      Top5 = list(placement = 6,  operator = "<"),
      Top6 = list(placement = 7,  operator = "<"),
      Top7 = list(placement = 8,  operator = "<"),
      Top8 = list(placement = 9,  operator = "<"),
      Rel  = list(placement = 17, operator = ">")
    )
  ),

  "Championship" = list(
    code      = "E1",
    file      = "data/E1.csv",
    num_teams = 24,
    zones     = list(
      Win      = list(placement = 1,  operator = "=="),
      Promoted = list(placement = 3,  operator = "<"),
      Playoff  = list(placement = 7,  operator = "<"),
      Rel      = list(placement = 21, operator = ">")
    )
  ),

  "Serie A" = list(
    code      = "I1",
    file      = "data/I1.csv",
    num_teams = 20,
    zones     = list(
      Win  = list(placement = 1,  operator = "=="),
      Top4 = list(placement = 5,  operator = "<"),
      Top6 = list(placement = 7,  operator = "<"),
      Rel  = list(placement = 17, operator = ">")
    )
  ),

  "Serie B" = list(
    code      = "I2",
    file      = "data/I2.csv",
    num_teams = 20,
    zones     = list(
      Win      = list(placement = 1, operator = "=="),
      Promoted = list(placement = 3, operator = "<"),
      Playoff  = list(placement = 9, operator = "<"),
      Rel      = list(placement = 17, operator = ">")
    )
  ),

  "Bundesliga" = list(
    code      = "D1",
    file      = "data/D1.csv",
    num_teams = 18,
    zones     = list(
      Win  = list(placement = 1,  operator = "=="),
      Top4 = list(placement = 5,  operator = "<"),
      Top6 = list(placement = 7,  operator = "<"),
      Rel  = list(placement = 16, operator = ">")
    )
  ),

  "2. Bundesliga" = list(
    code      = "D2",
    file      = "data/D2.csv",
    num_teams = 18,
    zones     = list(
      Win      = list(placement = 1, operator = "=="),
      Promoted = list(placement = 3, operator = "<"),
      Playoff  = list(placement = 4, operator = "=="),
      Rel      = list(placement = 16, operator = ">")
    )
  ),

  "La Liga" = list(
    code      = "SP1",
    file      = "data/SP1.csv",
    num_teams = 20,
    zones     = list(
      Win  = list(placement = 1,  operator = "=="),
      Top4 = list(placement = 5,  operator = "<"),
      Top6 = list(placement = 7,  operator = "<"),
      Rel  = list(placement = 17, operator = ">")
    )
  ),

  "Ligue 1" = list(
    code      = "F1",
    file      = "data/F1.csv",
    num_teams = 18,
    zones     = list(
      Win  = list(placement = 1, operator = "=="),
      Top3 = list(placement = 4, operator = "<"),
      Top5 = list(placement = 6, operator = "<"),
      Rel  = list(placement = 15, operator = ">")
    )
  )

)


###
### current_season()
###
### Returns the two-digit-year code for the current football season,
### matching the football-data.co.uk URL format (e.g. "2526" for 2025-26).
###
### Logic:
###   Month >= 7 (July onward) → season started this calendar year
###   Month <  7 (Jan–June)    → season started last calendar year
###
current_season <- function() {
  today      <- Sys.Date()
  year       <- as.integer(format(today, "%Y"))
  month      <- as.integer(format(today, "%m"))
  start_year <- if (month >= 7) year else year - 1
  end_year   <- start_year + 1
  paste0(start_year %% 100, sprintf("%02d", end_year %% 100))
}


###
### make_download_url()
###
### Builds a football-data.co.uk download URL from a season code and
### league code.
###
### Example:
###   make_download_url("2526", "E0")
###   → "https://www.football-data.co.uk/mmz4281/2526/E0.csv"
###
make_download_url <- function(season_code, league_code) {
  paste0("https://www.football-data.co.uk/mmz4281/",
         season_code, "/", league_code, ".csv")
}


###
### make.configurable.538.table()
###
### Generalized version of create.538.table() that builds the odds table
### from a zone config instead of hardcoded positions.
###
### Args:
###   all_sims   — output of calc.points.and.rank()
###   sorted_lt  — output of create.sorted.league.table()
###   zones      — the $zones list from one LEAGUE_CONFIGS entry
###
make.configurable.538.table <- function(all_sims, sorted_lt, zones) {
  slt <- sorted_lt

  for (zone_name in names(zones)) {
    z    <- zones[[zone_name]]
    odds <- create.finishing.odds.table(all_sims, z$placement, z$operator)
    slt  <- dplyr::left_join(slt,
                             dplyr::select(odds, Team, !!zone_name := Percent),
                             by = "Team")
  }

  # Format zone columns to 1 decimal; NA → ""
  fmt <- function(x) {
    out        <- sprintf("%.1f", x)
    out[is.na(x)] <- ""
    out
  }
  zone_cols        <- names(zones)
  slt[, zone_cols] <- lapply(slt[, zone_cols], fmt)

  slt %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::select(Rank, Team, Played, Points, GD, dplyr::all_of(zone_cols))
}


###
### make.538.flextable()
###
### Turns a configurable 538 table into a styled flextable object.
### Does NOT save to disk — returns the object for renderUI or save_as_image.
###
### Convention: the LAST zone in zones is the danger zone (coloured red);
### all others are positive zones (coloured green when >= 50).
###
make.538.flextable <- function(t, zones) {
  zone_cols    <- names(zones)
  danger_zone  <- zone_cols[length(zone_cols)]
  good_zones   <- zone_cols[-length(zone_cols)]
  border_style <- officer::fp_border(color = "black", width = 1)
  n_fixed      <- 5L   # Rank, Team, Played, Points, GD
  n_total      <- n_fixed + length(zone_cols)

  ft <- flextable::flextable(t) %>%
    flextable::theme_zebra(odd_body = "#EFEFEF", even_body = "white") %>%
    flextable::color(~ as.numeric(GD) < 0, ~ GD, color = "red") %>%
    flextable::align(j = 1,       align = "right") %>%
    flextable::align(j = 2,       align = "left") %>%
    flextable::align(j = 3:n_total, align = "right") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::width(j = 2,   width = 1.3, unit = "in") %>%
    flextable::width(j = 3:5, width = 0.5, unit = "in") %>%
    flextable::vline(part = "all", j = 5,           border = border_style) %>%
    flextable::vline(part = "all", j = n_total - 1, border = border_style)

  for (col in good_zones) {
    ft <- flextable::color(ft,
            i = as.formula(paste0("~ as.numeric(", col, ") >= 50")),
            j = col, color = "#4CBB17")
  }
  ft <- flextable::color(ft,
          i = as.formula(paste0("~ as.numeric(", danger_zone, ") >= 50")),
          j = danger_zone, color = "red")

  ft
}


###
### ft_to_html()
###
### Converts a flextable object to an HTML string suitable for renderUI.
### Uses save_as_html() to a temp file — works across all flextable versions.
###
ft_to_html <- function(ft) {
  tmp <- tempfile(fileext = ".html")
  flextable::save_as_html(ft, path = tmp)
  htmltools::HTML(paste(readLines(tmp, warn = FALSE), collapse = "\n"))
}
