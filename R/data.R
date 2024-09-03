# function to add season to data
add_season <- function(data, year) {
  tbl <-
    data |>
    as_tibble()

  tbl$season <- year

  tbl |>
    select(season, everything())
}

get_cfbd_plays <- function(data) {
  season <- data$season
  week <- data$week
  season_type <- data$season_type

  cfbd_plays(
    year = season,
    week = week,
    season_type = season_type
  )
}

get_cfbd_pbp_data <- function(data) {
  season <- data$season
  week <- data$week
  season_type <- data$season_type

  cfbd_pbp_data(
    year = as.numeric(season),
    week = week,
    season_type = season_type
  )
}

get_game_player_stats <- function(data) {
  season <- data$season
  week <- data$week
  season_type <- data$season_type

  cfbd_game_player_stats(
    year = season,
    week = week,
    season_type = season_type
  )
}

theme_cfb = function(base_size = 11) {

  theme_light() %+replace%
    theme(
      plot.subtitle = element_text(size = 10, hjust = 0, vjust = 1, margin = margin(b = base_size/2)),
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
      )+
    theme(
      legend.position = 'top'
    )
}

rename_home_away <- function(data, text = "home") {
  data |>
    rename_with(
      .fn = ~ gsub(paste0("^", text, "_"), "", .x),
      .cols = starts_with(text)
    )
}

find_team_divisions <- function(data) {
  tmp <-
    data |>
    mutate(
      across(
        c(home_division, away_division),
        ~ replace_na(.x, "fcs")
      )
    )

  home <-
    tmp |>
    select(season, starts_with("home")) |>
    rename_home_away("home")

  away <-
    tmp |>
    select(season, starts_with("away")) |>
    rename_home_away("away")

  bind_rows(
    home, away
  ) |>
    distinct() |>
    arrange(season)
}

filter_to_fbs <- function(data, divisions = team_divisions) {
  data |>
  inner_join(
    divisions |>
    filter(division == "fbs"),
    by = join_by(season, team)
  )
}

factor_team_names <- function(data, ref = "fcs") {
  data |>
  mutate(
    offense_id = case_when(
      offense == home & home_division == "fbs" ~ home,
      offense != home & away_division == 'fbs' ~ away,
      TRUE ~ ref
    ),
    defense_id = case_when(
      defense == home & home_division == "fbs" ~ home,
      defense != home & away_division == 'fbs' ~ away,
      TRUE ~ ref
    )
  ) |>
  mutate(
    across(c("offense_id", "defense_id"), ~ relevel(factor(.x), ref = ref))
  )
}
