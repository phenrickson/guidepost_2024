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

pivot_games_to_teams = function(data, game_vars = c("season", "game_id", "season_type", "week", "start_date", "neutral_site")) {

  tmp =
    data |>
    select(game_id, home_team, away_team)

  longer =
    data |>
    select(-contains("_elo"), -contains("_prob")) |>
    longer_games(game_vars = game_vars) |>
    rename_with(
      .cols = c("id", "division", "points"),
      .fn = ~  paste("team", .x, sep = "_")
    )

  joined =
    longer |>
    left_join(tmp,
              by = join_by(game_id))  |>
    mutate(
      team_outcome = case_when(team_points > opponent_points ~ 'win',
                               opponent_points > team_points ~ 'loss',
                               team_points == opponent_points ~ 'tie'),
      team_is_home = case_when(home_team == team & neutral_site == F ~ T,
                               TRUE ~ F)
    )

  joined |>
    arrange(season, start_date) |>
    select(any_of(game_vars), team_is_home, starts_with("team"), starts_with("opponent"))

}

scale_color_cfb_muted = function (alt_colors = NULL, values = NULL, ..., aesthetics = "colour",
                                  breaks = ggplot2::waiver(), na.value = "grey50", guide = NULL,
                                  alpha = NA)
{
  if (is.null(values)) {
    values <- cfbplotR::logo_ref %>%
      dplyr::mutate(value = ifelse(.data$school %in% alt_colors, .data$alt_color, .data$color),
                    value = scales::muted(value))

    values <- values %>%
      dplyr::pull("value")
    names(values) <- cfbplotR::logo_ref$school
  }
  if (!is.na(alpha))
    values <- scales::alpha(values, alpha = alpha)
  ggplot2::scale_color_manual(..., values = values, aesthetics = aesthetics,
                              breaks = breaks, na.value = na.value, guide = guide)
}

find_season_weeks = function(games) {
  games |>
    filter(season_type %in% c('regular', 'postseason')) |>
    select(season, season_type, week, start_date) |>
    mutate(start_date = as.Date(start_date)) |>
    mutate(week_date = lubridate::ceiling_date(start_date, unit = "week", week_start = "Wednesday")) |>
    group_by(season, season_type, week, week_date) |>
    summarize(
      start_date = max(week_date),
      .groups = 'drop'
    ) |>
    arrange(start_date) |>
    group_by(season, season_type) |>
    mutate(flag = case_when(week_date == dplyr::lag(week_date, 1) ~ T,
                            TRUE ~ F)) |>
    filter(flag == F) |>
    arrange(week_date, desc(season_type)) |>
    group_by(season) |>
    mutate(season_week = paste(season, row_number()-1, sep = "_")) |>
    ungroup() |>
    select(season_week, season, season_type, week, week_date)
}

add_game_weeks <- function(data) {
  find_nearest_week <- function(data) {
    data |>
      mutate(diff = as.numeric(week_date - start_date)) |>
      filter(diff > 0) |>
      group_by(game_id) |>
      slice_min(diff, n = 1) |>
      ungroup() |>
      select(-diff)
  }

  season_weeks <-
    data |>
    find_season_weeks()

  joined <-
    data |>
    mutate(start_date = as.Date(start_date)) |>
    left_join(
      season_weeks,
      by = c("season", "season_type", "week"),
      relationship = "many-to-many"
    )

  out <-
    joined |>
    find_nearest_week() |>
    select(season, season_type, season_week, week, week_date, week, start_date, game_id, everything())

  if (nrow(data) > nrow(out)) {
    dropped <- data$game_id[!(data$game_id %in% out$game_id)]

    warning(paste(paste(dropped, collapse = ","), "game ids dropped from data"))
  } else if (nrow(data) < nrow(out)) {
    warning("too many games returned; check")
  }

  out |>
    select(
      -ends_with("_elo"),
      -ends_with("excitement_index"),
      -ends_with("post_win_prob"),
      -any_of(c("highlights"))
    )
}

find_team_ids <- function(data) {
  data |>
    select(
      team = home_team,
      team_id = home_id
    ) |>
    distinct() |>
    bind_rows(
      data |>
        select(
          team = away_team,
          team_id = away_id
        )
    ) |>
    distinct()
}

# adjust changes to team names that occurred in 2024
adjust_team_names <- function(data) {
  data |>
    mutate(
      across(
        c(home_team, away_team),
        ~ case_when(
          .x == "Sam Houston" ~ "Sam Houston State",
          .x == "UL Monroe" ~ "Louisiana Monroe",
          .x == "App State" ~ "Appalachian State",
          .x == "UConn" ~ "Connecticut",
          .x == "Massachusetts" ~ "UMass",
          .x == "Southern Miss" ~ "Southern Mississippi",
          .x == "UTSA" ~ "UT San Antonio",
          TRUE ~ .x
        )
      )
    )
}

# given data and teams filter to long
filter_to_team <- function(data, teams) {
  home_team_data <-
    data |>
    inner_join(
      tibble(home_team = teams)
    ) |>
    select(
      season,
      season_type,
      season_week,
      game_id,
      team = home_team,
      opponent = away_team,
      .draw,
      .prediction
    ) |>
    mutate(is_home = "yes") |>
    mutate(win = case_when(.prediction > 0 ~ 1, .prediction < 0 ~ 0))

  away_team_data <-
    data |>
    inner_join(
      tibble(away_team = teams)
    ) |>
    select(
      season,
      season_type,
      season_week,
      game_id,
      team = away_team,
      opponent = home_team,
      .draw,
      .prediction
    ) |>
    mutate(is_home = "no") |>
    mutate(win = case_when(.prediction > 0 ~ 0, .prediction < 0 ~ 1))

  bind_rows(
    home_team_data,
    away_team_data
  )
}
