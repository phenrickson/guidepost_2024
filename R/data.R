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