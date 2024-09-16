gt_tbl = function(data,
                  ...) {

  data |>
    gt::gt() |>
    gt::sub_missing() |>
    gtExtras::gt_theme_espn() |>
    gt::tab_options(
      container.overflow.y = T,
      ...
    )
}

round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

plays_points_tbl <- function(plays) {
  plays |>
    rename(
      ep_pre = expected_points_pre,
      ep_post = expected_points_post,
      ep_added = predicted_points_added,
    ) |>
    select(season, game_id, offense, defense, period, yards_to_goal, down, distance, play_text, ep_pre, ep_post, ep_added) |>
    mutate_if(is.numeric, round, 3) |>
    gt_tbl() |>
    gt::cols_align(
      columns = c("period", "down", "distance", "yards_to_goal", "ep_pre", "ep_post", "ep_added"),
      align = "center"
    ) |>
    gt::data_color(
      columns = c(
        "ep_pre",
        "ep_post",
        "ep_added"
      ),
      method = "numeric",
      domain = c(-10, 10),
      palette = c("orange", "white", "dodgerblue"),
      na_color = "white"
    ) |>
    gt::tab_options(
      data_row.padding = px(20),
      table.font.size = 12
    ) |>
    gt::cols_label(
      yards_to_goal = "ytg"
    )
}

top_plays_by_game <- function(plays, var = predicted_points_added, n = 10) {
  plays |>
    group_by(game_id) |>
    slice_max(abs({{ var }}), n = n) |>
    group_by(season, game_id)
}

# make table of game predictions
game_predictions_tbl <- function(data) {
  data |>
    gt_tbl() |>
    gt::fmt_datetime(
      columns = c(start_date),
      date_style = "MMMEd",
      time_style = "h_m_p"
    ) |>
    gt::cols_align(
      columns = everything(),
      align = "center"
    ) |>
    gt::cols_hide(
      columns = c(game_id, season_type, start_date, correct)
    ) |>
    gt::fmt_number(
      columns = c(game_quality, game_interest, home_prob),
      decimals = 3
    ) |>
    gt::cols_width(
      season ~ px(75),
      week ~ px(75),
      start_date ~ px(100),
      prediction ~ px(150),
      actual ~ px(150)
    ) |>
    gt::cols_label(
      season = "Season",
      week = "Week",
      start_date = "Game Time",
      home_team = "Home",
      away_team = "Away",
      game_quality = "Quality",
      game_interest = "Interest",
      home_prob = "Pr(Home Win)",
      prediction = "Prediction",
      actual = "Result"
    ) |>
    gt::data_color(
      columns = c("game_quality", "game_interest"),
      domain = c(0, 1),
      palette = c("orange", "white", "dodgerblue2")
    ) |>
    gt::data_color(
      columns = c("home_prob"),
      domain = c(0, 1),
      palette = c("white", "deepskyblue1")
    ) |>
    gt::opt_interactive(
      use_filters = T,
      page_size_default = 25,
      use_compact_mode = T,
      use_highlight = T
    )
}
