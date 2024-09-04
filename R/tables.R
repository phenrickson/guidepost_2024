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
