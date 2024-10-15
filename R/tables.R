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

add_gt_formatting = function(tbl, ...) {
  
  tbl |>
    gt::opt_row_striping(row_striping = F) |>
    gt::tab_options(table.font.size = 14,
                    ...)
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

gt_correct_color = function(tbl) { 
  
  tbl |>
    tab_style(
    style = cell_fill(color = "deepskyblue"),
    locations = cells_body(
      columns = "actual",
      rows = correct == "yes"
    )
  )
}

# make table of game predictions
game_predictions_tbl <- function(data) {
  data |>
    gt_tbl() |>
    gt::fmt_number(
      columns = c(game_interest, game_quality),
      decimals = 0
    ) |>
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
      columns = c(home_prob),
      decimals = 3
    ) |>
    gt::cols_width(
      season ~ px(75),
      week ~ px(75),
      game_quality ~ px(85),
      game_interest ~ px(85),
      home_prob ~ px(85),
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
      domain = c(0, 100),
      palette = c("orange", "white", "dodgerblue3")
    ) |>
    gt::data_color(
      columns = c("home_prob"),
      domain = c(0, 1),
      palette = c("white", "deepskyblue1")
    ) |>
    gt::opt_interactive(
      use_filters = T,
      page_size_default = 15,
      use_compact_mode = T,
      use_highlight = T
    ) |>
    gt_correct_color()
}

prepare_team_category_estimates = function(data) {
  
  data |>
    add_team_ranks(groups = c("season", "season_week", "play_category", "type")) |>
    pivot_wider(names_from = c("play_category", "type"),
                values_from = c("estimate", "rank")) |>
    select(team, season, season_week, contains("pass_offense"), contains("rush_offense"), contains("pass_defense"), contains("rush_defense")) |>
    arrange(desc(estimate_pass_offense)) |>
    add_season_week() |>
    mutate(logo = team) |>
    select(season, season_week, week, logo, team, everything()) |>
    arrange(desc(estimate_rush_offense))
}

team_category_estimates_tbl = function(data) {
  
  data |>
    gt_tbl() |>
    gt::fmt_number(
      contains("estimate"),
      decimals = 3
    ) |>
    gt::cols_merge(
      columns = contains("pass_offense"),
      pattern = "{1}<< ({2})>>"
    ) |>
    gt::cols_merge(
      columns = contains("rush_offense"),
      pattern = "{1}<< ({2})>>"
    ) |>
    gt::cols_merge(
      columns = contains("pass_defense"),
      pattern = "{1}<< ({2})>>"
    ) |>
    gt::cols_merge(
      columns = contains("rush_defense"),
      pattern = "{1}<< ({2})>>"
    ) |>
    gt::cols_label(
      season = "Season",
      week = "Week",
      team = "Team",
      estimate_pass_offense = "Pass Offense",
      estimate_rush_offense = "Run Offense",
      estimate_pass_defense = "Pass Defense",
      estimate_rush_defense = "Run Defense"
    )  |>
    gt::cols_hide(
      c(season_week, week)
    ) |>
    gt::cols_width(
      season ~ px(75),
      week ~ px(75)
    ) |>
    gt::cols_align(
      c(contains("offense"), contains("defense"), "season", "week"),
      align = "center"
    ) |>
    gt_est_color(columns = c(contains("estimate")),
                 domain = c(-0.75, 0.75)) |>
    gt::opt_interactive(
      page_size_default = 15,
      use_filters = T
    ) |>
    cfbplotR::gt_fmt_cfb_logo(columns = "logo") |>
    gt::cols_label(
      logo = "Logo"
    ) |>
    gt::cols_width(
      logo ~ px(75)
    ) |>
    gt::cols_align(
      align = "center",
      columns = "logo"
    )
}

efficiency_overall_tbl = function(data, hide_special = F) {
  
  tab = 
    data |>
    gt_tbl() |>
    # estimates
    gt_est_color(
      columns = contains("estimate")
    ) |>
    # overall
    gt_est_color(
      columns = c("estimate_overall"),
      domain = c(-40, 40)
    ) |>
    gt::fmt_number(
      columns = contains("estimate"),
      decimals = 3
    ) |>
    gt::cols_merge(
      contains("overall"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_merge(
      contains("offense"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_merge(
      contains("defense"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_merge(
      contains("special"),
      pattern = "<<{1} ({2})>>"
    ) |>
    gt::cols_label(
      estimate_overall = "overall",
      estimate_offense = "offense",
      estimate_defense = "defense",
      estimate_special = "special"
    ) |>
    gt::cols_align(
      columns = c(contains("estimate"), contains("rank")),
      align = "center"
    ) |>
    gt::tab_spanner(
      label = "net points per play",,
      columns = c("estimate_offense", "estimate_defense", "estimate_special")
    ) |>
    gt::tab_spanner(
      label = "estimated score",
      columns = c("estimate_overall")
    )
  
  if (hide_special == T) {
    
    tab |>
      gt::cols_hide("estimate_special")
  } else {
    tab
  }
}


prepare_efficiency_overall_tbl = function(data) {
  
  data |>
    find_team_season_score() |>
    rename(overall = score) |>
    pivot_longer(
      cols = c(overall, offense, defense, special),
      names_to = c("type"),
      values_to = c("estimate")
    ) |>
    add_team_ranks(groups = c("season", "season_week", "week", "type")) |>
    select(season, team, type, estimate, rank) |>
    pivot_wider(
      names_from = c("type"),
      values_from = c("estimate", "rank")
    ) |>
    select(season,
           team,
           starts_with("estimate_"),
           starts_with("rank_")
    )
}

team_efficiency_overall_tbl = function(data, teams, ...) {
  
  data |>
    prepare_efficiency_overall_tbl() |>
    inner_join(
      tibble(team = teams),
      by = join_by(team)
    ) |>
    efficiency_overall_tbl(...)
}
