targets::tar_load_globals()

tar_load(cfbd_team_info_tbl)
tar_load(cfbd_game_info_tbl)
tar_load(cfbd_betting_lines_tbl)
tar_load(efficiency_by_week)
tar_load(cfb_season_weeks)

# efficiency_by_week |>
#   mutate(type = case_when(play_situation == 'special' & type == 'overall' ~ 'special',
#                           TRUE ~ type)) |>
#   anti_join(
#     tibble(type = c('offense', 'defense'),
#            play_situation = 'special')
#   )  |>
#   mutate(type = factor(type, levels = c('overall', 'offense', 'defense', 'special'))) |>
#   plot_team_efficiency_by_week(team = 'Wisconsin', label = F, point = F, line = T) |>
#   plot_ranking(ranking = c(25, 50))

prepare_game_weeks <- function(data, season_weeks) {
  data |>
    select(season, season_type, week, home_team, away_team, game_id, start_date) |>
    mutate(start_date = as.Date(start_date)) |>
    left_join(
      season_weeks |>
        distinct(),
      by = join_by(season, season_type, week),
      relationship = "many-to-many"
    ) |>
    mutate(diff = as.numeric(week_date - start_date)) |>
    filter(diff > 0) |>
    group_by(game_id) |>
    slice_min(diff, n = 1) |>
    ungroup() |>
    select(-diff) |>
    select(season, season_week, week, week_date, season_type, home_team, away_team, game_id, start_date) |>
    arrange(start_date, week_date)
}


cfbd_game_info_tbl |>
  prepare_game_weeks(cfb_season_weeks)

add_season_week <- function(data) {
  data |>
    mutate(week = as.numeric(stringr::str_sub(season_week, 6, 7)))
}

prepare_team_estimates <- function(data) {
  data |>
    arrange(week_date) |>
    # remove offense and defense special teams estimates; only using overall
    anti_join(
      tibble(
        type = c("offense", "defense"),
        play_situation = "special"
      ),
      by = join_by(play_situation, type)
    ) |>
    # get week from season week
    add_season_week() |>
    select(season, season_week, season_type, week_date, team, metric, type, estimate) |>
    pivot_wider(
      names_from = c("type"),
      values_from = c("estimate")
    ) |>
    select(season_week, season, season_type, week_date, team, any_of(c("overall", "offense", "defense", "special"))) |>
    rename_with(
      .cols = any_of(c("overall", "offense", "defense", "special")),
      .fn = ~ paste0("postgame_", .x)
    ) |>
    group_by(team) |>
    mutate(
      pregame_overall = dplyr::lag(postgame_overall, 1),
      pregame_offense = dplyr::lag(postgame_offense, 1),
      pregame_defense = dplyr::lag(postgame_defense, 1),
      pregame_special = dplyr::lag(postgame_special, 1)
    ) |>
    ungroup()
}

#
team_estimates <-
  efficiency_by_week |>
  prepare_team_estimates()




add_team_estimates <- function(data, estimates) {
  data |>
    inner_join(
      estimates |>
        select(season,
          season_type,
          season_week,
          home = team,
          home_overall = pregame_overall,
          home_offense = pregame_offense,
          home_defense = pregame_defense,
          home_special = pregame_special
        ),
      by = join_by(season, season_week, season_type, home)
    ) |>
    left_join(
      estimates |>
        select(
          season,
          season_type,
          season_week,
          away = team,
          away_overall = pregame_overall,
          away_offense = pregame_offense,
          away_defense = pregame_defense,
          away_special = pregame_special
        ),
      by = join_by(season, season_week, season_type, away)
    )
}

add_game_outcomes <- function(data) {
  data |>
    mutate(
      home_margin = home_points - away_points,
      home_win = case_when(
        home_points > away_points ~ "yes",
        home_points < away_points ~ "no"
      ),
      home_win = factor(home_win, levels = c("yes", "no")),
      total_points = home_points + away_points
    )
}

prepare_fcs_teams <- function(data) {
  # handle fcs case
  data |>
    mutate(
      home = case_when(is.na(home_division) | home_division != "fbs" ~ "fcs", TRUE ~ home_team),
      away = case_when(is.na(away_division) | away_division != "fbs" ~ "fcs", TRUE ~ away_team)
    )
}

game_weeks <- cfbd_game_info_tbl |>
  prepare_game_weeks(cfb_season_weeks)

team_estimates <-
  efficiency_by_week |>
  prepare_team_estimates()


prepare_game_estimates <- function(games, game_weeks, team_estimates) {
  games |>
    mutate(
      start_date = as.Date(start_date),
      neutral_site = case_when(neutral_site == T ~ 1, TRUE ~ 0)
    ) |>
    select(game_id, season, season_type, start_date, neutral_site, home_team, away_team, home_division, away_division, home_points, away_points) |>
    add_game_outcomes() |>
    prepare_fcs_teams() |>
    inner_join(
      game_weeks,
      by = join_by(game_id, season, season_type, start_date, home_team, away_team)
    ) |>
    add_team_estimates(team_estimates) |>
    select(
      season,
      season_type,
      season_week,
      week_date,
      game_id,
      neutral_site,
      start_date,
      home_team,
      away_team,
      home_points,
      away_points,
      total_points,
      home_margin,
      home_win,
      home,
      away,
      home_overall,
      home_offense,
      home_defense,
      home_special,
      away_overall,
      away_offense,
      away_defense,
      away_special
    )
}

games_and_estimates |>
  add_game_weights()

games_and_estimates <-
  cfbd_game_info_tbl |>
  prepare_game_estimates(game_weeks = game_weeks, team_estimates = team_estimates) |>
  mutate(
    weight = weights_from_dates(start_date, ref = "2015-01-01", base = .999),
    weight = recipes::importance_weights(weight)
  )

harmonic_mean = function(x, y) {

  2 / ((1 / x) + (1/y))
}



end_train_year <- 2021

train_games <-
  games_and_estimates |>
  filter(season <= end_train_year)

valid_games <-
  games_and_estimates |>
  filter(season > end_train_year)

model <-
  train_games |>
  lm(home_margin ~ home_offense + home_defense + home_special + away_offense + away_defense + away_special + neutral_site, data = _)

model <-
  workflow() |>
  add_model(
    linear_reg() |>
      set_engine("stan")
  ) |>
  add_formula(
    home_margin ~ home_offense + home_defense + home_special + away_offense + away_defense + away_special + neutral_site,
  ) |>
  add_case_weights(weight) |>
  fit(train_games)


simulateX <- function(object, nsim = 1, seed = NULL, X, ...) {
  object$fitted.values <- predict(object, X)
  simulate(object = object, nsim = nsim, seed = seed, ...)
}

# sims =
#   model |>
#   simulateX(
#     nsim = 1000,
#     X = games_and_estimates |>
#       filter(season == 2022)
#   )
#
# sims |>
#   bind_cols(
#     games_and_estimates |>
#       filter(season == 2022) |>
#       select(game_id, season, home, away, home_margin)
#   ) |>
#   as_tibble() |>
#   pivot_longer(cols = starts_with("sim_")) |>
#   left_join(
#     games_and_estimates |>
#       select(game_id, home_margin)
#   ) |>
#   nest(-game_id) |>
#   sample_n(10) |>
#   unnest() |>
#   mutate(game = paste(home, away, season)) |>
#   ggplot(aes(x=value))+
#   facet_wrap(game ~.,
#              ncol = 2)+
#   geom_histogram(bins = 50)+
#   geom_vline(aes(xintercept = home_margin), color = 'red')+
#   theme_cfb()
#
# preds |>
#   filter(season == 2022,
#          home == 'South Carolina',
#          away == 'Texas A&M') |>
#   select(season, game_id, home, away, home_margin, pred_margin)
#

preds <-
  model |>
  augment(
    new_data =
      valid_games
  ) |>
  mutate(
    home_pred = case_when(.pred >= 0 ~ "yes", .pred < 0 ~ "no"),
    home_pred = factor(home_pred, levels = c("yes", "no"))
  ) |>
  mutate(correct = case_when(home_pred == home_win ~ "yes", TRUE ~ "no")) |>
  rename(pred_margin = .pred) |>
  add_season_week()

preds |>
  group_by(season, season_type, week, week_date) |>
  mutate(games = n_distinct(game_id)) |>
  group_by(season, season_type, week, week_date, games) |>
  yardstick::accuracy(
    home_win,
    home_pred
  ) |>
  print(n = 25) |>
  arrange(week_date)

preds |>
  group_by(season) |>
  yardstick::accuracy(
    truth = home_win,
    home_pred
  )

simulate_games = function(object, newdata, ndraws = 1000, ...) {

  if (class(object) == 'workflow') {

    model = object |>
      hardhat::extract_fit_engine()

  } else if (inherits(model |> extract_fit_engine(), "stanreg")) {

    model = object

  } else {

    error("requires stanreg")

  }

  model |>
  tidybayes::predicted_draws(newdata = newdata, ndraws = ndraws, ...)
}

summarize_simulations = function(simulations) {

  simulations |>
  mutate(
    home_win = case_when(.prediction > 0 ~ 'yes',
  .prediction == 0 ~ sample(c("yes", "no"), size = 1, replace = T),
  .prediction < 0 ~ 'no'),
    home_win = factor(home_win, levels = c("yes", "no"))) |>
  group_by(game_id, home_team, away_team) |>
  summarize(
    home_wins = sum(home_win == 'yes'),
    pred_margin = mean(.prediction),
    sims = n_distinct(.draw),
  .groups = 'drop') |>
  mutate(home_prob = home_wins / sims) |>
  select(game_id, home_team, away_team, pred_margin, home_prob)
}

draws = 
  model |>
  simulate_games(newdata = valid_games)

sims = 
  draws |>
  summarize_simulations()


preds |>
  select(-pred_margin) |>
  left_join(
    sims
  ) |>
  group_by(season) |>
  yardstick::rmse(
    truth = home_margin,
    pred_margin
  )

preds |>
  ggplot(aes(
    x = pred_margin,
    color = correct,
    shape = season_type,
    label = paste(paste(home_team, away_team, sep = " vs "), paste(home_points, away_points, sep = "-")),
    y = home_margin
  )) +
  geom_point() +
  geom_text(size = 2, check_overlap = T, vjust = -1.25) +
  theme_cfb() +
  scale_color_manual(values = c("red", "dodgerblue2")) +
  coord_obs_pred() +
  facet_wrap(season ~ .) +
  xlab("Predicted Margin") +
  ylab("Actual Margin") +
  guides(color = "none") +
  scale_shape_manual(values = c(1, 19)) +
  geom_abline()

team_overalls <-
  model |>
  augment(
    new_data =
      team_estimates |>
        mutate(season_week,
          season,
          season_type,
          team,
          home_offense = postgame_offense,
          home_defense = postgame_defense,
          home_special = postgame_special,
          away_offense = 0,
          away_defense = 0,
          away_special = 0,
          neutral_site = 1,
          .keep = "none"
        )
  ) |>
  add_season_week() |>
  select(season, season_type, season_week, week, everything())

team_scores = 
  team_overalls |>
  select(
    season, 
    season_type,
    season_week, 
    team,
    overall = .pred,
    offense = home_offense,
    defense = home_defense,
    special = home_special
  )

team_scores |>
  pivot_longer(
    cols = c(overall, offense, defense, special),
    names_to = c("type"),
    values_to = "estimate"
  )|>
  add_team_ranks(groups = c("season", "season_week", "type")) |>
  filter(team == 'Pittsburgh') |>
  arrange(rank)


preds |>
  select(season_week, game_id, home_team, away_team, home_overall, away_overall, pred_margin, home_margin, correct) |>
  filter(home_team == 'Pittsburgh' | away_team == "Pittsburgh")



add_team_overalls = function(data, estimates) {

  data |>
    left_join(
      estimates |>
      select(
      season,
      season_type,
      season_week,
      home = team,
      home_overall = overall
      )
    )    |>
    left_join(
    estimates |>
    select(
      season,
    season_type,
    season_week,
    away = team,
    away_overall = overall
    )
  )
}

calculate_game_quality = function(data, groups = NULL) {

  data |>
  mutate(across(c(home_overall, away_overall), normalize)) |>
  group_by(across(any_of(groups))) |>
  mutate(
    game_quality = harmonic_mean(home_overall, away_overall),
    game_quality = normalize(game_quality)
  ) |>
  ungroup()
}


harmonic_mean = function(x, y) {

  2 / ((1 / x) + (1/y))

}
  
normalize = function(x) {

  (x - min(x)) / ((max(x) - min(x)))
}

  
games_and_estimates |>
  select(season,season_type, season_week, week_date, game_id, home_team, away_team, home, away, neutral_site) |>
  add_team_overalls(team_scores) |>
  filter(season == 2023) |>
  calculate_game_quality() |>
  arrange(desc(game_quality)) |>
  print(n = 25)
  arrange(desc(game_quality)) |>
  filter(season == 2023)
  ggplot(aes(x=game_quality))+
  geom_histogram()
  arrange(desc(game_quality))
  group_by(season) |>
  mutate(
    game_quality = ((game_quality - min(game_quality)) / ((max(game_quality) - min(game_quality))))
  )


team_overalls |>
  filter(season == 2022) |>
  filter(week == max(week)) |>
  ggplot(aes(
    x = as.factor(season),
    label = team,
    y = .pred
  )) +
  geom_label(position = ggforce::position_auto(seed = 1)) +
  theme_cfb()


my_fpi <-
  team_estimates |>
  left_join(
    team_overalls
  ) |>
  select(season_week, season, season_type, week, team,
    overall = postgame_overall,
    offense = postgame_offense,
    defense = postgame_defense,
    special = postgame_special,
    score = .pred
  ) |>
  group_by(season) |>
  slice_max(week, n = 1) |>
  ungroup()

espn_fpi <-
  map(
    c(2017, 2018, 2019, 2020, 2021, 2022, 2023),
    ~ cfbfastR::espn_ratings_fpi(year = .x) |>
      as_tibble()
  ) |> list_rbind()


tar_load(cfbd_team_info_tbl)

combined <-
  espn_fpi |>
  select(
    season = year,
    team_id,
    team = team_name,
    fpi
  ) |>
  inner_join(
    cfbd_team_info_tbl |>
      select(season, team_id, school)
  ) |>
  left_join(
    my_fpi |>
      rename(school = team)
  ) |>
  select(season, team = school, fpi, season_week, overall, offense, defense, score, fpi)

combined |>
  mutate(fpi = as.numeric(fpi)) |>
  ggplot(aes(x = score, y = fpi)) +
  geom_point() +
  facet_wrap(season ~ .) +
  theme_cfb() +
  ggpubr::stat_cor(method = ) +
  xlab("Phil'S CFB Team Score") +
  ylab("ESPN CFB FPI") +
  geom_abline() +
  coord_obs_pred()


phil_fpi <-
  team_estimates |>
  left_join(
    team_overalls
  ) |>
  select(season_week,
    season,
    season_type,
    week,
    team,
    overall = postgame_overall,
    offense = postgame_offense,
    defense = postgame_defense,
    special = postgame_special,
    score = .pred
  )

plot_team_fpi <- function(data, team, rankings = c(10, 25, 50)) {
  data |>
    select(-overall, -week) |>
    mutate(overall = score) |>
    pivot_longer(
      cols = c(overall, offense, defense, special),
      names_to = c("type"),
      values_to = c("estimate")
    ) |>
    mutate(type = factor(type, levels = c("overall", "offense", "defense", "special"))) |>
    plot_team_efficiency_by_week(team = team, label = F, point = F, line = T) |>
    plot_ranking(ranking = rankings) +
    ylab("Team Net Points per Play    ")
}

phil_fpi |>
  plot_team_fpi(team = "Texas A&M")

phil_fpi |>
  filter(team == 'Texas A&M') |>
  arrange(desc(score))

foo <-
  phil_fpi |>
  inner_join(
    cfbd_team_info_tbl |>
      select(season, team = school, abbreviation, conference)
  ) |>
  filter(conference == "SEC") |>
  pivot_longer(
    cols = c(overall, offense, defense, special, score),
    names_to = c("type"),
    values_to = c("estimate")
  ) |>
  filter(type == "score") |>
  group_by(team) |>
  mutate(
    label_max = case_when(week == max(week) ~ abbreviation),
    label_min = case_when(week == min(week) ~ abbreviation)
  ) |>
  ungroup()

foo |>
  ggplot(aes(
    x = week,
    y = estimate,
    color = team,
    group = team
  )) +
  geom_line(stat = "smooth", span = 0.15) +
  cfbplotR::scale_color_cfb() +
  theme_cfb() +
  facet_grid(conference ~ season) +
  coord_cartesian(xlim = c(-3, 23), clip = "off") +
  # ggtitle(paste("Team Efficiency by Season", unique(foo$season), sep = " - "))+
  geom_label(
    aes(label = label_min),
    hjust = 1.2,
    size = 2,
    alpha = 0.8
  ) +
  geom_label(
    aes(label = label_max),
    hjust = -0.5,
    size = 2,
    alpha = 0.8
  )


phil_fpi |>
  select(-overall) |>
  rename(overall = score) |>
  group_by(season_week, season, season_type, week) |>
  arrange(desc(overall)) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  arrange(season, desc(season_type), week) |>
  filter(team == "Texas A&M") |>
  print(n = 500)

phil_fpi |>
  filter(season > 2017) |>
  select(-overall) |>
  rename(overall = score) |>
  group_by(season_week, season, season_type, week) |>
  mutate(rank = rank(-overall)) |>
  ungroup() |>
  group_by(team) |>
  mutate(rank_pre = dplyr::lag(rank, 1), rank_post = rank) |>
  mutate(diff = -(rank_post - rank_pre)) |>
  ungroup() |>
  filter(team == "Texas") |>
  slice_max(diff, n = 10) |>
  left_join(
    cfb_season_weeks |>
      select(season_week, week_date)
  ) |>
  select(season, season_week, week_date, everything())

combined |>
  filter(team == "Georgia") |>
  select(season, team, espn_fpi = fpi, my_score = score)

# preds |>
#   mutate(correct = case_when(home_win == home_pred ~ 'yes', TRUE ~ 'no')) |>
#   filter(season_type == 'postseason') |>
#   group_by(season) |>
#   slice_tail(n = 10) |>
#   ungroup() |>
#   select(start_date, season, home_team, away_team, home_margin, pred_margin, home_win, home_pred, correct) |>
#   mutate_if(is.numeric, round, 1)
#
# # mutate(
# #   home_pred = 'yes',
# #   home_pred = factor(home_pred, levels = c("yes", "no"))
# # ) |>
# mutate(
#   home_pred = case_when(.fitted >= 0 ~ 'yes',
#                         .fitted < 0 ~ 'no'),
#   home_pred = factor(home_pred, levels = c("yes", "no"))
# ) |>
#   # group_by(home_division, away_division) |>
#   yardstick::accuracy(
#     truth = home_win,
#     home_pred
#   )
#
# yardstick::rmse(
#   truth = home_margin,
#   estimate = .fitted
# )
# ggplot(aes(x=start_date, y=.estimate))+
#   geom_point()
#
# ggplot(aes(x=home_margin, y= .fitted))+
#   geom_point()
# dat |>
#   filter(season == 2022)
# mutate(
#   correct = case_when(home_margin < 0 & .fitted < 0 ~ 'yes',
#                       home_margin > 0 & .fitted > 0 ~ 'yes',
#                       TRUE ~ 'no')
# ) |>
#   group_by(correct) |>
#   count()
# ggplot(aes(x=home_margin,
#            y=.fitted))+
#   geom_point() +
#   coord_obs_pred()
#
# group_by(team) |>
#   slice_max(week) |>
#   ungroup() |>
#   pivot_wider(names_from = c("type"),
#               values_from = c("estimate")) |>
#   select(-season_week) |>
#   arrange(desc(overall)) |>
#   mutate(score = overall * 65,
#          rank = row_number()) |>
#   select(rank, season, week, team, overall, score) |>
#   mutate_if(is.numeric, round, 2) |>
#   gt::gt()
#
#
#
#
