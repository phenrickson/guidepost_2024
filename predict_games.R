targets::tar_load_globals()

tar_load(cfbd_team_info_tbl)
tar_load(cfbd_game_info_tbl)
tar_load(cfbd_betting_lines_tbl)
tar_load(efficiency_by_week)

tar_load(cfb_season_weeks)

efficiency_by_week |>
  mutate(type = case_when(play_situation == 'special' & type == 'overall' ~ 'special',
                          TRUE ~ type)) |>
  anti_join(
    tibble(type = c('offense', 'defense'),
           play_situation = 'special')
  )  |>
  mutate(type = factor(type, levels = c('overall', 'offense', 'defense', 'special'))) |>
  plot_team_efficiency_by_week(team = 'Wisconsin', label = F, point = F, line = T) |>
  plot_ranking(ranking = c(25, 50))

games =
  cfbd_game_info_tbl |>
  select(season, season_type, week, home_team, away_team, game_id, start_date) |>
  mutate(start_date = as.Date(start_date))

# 
game_weeks =
  games |>
  left_join(
    cfb_season_weeks |>
      select(-week_date) |>
      distinct(),
    by = join_by(season, season_type, week)
  ) |>
  group_by(game_id) |>
  slice_head(n=1) |>
  ungroup()
# 
team_estimates =
  efficiency_by_week |>
  anti_join(
    tibble(type = c('offense', 'defense'),
           play_situation = 'special')
  ) |>
  mutate(week = as.numeric(stringr::str_sub(season_week, 6, 7))) |>
  select(-play_situation) |>
  pivot_wider(names_from = c("type"),
              values_from = c("estimate")) |>
  select(season_week, season, season_type, week, team, overall, offense, defense, special) |>
  rename_with(
    .cols = c(overall, offense, defense, special),
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

games_and_estimates =
  cfbd_game_info_tbl |>
  mutate(start_date = as.Date(start_date)) |>
  select(game_id, season, season_type, week, start_date, neutral_site, home_team, away_team, home_division, away_division, home_points, away_points) |>
  mutate(
    home_margin = home_points - away_points,
    home_win = case_when(home_points > away_points ~ 'yes',
                         home_points < away_points ~ 'no'),
    home_win = factor(home_win, levels = c("yes", "no")),
    neutral_site = case_when(neutral_site == T ~ 1,
                             TRUE ~ 0)
  ) |>
  inner_join(
    game_weeks
  ) |>
  mutate(home = home_team,
         away = away_team,
         home_team = case_when(home_division != 'fbs' ~ 'fcs',
                               TRUE ~ home_team),
         away_team = case_when(away_division != 'fbs' ~ 'fcs',
                               TRUE ~ away_team)
  ) |>
  inner_join(
    team_estimates |>
      select(season,
             season_type,
             season_week,
             home_team = team,
             home_overall = pregame_overall,
             home_offense = pregame_offense,
             home_defense = pregame_defense,
             home_special = pregame_special
      )
  ) |>
  left_join(
    team_estimates |>
      select(season_week,
             away_team = team,
             away_overall = pregame_overall,
             away_offense = pregame_offense,
             away_defense = pregame_defense,
             away_special = pregame_special
      )
  ) |>
  mutate(
    home_overall_diff = home_overall - away_overall,
    home_offense_diff = home_offense - away_offense,
    home_defense_diff = home_defense - away_defense,
    total_points = home_points + away_points
  ) |>
  filter(season > 2017)

train_games = 
  games_and_estimates |>
  filter(season < 2022)

valid_games = 
  games_and_estimates |>
  filter(season >= 2022)

model =
  train_games |>
  lm(home_margin ~ home_offense + home_defense + home_special + away_offense + away_defense + away_special + neutral_site, data = _)


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
#   select(season, game_id, home, away, home_margin, predicted_margin)
# 

model |>
  augment(
    newdata =
      games_and_estimates |>
      filter(season == 2022)
  ) |>
  select(season, game_id, home, away, home_margin, .fitted)

preds =
  model |>
  augment(
    newdata =
      valid_games
  ) |>
  mutate(
    home_pred = case_when(.fitted >= 0 ~ 'yes',
                          .fitted < 0 ~ 'no'),
    home_pred = factor(home_pred, levels = c("yes", "no"))
  ) |>
  mutate(correct = case_when(home_pred == home_win ~ 'yes', TRUE ~ 'no')) |>
  rename(predicted_margin = .fitted)

preds |>
  group_by(season, week) |>
  yardstick::accuracy(
    home_win,
    home_pred
  ) |>
  print(n = 25)

preds |>
  group_by(season) |>
  yardstick::accuracy(
    truth = home_win,
    home_pred
  )

preds |>
  group_by(season) |>
  yardstick::rmse(
    truth = home_margin,
    predicted_margin
  )

preds |>
  ggplot(aes(x=predicted_margin,
             color = correct,
             shape = season_type,
             label = paste(paste(home, away, sep = " vs "), paste(home_points, away_points, sep = "-")),
             y=home_margin))+
  geom_point()+
  geom_text(size = 2, check_overlap = T, vjust = -1.25)+
  theme_cfb()+
  scale_color_manual(values = c("red", "dodgerblue2"))+
  coord_obs_pred()+
  facet_wrap(season ~.)+
  xlab("Predicted Margin")+
  ylab("Actual Margin")+
  guides(color = 'none')+
  scale_shape_manual(values = c(1, 19))+
  geom_abline()

team_overalls = 
  model |>
  augment(
    newdata = 
      team_estimates |>
      mutate(season_week, 
             season,
             team,
             home_offense = postgame_offense,
             home_defense = postgame_defense,
             home_special = postgame_special,
             away_offense = 0,
             away_defense = 0,
             away_special = 0,
             neutral_site = 1,
             .keep = 'none')
  ) |>
  mutate(week = as.numeric(stringr::str_sub(season_week, 6, 7))) |>
  select(season, season_week, week, everything())

team_overalls |>
  filter(season == 2022) |>
  filter(week == max(week)) |>
  ggplot(aes(x = as.factor(season),
             label = team,
             y = .fitted))+
  geom_label(position = ggforce::position_auto(seed = 1))

team_overalls |>
  filter(season == 2020) |>
  filter(week == max(week)) |>
  select(season, week, team, .fitted) |>
  arrange(desc(.fitted))

my_fpi = 
  team_estimates |>
  left_join(
    team_overalls
  ) |>
  select(season_week, season, season_type, week, team,
         overall = postgame_overall,
         offense = postgame_offense,
         defense = postgame_defense,
         special = postgame_special,
         score = .fitted) |>
  group_by(season) |> 
  slice_max(week, n =1) |>
  ungroup()

espn_fpi = 
  map(c(2017, 2018, 2019, 2020, 2021, 2022, 2023),
      ~ cfbfastR::espn_ratings_fpi(year = .x) |>
        as_tibble()
  ) |> list_rbind()


tar_load(cfbd_team_info_tbl)

combined = 
  espn_fpi |>
  select(season = year,
         team_id,
         team = team_name,
         fpi) |>
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
  ggplot(aes(x=score, y=fpi))+
  geom_point()+
  facet_wrap(season ~.)+
  theme_cfb()+
  ggpubr::stat_cor(method =)+
  xlab("Phil'S CFB Team Score")+
  ylab("ESPN CFB FPI")+
  geom_abline()+
  coord_obs_pred()


phil_fpi = 
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
         score = .fitted) 

phil_fpi |>
  select(-overall, -week) |>
  mutate(overall = score) |>
  pivot_longer(cols = c(overall, offense, defense, special),
               names_to = c("type"),
               values_to = c("estimate")) |>
  mutate(type = factor(type, levels = c("overall", "offense", "defense", "special"))) |>
  plot_team_efficiency_by_week(team = 'Texas A&M', label = F, point = F, line = T) |>
  plot_ranking(ranking = c(25, 50))+
  ylab("Team Net Points per Play                ")

  
  # preds |>
  #   mutate(correct = case_when(home_win == home_pred ~ 'yes', TRUE ~ 'no')) |>
  #   filter(season_type == 'postseason') |>
  #   group_by(season) |>
  #   slice_tail(n = 10) |>
  #   ungroup() |>
  #   select(start_date, season, home_team, away_team, home_margin, predicted_margin, home_win, home_pred, correct) |>
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