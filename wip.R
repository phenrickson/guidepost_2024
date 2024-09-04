targets::tar_load_globals()

# additional packages
library(cfbplotR)
library(ggforce)
library(stringr)

# source code
targets::tar_source("R")

# load data
tar_load(cfbd_conferences_tbl)
tar_load(cfbd_game_info_tbl)
tar_load(cfbd_team_info_tbl)
tar_load(pbp_predicted)

# functions
calculate_efficiency_by_type <- function(data, groups = c("season", "type", "team")) {
  data |>
  pivot_longer(
    cols = c(offense, defense),
    names_to = c("type"),
    values_to = c("team")
  ) |>
  group_by(across(any_of(groups))) |>
  summarize(
    ppa = mean(predicted_points_added, na.rm = T),
    epa = mean(expected_points_added, na.rm = T),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(ppa, epa),
    names_to = c("metric"),
    values_to = c("estimate")
  ) |>
  mutate(
    type = factor(type, levels = c("offense", "defense")),
    estimate = case_when(
      grepl("defense", type) ~ -1 * estimate,
      TRUE ~ estimate
    )
  )
}

add_overall_efficiency <- function(data, metric = "ppa") {
  data |>
  filter(metric == metric) |>
  pivot_wider(
    names_from = c("type"),
    values_from = c("estimate")
  ) |>
  mutate(
    overall = offense + defense
  ) |>
  select(season, team, metric, overall, offense, defense) |>
  pivot_longer(
    cols = c("overall", "offense", "defense"),
    names_to = c("type"),
    values_to = c("estimate")
  ) |>
  mutate(
    type = factor(type, levels = c("overall", "offense", "defense"))
  )
}

plot_team_efficiency <- function(data, teams = "Texas A&M", seed = 1) {
  selected_teams <- tibble(team = teams)

  all_teams_data <-
  data |>
  mutate(season = factor(season))

  all_teams_plot <-
  all_teams_data |>
  ggplot(aes(
    x = season,
    y = estimate
  )) +
  geom_point(
    alpha = 0.25,
    shape = 19,
    color = "grey60",
    position = position_auto(scale = F, seed = seed)
  ) +
  facet_grid(type ~ ., scales = "free_y") +
  guides(color = "none") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_cfb() +
  theme(axis.text.x = element_text(size = 8))

  team_data <- all_teams_data |>
  inner_join(selected_teams, by = join_by(team))

  all_teams_plot +
  geom_point(
    data = team_data,
    aes(color = team)
  ) +
  geom_line(
    data = team_data,
    aes(group = team, color = team),
    lwd = 1.2,
    alpha = 0.8
  ) +
  cfbplotR::scale_colour_cfb() +
  theme(
    axis.text.x = element_text(size = 8),
    plot.title = element_text(h = 0.5, size = 16)
  ) +
  labs(title = paste(teams, sep = ","))
}


theme_set(theme_cfb())

team_divisions <-
cfbd_game_info_tbl |>
select(season, home_team, away_team, home_division, away_division) |>
find_team_divisions()

team_conferences <-
cfbd_team_info_tbl |>
select(
  season,
  school,
  conference,
  division
) |>
distinct()

add_score_effects = function(data) {

  data |>
  mutate(
    home_score = case_when(
      drive_is_home_offense == T ~ offense_score,
      drive_is_home_offense == F ~ defense_score
    ),
    away_score = case_when(
      drive_is_home_offense == T ~ defense_score,
      drive_is_home_offense == F ~ offense_score
    ),
    home_margin = home_score - away_score,
    garbage_time = case_when(
      period == 2 & abs(home_margin) >= 38 ~ 1,
      period == 3 & abs(home_margin) > 28 ~ 1,
      period == 4 & abs(home_margin) > 22 ~ 1,
      TRUE ~ 0
    )
  )
}

expected_points <-
pbp_predicted |>
inner_join(
  cfbd_game_info_tbl |>
  select(season, game_id, neutral_site, week)
) |>
select(
  season,
  week,
  game_id,
  half,
  period,
  drive_id,
  play_id,
  play_type,
  neutral_site,
  drive_is_home_offense,
  home,
  away,
  offense,
  defense,
  offense_score,
  defense_score,
  play_text,
  contains("expected_points"),
  predicted_points_added
)

add_home_field = function(data) {

  data |>
  mutate(
    home_field_advantage = case_when(
      drive_is_home_offense == 1 & neutral_site == 0 ~ 1,
      TRUE ~ 0
    )
  )

}

join_team_divisions = function(data, games = cfbd_game_info_tbl) {

  data |>
  left_join(
    games |>
    select(game_id, season_type, home = home_team, away = away_team, home_division, away_division)
  )
}

dat =
expected_points |>
add_play_category() |>
add_score_effects() |>
add_home_field() |>
join_team_divisions() |>
filter(!is.na(predicted_points_added), season_type == "regular") |>
factor_team_names() |>
select(
  season,
  week,
  game_id,
  play_id,
  play_type,
  play_category,
  play_text,
  neutral_site,
  home,
  away,
  home_division,
  away_division,
  offense,
  defense,
  garbage_time,
  home_field_advantage,
  predicted_points_added,
  expected_points_added
)

effects_recipe =
dat |>
build_recipe(
  outcome = predicted_points_added,
  predictors = c("offense", "defense", "home_field_advantage"),
  ids = c("season", "game_id")
) |>
step_dummy(
  offense,
  defense
)

glmnet_spec =
linear_reg(
  penalty = 0.001,
  mixture = 0
)|>
set_engine("glmnet")

lm_spec =
linear_reg()

effects_wflow =
workflow() |>
add_model(
  lm_spec
) |>
add_recipe(
  effects_recipe
)

effects_fit =
workflow() |>
add_recipe(

)
add_model(
  lm_spec,
  formula =
  predicted_points_added ~ 0 + home_field_advantage + offense + defense
) |>
fit(
  dat |> filter(season == 2023)
)

foo =
glmnet_spec |>
fit(
  predicted_points_added ~ home_field_advantage + offense_ + defense_,
  data =
  dat |>
  filter(season == 2021) |>
  filter(garbage_time == 0) |>
  rename(
    offense_ = offense,
    defense_ = defense
  )
)

intercept =
foo |>
tidy() |>
filter(term == "(Intercept)") |>
  pull(estimate)

effects =
foo |>
tidy() |>
filter(term != "home_field_advantage" & term != "(Intercept)") |>
  mutate(
    type = sub("_.*", "", term),
    team = sub("^[^_]*_", "", term)
  ) |>
  select(type, team, everything()) |>
  mutate(adjusted = estimate + intercept)

effects |>
  select(type, team, adjusted) |>
  pivot_wider(
    names_from = c("type"),
    values_from = c("adjusted")
  ) |>
  mutate(
    defense = defense,
    overall = offense + defense
  ) |>
  arrange(desc(overall))

effects |>
filter(term == 'offense_fcs' | term == 'defense_fcs')

separate(
  col = term, into = c("type", "team")
)
filter(grepl("Texas", term))

$$PPA  = Offense_i + Defense_j + HomeFieldAdvantage

dat |>
nest(-c(season, week))


effects_fit |>
tidy()

# using functions
expected_points_by_type <-
expected_points |>
calculate_efficiency_by_type() |>
add_overall_efficiency() |>
filter_to_fbs()

foo =
expected_points |>
add_play_category(remove = F)

team_efficiency_by_category =
foo |>
calculate_efficiency_by_type(groups = c("season", "type", "team", "play_category")) |>
filter_to_fbs()

team_efficiency_overall =
foo |>
calculate_efficiency_by_type(groups = c("season", "type", "team")) |>
add_overall_efficiency() |>
filter_to_fbs()

team_efficiency_overall |>
filter(metric == 'ppa') |>
plot_team_efficiency(team = 'Wisconsin') +
facet_grid(type ~ ., scales = "free_y")


team_efficiency_by_category |>
filter(metric == "epa") |>
filter(type == 'offense', play_category != 'special') |>
plot_team_efficiency(team = "Colorado") +
facet_grid(play_category~ type, scales = "free_y")

# ## Examining Seasons
# ### 2023
# top_plays_by_season <- function(plays, var = predicted_points_added, n = 10) {
#     plays |>
#       group_by(season) |>
#       slice_max(abs({{ var }}), n = n) |>
#       group_by(season)
# }

# top_plays_2023 <-
#   pbp_fit |>
#   augment(
#     prepared_pbp |>
#       filter(season == 2023)
#   ) |>
#   calculate_expected_points() |>
#   calculate_points_added()

# top_plays_2023 |>
#     top_plays_by_season() |>
#     plays_p
