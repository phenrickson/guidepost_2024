tar_load(cfbd_recruiting_position_tbl)
tar_load(cfbd_recruiting_team)
tar_load(cfbd_team_talent_tbl)


tar_load(team_divisions)

cfbd_team_talent_tbl

team_talent = 
cfbd_team_talent_tbl |>
  inner_join(
    team_divisions |>
      rename(school = team, year = season)
  ) |>
  mutate(talent = as.numeric(talent))

my_fpi |>
  select(season, team, score) |>
  left_join(
    team_talent |>
      group_by(school) |>
      mutate(
        season = dplyr::lead(year, 1),
        team = school
      ) |>
      ungroup() |>
      select(season, team, talent, year)
  ) |>
  left_join(
    team_divisions
  ) |>
  filter(team != 'fcs') |> 
  filter(division !='fcs') |>
  mutate(talent = case_when(is.na(talent) ~ replace_na(talent, 75), TRUE ~ talent)) -> bar

bar |>
  ggplot(aes(x=talent, y=score, color = team, label = paste(team, season)))+
  geom_label(size = 2, alpha = 0.5)+
  theme_cfb()+
  cfbplotR::scale_color_cfb()

bar |>
  group_by(team) |>
  mutate(lag_score = lag(score, 1)) |>
  ungroup() |>
  filter(season >= min(season)) |>
  lm(score ~ talent, data = _) |>
  glance()

dat = 
bar  |>
  mutate(service_academy = case_when(team %in% c('Air Force', 'Navy', 'Army') ~ 1, TRUE ~ 0)) |>
  group_by(team) |>
  mutate(lag_score = lag(score, 1)) |>
  ungroup() |>
  filter(season > min(season))

mod = 
  dat |>
  lm(score ~ talent + lag_score + service_academy, data = _)

mod |> 
  augment(newdata = dat) |>
  select(season, team, score, .fitted, .resid) |>
  ggplot(aes(x=.fitted, y=score, color = team, label = paste(team, season)))+
  geom_label(size = 2, alpha = 0.8)+
  cfbplotR::scale_color_cfb()+
  theme_cfb()

new = 
team_talent |>
  filter(year == 2023) |>
  mutate(season = 2023) |>
  select(season, team = school, talent) |>
  left_join(
    my_fpi |>
      select(season, team, score)
  ) |>
  mutate(season = 2024, lag_score = score) |>
  select(season, team, talent, lag_score) |>
  mutate(talent = case_when(is.na(talent) ~ replace_na(talent, 75), TRUE ~ talent)) |>
  mutate(service_academy = case_when(team %in% c('Air Force', 'Navy', 'Army') ~ 1, TRUE ~ 0))


 
mod |>
  augment(
    newdata = new
  ) |> 
select(season, team, score = lag_score, talent, .fitted) |>
  ggplot(aes(x=.fitted, y=score, color = team, label = paste(team, season)))+
    geom_label(size = 2, alpha = 0.8)+
    cfbplotR::scale_color_cfb()+
    theme_cfb()+
  facet_grid(season ~.)+
  geom_abline()+
  coord_obs_pred()

