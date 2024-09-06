# # efficiency in season
# targets::tar_load_globals()
# 
# # source code
# targets::tar_source("R")
# 
# # objects
# tar_load(team_divisions)
# tar_load(cfbd_game_info_tbl)
# tar_load(cfbd_calendar_tbl)
# tar_load(pbp_efficiency)
# 
# df = tibble(x = 1:1000)
# 
# map(c(.9, .99, .995, .999),
#     ~ df |>
#       mutate(weight = .x ^ x,
#              base = factor(.x))) |>
#   list_rbind() |>
#   ggplot(aes(x=x, y=weight, color = base))+
#   geom_line()+
#   theme_cfb()
# 
# weights_from_dates <- function(x, ref, base, max = 365 * 2) {
#   case_when(
#     # if game date greater than or equal to max then 1
#     x >= ref ~ 1,
#     # if its more than a set number of days away, set to 0
#     as.numeric(difftime(ref, x, units = "days")) > max ~ 0,
#     TRUE ~ base ^ as.numeric(difftime(ref, x, units = "days"))
#   )
# }
# 
# add_game_weights = function(data, ref = NULL, base = 0.995, importance = T, filter = T) {
#   
#   if (is.null(ref)) {
#     ref = max(data$start_date)
#   }
#   
#   tmp =
#     data |>
#     mutate(weight = weights_from_dates(start_date, ref = ref, base = base))
#   
#   if (filter == T) {
#     
#     tmp = 
#       tmp |>
#       filter(weight > 0)
#   }
#   
#   if (importance == T) {
#     
#     tmp = 
#       tmp |>
#       mutate(weight = recipes::importance_weights(weight))
#   }
#   
#   tmp
#   
# }
# estimate_efficiency_cumulative = function(data, metric) {
#   
#   dates = 
#     data |>
#     mutate(season, 
#            week, 
#            .keep = 'none'
#     ) |>
#     distinct()
#   
#   map2(
#     .x = dates$season,
#     .y = dates$week,
#     ~ data |>
#       filter(season == .x,
#              week <= .y) |>
#       estimate_efficiency(metric = metric) |>
#       add_overall_efficiency() |>
#       mutate(season_week = paste(.x, .y, sep = "_")) |>
#       select(season_week, everything())
#   ) |>
#     list_rbind()
#   
# }
# estimate_efficiency_weighted = function(data, metric, dates, base = .995) {
#   
#   map(
#     dates,
#     ~ {
#       message(paste0("estimating week ", .x, "..."))
#       
#       data |>
#         filter(start_date <= .x) |>
#         add_game_weights(base =base) |>
#         estimate_efficiency(metric = metric, weights = T) |>
#         add_overall_efficiency() |>
#         mutate(start_date = .x)
#     }
#   ) |>
#     list_rbind()
# }
# estimate_efficiency_by_week = function(data, metric, season, base = .995, season_type = 'regular', ...) {
#   
#   calendar = 
#     data |>
#     find_season_dates(seasons = season, 
#                       season_types = season_type)
#   
#   dates = 
#     calendar |>
#     pull(start_date)
#   
#   estimates = 
#     data |>
#     estimate_efficiency_weighted(dates = dates,
#                                  metric = metric,
#                                  base = base,
#                                  ...)
#   
#   estimates |>
#     left_join(calendar) |>
#     select(-start_date) |>
#     select(season, season_type, week, season_week, everything())
#   
# }
# plot_team_efficiency_by_week = function(data, team) {
#   
#   data|>
#     mutate(week = as.numeric(stringr::str_sub(season_week, 6, 7))) |>
#     add_team_ranks(groups = c("season", "week", "type", "metric")) |> 
#     plot_team_efficiency(x = 'week', teams = team)+
#     facet_grid(type ~ season,
#                scales = "free_y")
#   
#   
# }
# 
# find_season_dates = function(data, seasons, season_types = c("regular", "postseason"), week_start = "Wednesday") {
#   
#   # create a data frame for the cfb season at the weekly level; 
#   # adds a week zero and creates individual weeks for the postseason
#   tmp = 
#     data |>
#     filter(season %in% seasons,
#            season_type %in% season_types) |>
#     mutate(week_date = lubridate::ceiling_date(start_date, unit = "week", week_start = week_start)) |>
#     group_by(season, season_type, week, week_date) |>
#     summarize(
#       start_date = max(week_date),
#       .groups = 'drop'
#     ) |>
#     arrange(start_date)
#   
# 
#   # fix issues where games overlap between weeks
#   tmp |>
#     mutate(flag = case_when(week_date == dplyr::lag(week_date, 1) ~ T,
#                             TRUE ~ F)) |>
#     filter(flag == F) |>
#     select(-flag, -week_date) |>
#     mutate(season_week = paste(season, row_number()-1, sep = "_")) |>
#     select(season_week, everything()) 
#   
# }
# 
# pbp_efficiency |> 
#   find_season_dates(seasons = 2022) |>
#   print(n = 50)
# 
# estimates = 
#     map(
#     c(2021),
#     ~ pbp_efficiency |>
#       estimate_efficiency_by_week(season = .x,
#                                   season_type = c('regular', 'postseason'),
#                                   metric = 'predicted_points_added')
#   ) |>
#   list_rbind()
# 
# estimates 
# 
# estimates = 
#   map(
#     c(2021, 2022, 2023),
#     ~ pbp_efficiency |>
#       estimate_efficiency_by_week(season = .x,
#                                   season_type = c('regular', 'postseason'),
#                                   metric = 'predicted_points_added')
#   ) |>
#   list_rbind()
# 
# estimates |>
#   mutate(week = as.numeric(stringr::str_sub(season_week, 6, 7))) |>
#   plot_team_efficiency_by_week(team = 'Cincinatti')
# 
# 
# estimate_effiency_by_week(season = 2023,
#                           season_type = c('regular', 'postseason'),
#                           metric = 'predicted_points_added')
# 
# estimated |> 
#   plot_team_efficiency_by_week(team = 'Alabama')
# 
# add_team_ranks(groups = c("season", "week", "type", "metric")) |> 
#   plot_team_efficiency(x = 'week', teams = 'TCU')+
#   facet_grid(type ~ season,
#              scales = "free_y")+
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
# 
# 
# 
# 
# 
# 
# 
# no_weights = 
#   pbp_efficiency |>
#   filter(play_situation != 'special',
#          season == 2022,
#          season_type == 'regular') |>
#   estimate_efficiency_cumulative(metric = 'predicted_points_added')
# 
# season_weeks = 
#   cfbd_calendar_tbl |> 
#   filter(season == 2022,
#          season_type == 'regular') |>
#   mutate(start_date = as.Date(last_game_start)) |>
#   select(season, 
#          season_type,
#          week,
#          start_date)
# 
# pbp_efficiency |>
#   left_join(
#     cfbd_game_info_tbl |>
#       mutate(start_date = as.Date(start_date)) |>
#       select(game_id,
#              start_date)
#   )
# 
# 
# 
# calendar = 
#   
#   pbp_efficiency |>
#   left_join(
#     cfbd_game_info_tbl |>
#       mutate(start_date = as.Date(start_date)) |>
#       select(game_id,
#              start_date)
#   ) |>
#   find_season_dates(seasons = 2023, 
#                     season_types = 'regular')
# 
# weights = 
#   pbp_efficiency |>
#   filter(season <= 2022,
#          play_situation != 'special',
#          season_type == 'regular') |>
#   left_join(
#     cfbd_game_info_tbl |>
#       mutate(start_date = as.Date(start_date)) |>
#       select(game_id,
#              start_date)
#   ) |>
#   estimate_efficiency_weighted(dates = season_weeks$start_date,
#                                metric = 'predicted_points_added') |>
#   left_join(
#     season_weeks |>
#       mutate(season_week = paste(season, week, sep = "_"))
#   ) |>
#   select(season_week, team, metric, type, estimate)
# 
# 
# plot_season_estimates = function(data, span = 0.15) {
#   
#   df = 
#     data |>
#     #separate_wider_delim(season_week, names = c("season", "week"), delim= "_") |>
#     mutate(across(c(season, week), as.numeric)) |>
#     mutate(team_label = case_when(week == max(week) ~ team,
#                                   week == min(week) ~ team))
#   
#   df |>
#     ggplot(aes(x=week,
#                color = team,
#                linetype = method,
#                label = team_label,
#                group = paste(team, method),
#                y=estimate))+
#     # geom_text(aes(label = rank),
#     #           size = 2,
#     #           vjust = -.5)+
#     geom_hline(yintercept = 0,
#                alpha = 0.25)+
#     geom_line(stat = 'smooth',
#               span = span)+
#     # geom_label(vjust = 0.5,
#     #            size = 2)+
#     scale_color_cfb()+
#     scale_linetype_manual(values = c("dotted", "solid"))+
#     xlab("Week")+
#     ylab("Estimated Team Strength")+
#     facet_wrap(season ~.)+
#     facet_wrap(paste(team, season)~.)+
#     theme_cfb()
# }
# 
# bind_rows(
#   no_weights |>
#     mutate(method = 'no_weights'),
#   weights |>
#     mutate(method = 'weights')
# ) |>
#   add_team_ranks(
#     groups = c("season_week", "metric", "type", "method")
#   )  |>
#   filter(type == 'overall') |>
#   # filter(team == 'Florida International') |>
#   inner_join(
#     tibble(team = c("Michigan", "Oregon", "Georgia", "Tennessee", "Ohio State", "LSU", "USC", "TCU", "Alabama"))
#   ) |>
#   plot_season_estimates()
# 
# estimates = 
#   weights |>
#   pivot_wider(names_from = c('type'),
#               values_from = c('estimate')) |>
#   separate_wider_delim(season_week, names = c("season", "week"), delim= "_") |>
#   mutate(across(c(season, week), as.numeric)) |>
#   select(-metric)
# 
# estimates |>
#   pivot_longer(
#     cols = c("overall", "offense", "defense")
#   )
# 
# 
# df = 
#   cfbd_game_info_tbl |>
#   filter(season == 2022,
#          season_type == 'regular') |>
#   select(season, game_id, week, start_date, neutral_site, home = home_team, away = away_team, home_points, away_points) |>
#   mutate(home_win = case_when(home_points > away_points ~ 'yes',
#                               TRUE ~ 'no'),
#          home_win = factor(home_win, levels = c("no", "yes"))) |>
#   left_join(
#     estimates |>
#       mutate(week = week - 1) |>
#       select(season, 
#              week,
#              home = team,
#              home_overall = overall
#       )
#   ) |>
#   left_join(
#     estimates |>
#       mutate(week = week - 1) |>
#       select(season, week,
#              away = team,
#              away_overall = overall)
#   ) |>
#   na.omit() |>
#   # mutate(across(ends_with("overall"), ~ .x * 100)) |>
#   mutate(home_diff_overall = home_overall - away_overall,
#          home_margin = home_points - away_points,
#          neutral_site = case_when(neutral_site == T ~ 1,
#                                   TRUE ~ 0)) 
# 
# logistic_reg() |>
#   fit(home_win ~ home_overall + away_overall + neutral_site,
#       data = df)
# 
# 
# df |>
#   ggplot(aes(x=home_diff_overall,
#              color = home_win,
#              label = paste(paste(home, away, sep = ' vs '),
#                            paste(home_points, away_points, sep = " - ")),
#              y=home_margin))+
#   geom_point()+
#   geom_text(size = 2,
#             vjust = -.5)
# # nest(data = -team) |>
# # sample_n(25) |>
# # unnest(data) |>
# # filter(team %in% c('Georgia', 'TCU', 'Michigan', 'Texas', 'Notre Dame', 'Florida State')) |>
# #inner_join(tibble(team = c("Florida State", "Texas A&M", "Marshall", "Notre Dame", "Auburn","TCU", "Texas", "LSU", "Georgia", "Tennessee"))) |>
# 
# 
# # 
# # no_weights = 
# #   pbp_efficiency |>
# #   left_join(
# #     cfbd_game_info_tbl |>
# #       mutate(start_date = as.Date(start_date)) |>
# #       select(game_id,
# #              start_date)
# #   ) |>
# #   filter(play_situation != 'special',
# #          season == 2023,
# #          season_type == 'regular') |>
# #   cumulative_data() |>
# #   nest(data = -c(season_week)) |>
# #   mutate(estimated = map(data, ~ estimate_efficiency(.x, metric = 'predicted_points_added')))
# # 
# # 
# # with_weights = 
# #   pbp_efficiency |>
# #   add_weights() 
# # filter(play_situation != 'special') |>
# #   filter(season == 2023,
# #          season_type == 'regular') |>
# #   nest(data = -c(week))
# # filter(play_situation != 'special') |>
# #   estimate_efficiency(metric = "predicted_points_added",
# #                       weights = T)
# # 
# # plot_weights_vs_none = function(data) {
# #   
# #   data |>
# #     add_overall_efficiency() |>
# #     pivot_wider(
# #       names_from = c("type"),
# #       values_from = c("estimate")
# #     ) |>
# #     pivot_wider(
# #       names_from = c("method"),
# #       values_from = c("overall", "offense", "defense")
# #     ) |>
# #     ggplot() +
# #     geom_point(aes(x=offense_no_weights,
# #                    y=defense_no_weights,
# #                    color = 'no_weights'),
# #                size = 0)+
# #     # geom_point(aes(x=offense_weights,
# #     #                y=defense_weights,
# #     #                color = 'weights'))+
# #     # ggrepel::geom_text_repel(
# #     #   aes(x=offense_no_weights,
# #     #       y=defense_no_weights,
# #     #       label = team,
# #     #       vjust = -1,
# #     #       color = 'no_weights'),
# #     #   size = 2,
# #     #   color = 'navy')+
# #     geom_text(aes(x=offense_weights,
# #                   y=defense_weights,
# #                   label = team,
# #                   vjust = -1,
# #                   color = 'weights'),
# #               size = 1.5)+
# #     geom_segment(aes(x = offense_no_weights, 
# #                      y = defense_no_weights,
# #                      xend = offense_weights, 
# #                      yend = defense_weights),
# #                  arrow = arrow(length = unit(0.1, "cm")),
# #                  color = "dodgerblue4")+
# #     coord_cartesian(xlim = c(-.4, .4),
# #                     ylim = c(-.4, .4))+
# #     geom_vline(xintercept = 0, linetype = 'dotted')+
# #     geom_hline(yintercept = 0, linetype = 'dotted')+
# #     xlab("Offensive Efficiency")+
# #     ylab("Defensive Efficiency")+
# #     scale_color_manual(values = c("grey", "navy"))+
# #     guides(color = 'none')
# # }
# # 
# # 
# # # estimate team strength for each individual week of 2023
# # no_weights = 
# #   pbp_weights |>
# #   filter(play_situation != 'special') |>
# #   filter(season == 2023,
# #          season_type == 'regular',
# #          week <=3) |>
# #   estimate_efficiency(metric = "predicted_points_added")
# # 
# # no_weights |>
# #   add_overall_efficiency() |>
# #   pivot_wider(names_from = c("type"), values_from = c("estimate")) |> 
# #   arrange(desc(overall))
# # 
# # no_weights = 
# #   pbp_weights |>
# #   inner_join(
# #     cfbd_game_info_tbl |>
# #       filter(season )
# #   )
# # filter(season ) |>
# #   filter(play_situation != 'special') |>
# #   estimate_efficiency(metric = "predicted_points_added")
# # 
# # no_weights |>
# #   mutate(method = 'no_weights') |>
# #   bind_rows(
# #     with_weights |>
# #       mutate(method = 'weights')
# #   ) |>
# #   plot_weights_vs_none() 
# # # add_team_ranks(groups = c("season", "type", "metric", "method")) |>
# # pivot_wider(
# #   names_from = c("type"),
# #   values_from = c("estimate")
# # ) |>
# #   pivot_wider(
# #     names_from = c("method"),
# #     values_from = c("overall", "offense", "defense")
# #   ) |>
# #   ggplot() +
# #   geom_point(aes(x=offense_no_weights,
# #                  y=defense_no_weights,
# #                  color = 'no_weights'),
# #              size = 0)+
# #   # geom_point(aes(x=offense_weights,
# #   #                y=defense_weights,
# #   #                color = 'weights'))+
# #   # ggrepel::geom_text_repel(
# #   #   aes(x=offense_no_weights,
# #   #       y=defense_no_weights,
# #   #       label = team,
# #   #       vjust = -1,
# #   #       color = 'no_weights'),
# #   #   size = 2,
# #   #   color = 'navy')+
# #   geom_text(aes(x=offense_weights,
# #                 y=defense_weights,
# #                 label = team,
# #                 vjust = -1,
# #                 color = 'weights'),
# #             size = 1.5)+
# #   geom_segment(aes(x = offense_no_weights, 
# #                    y = defense_no_weights,
# #                    xend = offense_weights, 
# #                    yend = defense_weights),
# #                arrow = arrow(length = unit(0.1, "cm")),
# #                color = "dodgerblue4")+
# #   coord_cartesian(xlim = c(-.4, .4),
# #                   ylim = c(-.4, .4))+
# #   geom_vline(xintercept = 0, linetype = 'dotted')+
# #   geom_hline(yintercept = 0, linetype = 'dotted')+
# #   xlab("Offensive Efficiency")+
# #   ylab("Defensive Efficiency")+
# #   scale_color_manual(values = c("grey", "navy"))+
# #   guides(color = 'none')
# # 
# # 
# # 
# # 
# # geom_text()
# # geom_point()+
# #   pivot_wide r(
# #     names_from = c("method"),
# #     values_from = c("estimate")
# #   ) |>
# #   ggplot(aes(x=no_weights,
# #              y=weights,
# #              label = team))+
# #   geom_point()+
# #   geom_label(alpha = 0.5)+
# #   facet_wrap(type ~.,
# #              ncol = 1)_
# # 
# # with_weights
# # estimate_efficiency_weights = 
# #   
# #   build_efficiency_wflow_weights = function(data, metric = "expected_points_added", model = efficiency_model(), ...) {
# #     
# #     rec = data |>
# #       build_efficiency_recipe()
# #     
# #     wflow |>
# #       workflow() |>
# #       add_case_weights(
# #         weight
# #       ) |>
# #       add_recipe(
# #         rec
# #       ) |>
# #       add_model(
# #         model
# #       )
# #   }
# # 
# # pbp_weights |>
# #   build_efficiency_wflow(metric = "predicted_points_added")
# # 
# # foo = 
# #   pbp_weights |>
# #   filter(weight > 0) |>
# #   
# #   
# #   foo$teams |>
# #   filter(team == 'Texas A&M') |>
# #   select(game_id, season, team)
# # 
# # foo$teams |>
# #   filter(team == 'Texas A&M') |>
# #   left_join(
# #     foo$opponents
# #   )
# # 
# # 
# # pluck("opponents")
# # filter(game_id == 63070) |>
# #   select(opponent)
# # filter(opponent == 'Texas A&M')
# # select(game_id, season, week, starts_with("team"))
# # pivot_home_away(var = 'away', replace = 'team')
# # 
# # 
# # pivot_games_to_teams()  |>
# #   filter(game_id == 63070)
# # filter(team == 'Texas A&M') |>
# #   filter(is_home_team == T) |>
# #   filter(game_id == 63177) |>
# #   select(game_id, team)
# # 
# # cfbd_game_info_tbl |>
# #   filter(game_id == 63177) |>
# #   select(home_team, away_team)
# # select(game_id, season, week, team, opponent)
# # select(season, game_id, team, is_home_team)
# # select(game_id, season, team, opponent)
# # 
# # 
# # filter(is_home_team ==)
# # pivot_home_away() |>
# #   group_by(team, opponent) |> 
# #   count()
# # filter(team == 'Texas A&M')
# # 
# # 
# # cfbd_game_info_tbl |>
# #   select(game_id, starts_with("home")) |>
# #   rename_with(cols = starts_with("home_"), ~ gsub("home_", "", .x)) |>
# #   mutate(is_home_team = T)
# # 
# # cfbd_game_info_tbl |>
# #   select(game_id, starts_with("away")) |>
# #   rename_with(cols = starts_with("home_"), ~ gsub("away_", "", .x)) |>
# #   mutate(is_home_team = F)
# # 
# # 
# # 
# # 
# # pivot_team()
# # filter(team == 'Texas A&M', season > 2007)
