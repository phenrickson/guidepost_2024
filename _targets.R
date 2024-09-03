# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)
library(gitcreds)

# authenticate
googleAuthR::gar_auth_service(
  json_file = Sys.getenv("GCS_AUTH_FILE"),
  scope = c(
    "https://www.googleapis.com/auth/devstorage.full_control",
    "https://www.googleapis.com/auth/cloud-platform"
  )
)

# set default bucket
suppressMessages({
  googleCloudStorageR::gcs_global_bucket(bucket = "cfb_models")
})

# Set target options:
tar_option_set(
  packages = c(
    "cfbfastR",
    "dplyr",
    "tidyr",
    "purrr",
    "stringr",
    "tidymodels",
    "glmnet"
  ),
  format = "qs",
  memory = "transient",
  resources =
    tar_resources(
      gcp = tar_resources_gcp(
        bucket = "cfb_models",
        predefined_acl = "bucketLevel",
        prefix = "data"
      )
    ),
  # controller =
  #   crew_controller_local(workers = 7),
  repository = "gcp"
)

# Run the R scripts in the R/ folder with your custom functions:
suppressMessages({
  tar_source("R")
})
# tar_source("other_functions.R") # Source other scripts as needed.

# running over seasons
seasons <- 2000:2023

# Replace the target list below with your own:
list(
  ### cfbd data
  # load all games
  tar_target(
    cfbd_games_tbl,
    map_df(
      1869:cfbfastR:::most_recent_cfb_season(),
      ~ cfbd_game_info(
        year = .x,
        season_type = "both",
        division = "fbs"
      ) |>
        as_tibble()
    )
  ),
  # calendars
  tar_target(
    cfbd_calendar_tbl,
    map_df(
      seasons,
      ~ cfbd_calendar(year = .x)
    ) |>
      as_tibble()
  ),
  # conferences
  tar_target(
    cfbd_conferences_tbl,
    cfbd_conferences() |>
      as_tibble()
  ),
  # fbs team info
  tar_target(
    cfbd_team_info_tbl,
    map_df(
      seasons,
      ~ cfbd_team_info(
        only_fbs = T,
        year = .x
      ) |>
        as_tibble() |>
        mutate(season = .x)
    ) |>
      select(season, everything())
  ),
  # talent
  tar_target(
    cfbd_team_talent_tbl,
    map_df(
      seasons,
      ~ cfbd_team_talent(year = .x)
    ) |>
      as_tibble()
  ),
  # team recruiting,
  tar_target(
    cfbd_recruiting_team,
    map_df(
      seasons,
      ~ cfbd_recruiting_team(year = .x)
    ) |>
      as_tibble()
  ),
  # games for selected seasons
  tar_target(
    cfbd_game_info_tbl,
    map_df(
      seasons,
      ~ cfbd_game_info(
        year = .x,
        season_type = "both"
      )
    ) |>
      as_tibble()
  ),
  # betting lines
  tar_target(
    cfbd_betting_lines_tbl,
    {
      tmp <- expand_grid(
        season = seasons,
        type = c("regular", "postseason")
      )
      
      map2_df(
        .x = tmp$season,
        .y = tmp$type,
        ~ cfbd_betting_lines(
          year = .x,
          season_type = .y
        )
      )
    }
  ),
  # rankings
  tar_target(
    cfbd_game_rankings_tbl,
    {
      tmp <- expand_grid(
        season = seasons,
        type = c("regular", "postseason")
      )
      
      map2_df(
        .x = tmp$season,
        .y = tmp$type,
        ~ cfbd_rankings(
          year = .x,
          season_type = .y
        )
      )
    }
  ),
  # draft picks
  tar_target(
    cfbd_draft_picks,
    map_df(
      seasons,
      ~ cfbd_draft_picks(year = .x)
    ) |>
      as_tibble()
  ),
  # play types
  tar_target(
    cfbd_play_types_tbl,
    cfbd_play_types() |>
      as_tibble()
  ),
  # coaches
  tar_target(
    cfbd_coaches_tbl,
    map_df(
      seasons,
      ~ cfbd_coaches(year = .x) |>
        add_season(year = .x)
    )
  ),
  # rosters
  tar_target(
    cfbd_team_roster_tbl,
    map_df(
      seasons,
      ~ cfbd_team_roster(year = .x) |>
        add_season(year = .x)
    )
  ),
  # recruiting_player
  tar_target(
    cfbd_recruiting_player_tbl,
    map_df(
      seasons,
      ~ cfbd_recruiting_player(year = .x) |>
        add_season(year = .x)
    )
  ),
  # recruiting position
  tar_target(
    cfbd_recruiting_position_tbl,
    map_df(
      seasons,
      ~ cfbd_recruiting_position(
        start_year = .x,
        end_year = .x
      ) |>
        add_season(year = .x)
    )
  ),
  # player usage
  tar_target(
    cfbd_player_usage_tbl,
    map_df(
      seasons[seasons > 2012],
      ~ cfbd_player_usage(year = .x) |>
        as_tibble()
    )
  ),
  # player returning
  tar_target(
    cfbd_player_returning_tbl,
    map_df(
      seasons,
      ~ cfbd_player_returning(year = .x) |>
        as_tibble()
    )
  ),
  # drives
  tar_target(
    cfbd_drives_tbl,
    map_df(
      seasons,
      ~ cfbd_drives(
        year = .x,
        season_type = "both"
      ) |>
        add_season(year = .x)
    )
  ),
  ### now get espn data
  # calendar
  tar_target(
    espn_cfb_calendar_tbl,
    map_df(
      seasons,
      ~ espn_cfb_calendar(year = .x) |>
        as_tibble()
    )
  ),
  # schedule
  tar_target(
    espn_cfb_schedule_tbl,
    map_df(
      seasons,
      ~ espn_cfb_schedule(year = .x) |>
        as_tibble()
    )
  ),
  # espn games
  tar_target(
    espn_cfb_game_ids,
    espn_cfb_schedule_tbl |>
      # seasons with pbp data
      filter(season > 2002) |>
      filter(play_by_play_available == T) |>
      distinct(game_id) |>
      pull()
  ),
  # espn fpi
  tar_target(
    espn_ratings_fpi_tbl,
    map_df(
      seasons[seasons > 2004],
      ~ espn_ratings_fpi(year = .x) |>
        as_tibble()
    )
  ),
  # get historical conferences and divisions
  # divisions
  tar_target(
    team_divisions,
    cfbd_game_info_tbl |>
      select(season, home_team, away_team, home_division, away_division) |>
      find_team_divisions()
  ),
  # conferences
  tar_target(
    team_conferences,
    cfbd_team_info_tbl |>
      select(
        season,
        school,
        conference,
        division
      ) |>
      distinct()
  ),
  # dynamic branch over seasons, weeks, and season type to get play by play
  tar_target(
    cfbd_season_week_games,
    cfbd_game_info_tbl |>
      select(season, week, season_type) |>
      distinct() |>
      filter(season_type %in% c("regular", "postseason")) |>
      group_by(season, week, season_type) |>
      tar_group(),
    iteration = "group"
  ),
  # get cfbd plays for each branch
  tar_target(
    cfbd_plays_tbl,
    get_cfbd_plays(cfbd_season_week_games),
    pattern = map(cfbd_season_week_games),
    error = "null"
  ),
  # get cleaned cfbd pbp (cfbfastR) for each branch
  tar_target(
    cfbd_pbp_data_tbl,
    get_cfbd_pbp_data(cfbd_season_week_games),
    pattern = map(cfbd_season_week_games),
    error = "null"
  ),
  # filter to only relevant
  tar_target(
    filtered_pbp,
    cfbd_pbp_data_tbl |>
      # filter to only games with both fbs divisions
      inner_join(
        cfbd_game_info_tbl |>
          filter(home_division == "fbs" | away_division == "fbs")
      ) |>
      # filter to games after 2005
      filter(season > 2005)
  ),
  # prepare pbp data using custom functions
  tar_target(
    prepared_pbp,
    filtered_pbp |>
      prepare_pbp() |>
      add_score_events()
  ),
  # prepare games for use in elo functions
  tar_target(
    prepared_games,
    cfbd_games_tbl |>
      prepare_games()
  ),
  # elo parameters
  tar_target(
    elo_params,
    expand.grid(
      reversion = c(0, 0.1, 0.2),
      k = c(25, 35, 45),
      v = 400,
      home_field_advantage = c(25, 75, 100)
    )
  ),
  tar_target(
    elo_tuning_results,
    prepared_games |>
      tune_elo_ratings(params = elo_params),
    pattern = map(elo_params),
    iteration = "vector",
    cue = tar_cue(mode = "never")
  ),
  tar_target(
    elo_metrics,
    elo_tuning_results |>
      select(game_outcomes, settings) |>
      # prioritize games since 2000
      mutate(game_outcomes = map(game_outcomes, ~ .x |> filter(season >= 2000))) |>
      assess_elo_ratings()
  ),
  tar_target(
    elo_best_params,
    elo_metrics |>
      select(overall) |>
      unnest() |>
      select_elo_params(),
    packages = c("desirability2")
  ),
  tar_target(
    elo_games,
    prepared_games |>
      tune_elo_ratings(params = elo_best_params)
  ),
  # expected points modeling
  tar_target(
    class_metrics,
    metric_set(
      yardstick::roc_auc,
      yardstick::mn_log_loss
    )
  ),
  # create split
  tar_target(
    split_pbp,
    prepared_pbp |>
      filter(season >= 2007 & season <= max(seasons)) |>
      split_seasons(
        end_train_year = 2017,
        valid_years = 2
      )
  ),
  # create recipe for pbp mmodel
  tar_target(
    pbp_recipe,
    split_pbp |>
      training() |>
      build_pbp_recipe()
  ),
  # create model specification for pbp model
  tar_target(
    pbp_model_spec,
    multinom_reg(
      mode = "classification",
      engine = "glmnet",
      penalty = 0,
      mixture = NULL
    )
  ),
  # create workflow for pbp
  tar_target(
    pbp_wflow,
    workflow() |>
      add_recipe(pbp_recipe) |>
      add_model(pbp_model_spec)
  ),
  # fit to training set; estimate on valid set
  tar_target(
    pbp_last_fit,
    pbp_wflow |>
      last_fit(
        split =
          split_pbp |>
          validation_set() |>
          pluck("splits", 1),
        metrics = class_metrics
      )
  ),
  # extract metrics
  tar_target(
    pbp_valid_metrics,
    pbp_last_fit |>
      collect_metrics()
  ),
  # extract predictions
  tar_target(
    pbp_valid_preds,
    pbp_last_fit |>
      collect_predictions() |>
      left_join(
        split_pbp |>
          validation() |>
          mutate(.row = row_number())
      )
  ),
  # predict test set
  tar_target(
    pbp_test_preds,
    pbp_last_fit |>
      extract_workflow() |>
      augment(split_pbp |> testing())
  ),
  # final fit
  tar_target(
    pbp_final_fit,
    pbp_last_fit |>
      extract_workflow() |>
      fit(
        split_pbp$data
      )
  ),
  # predict all plays with final model
  tar_target(
    pbp_all_preds,
    pbp_last_fit |>
      extract_workflow() |>
      augment(split_pbp$data)
  ),
  # calculate expected points
  tar_target(
    pbp_predicted,
    pbp_all_preds |>
      calculate_expected_points() |>
      calculate_points_added()
  ),
  # prepare for efficiency
  tar_target(
    pbp_efficiency,
    pbp_predicted |>
      prepare_efficiency(games = cfbd_game_info_tbl)
  ),
  # now add in efficiency estimates
  # overall
  tar_target(
    raw_efficiency_overall,
    pbp_efficiency |>
      calculate_efficiency(groups = c("season", "type", "team"))
  ),
  tar_target(
    adjusted_efficiency_overall_epa,
    pbp_efficiency |>
      estimate_efficiency_overall(metric = 'expected_points_added')
  ),
  tar_target(
    adjusted_efficiency_overall_ppa,
    pbp_efficiency |>
      estimate_efficiency_overall(metric = 'predicted_points_added')
  ),
  # pass/rush
  tar_target(
    adjusted_efficiency_category_epa,
    pbp_efficiency |>
      estimate_efficiency_category(metric = 'expected_points_added')
  ),
  tar_target(
    adjusted_efficiency_category_ppa,
    pbp_efficiency |>
      estimate_efficiency_category(metric = 'predicted_points_added')
  )
  # # quarto
  # tar_quarto(
  #   reports,
  #   quiet = F
  # )
)
