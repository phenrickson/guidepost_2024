# split data into training, validation, test sets based on seasons
split_seasons = function(data, end_train_year, valid_years) {

    tmp =
        data |>
        mutate(.row_number = row_number())

    train_id =
        tmp |>
        filter(season <= end_train_year) |>
        pull(.row_number)

    val_id =
        tmp |>
        dplyr::filter(season > end_train_year,
                      season <= end_train_year + valid_years) |>
        pull(.row_number)

    test_id =
        tmp |>
        filter(season > end_train_year + valid_years) |>
        pull(.row_number)

    res =
        list(
            data = data,
            train_id = train_id,
            val_id = val_id,
            test_id = test_id
        )

    class(res) <- c("initial_validation_split", "three_way_split")

    res
}

# get split by type
split_plan = function(split) {

    train =
        split |>
        training() |>
        mutate(type = 'training')

    valid =
        split |>
        validation() |>
        mutate(type = 'validation')

    testing =
        split |>
        testing() |>
        mutate(type = 'testing')

    bind_rows(train,
              valid,
              testing) |>
        mutate(type = factor(type, levels = c("testing", "validation", "training")))
}

# plot split plan
plot_split_plan = function(split) {


    dat =
        split_plan(split)

    dat |>
        group_by(type, season) |>
        count() |>
        ggplot(aes(x=season,
                   y=n,
                   fill = type))+
        geom_col()+
        scale_fill_viridis_d()+
        scale_y_continuous(labels = scales::label_comma())

}

# function to build a recipe and set outcome
build_recipe = function(data,
                        outcome,
                        ids = NULL,
                        predictors = NULL,
                        ...) {

    recipe(x = data) |>
        # set ids
        update_role(
            any_of(ids),
            new_role = "id"
        ) |>
        # set predictors
        update_role(
            any_of(predictors),
            new_role = "predictor"
        ) |>
        # set outcome
        update_role(
            {{ outcome }},
            new_role = "outcome"
        ) |>
        # set anything else as extras
        update_role(
            -has_role("predictor"),
            -has_role("outcome"),
            -has_role("id"),
            new_role = "extras"
        ) |>
        # drop these
        step_rm(has_role("extras"))
}

# ids to be used in pbp
pbp_ids = function() {

    c(
        "season",
        "play_id",
        "game_id",
        "offense",
        "defense",
        "home",
        "away",
        "offense_score",
        "defense_score",
        "drive_id",
        "drive_number",
        "play_number",
        "half",
        "period",
        "wallclock",
        "clock_minutes",
        "clock_seconds",
        "play_type",
        "play_text",
        "drive_is_home_offense",
        "yard_line"
    )
}

# predictors in pbp
pbp_predictors = function() {

    c(
        "period",
        "seconds_in_half",
        "yards_to_goal",
        "down",
        "distance"
    )
}

# steps to be used in pbp recipe
add_pbp_steps = function(recipe) {

    recipe |>
        step_mutate(down_to_goal = case_when(yard_line == distance ~ 1, TRUE ~ 0),
                    down = factor(down),
                    period = factor(period)) |>
        # impute missingness in distance; shouldnt be much
        step_impute_linear(
            distance,
            impute_with = imp_vars(down)
        ) |>
        step_log(distance, offset = 1) |>
        step_novel(all_nominal_predictors()) |>
        step_dummy(all_nominal_predictors()) |>
        step_interact(terms = ~ distance:(starts_with("down_"))) |>
        step_interact(terms = ~ yards_to_goal:(starts_with("down"))) |>
        step_interact(terms = ~ yards_to_goal*seconds_in_half)
}

# put pieces together into one pbp function
build_pbp_recipe = function(data,
                            outcome = "next_score_event_offense",
                            predictors = pbp_predictors(),
                            ids = pbp_ids()) {

    data |>
        build_recipe(
            outcome = "next_score_event_offense",
            predictors = pbp_predictors(),
            ids = pbp_ids()
        ) |>
        add_pbp_steps() |>
        check_missing(all_predictors()) |>
        step_zv(all_numeric_predictors()) |>
        step_normalize(all_numeric_predictors())
}

# summarize probabilities
summarize_pbp_effect = function(estimates,
                                vars) {

    estimates |>
        group_by(across(any_of(c(vars)))) |>
        summarize(
            across(
                c(.pred_TD,
                  .pred_FG,
                  .pred_Safety,
                  .pred_No_Score,
                  .pred_Opp_Safety,
                  .pred_Opp_FG,
                  .pred_Opp_TD
                ),
                ~ mean(.x)
            )
        )
}

# helper function for predicting pbp
estimate_pbp_effect = function(data,
                               fit) {

    fit |>
        augment(
            data
        )
}

# helper function for pivoting play by play
pivot_pbp = function(data) {

    data |>
        pivot_longer(
            cols = starts_with(".pred"),
            names_to = c("class"),
            names_prefix = c(".pred_"),
            values_to = c("prob")
        )

}

# helper function to set levels
factor_class = function(var) {

    factor({{var}},
           levels = c("TD",
                      "FG",
                      "Safety",
                      "No_Score",
                      "Opp_Safety",
                      "Opp_FG",
                      "Opp_TD"))
}

# function to plot calibration
plot_pbp_calibration = function(preds,
                                bin = 0.025) {

    preds |>
        pivot_pbp() |>
        group_by(class, prob_bin = round_any(prob, bin)) |>
        mutate(number_probs = n()) |>
        group_by(next_score_event_offense, class, prob_bin, number_probs) |>
        count() |>
        filter(next_score_event_offense == class) |>
        mutate(prop = n / number_probs) |>
        ggplot(aes(x=prob_bin,
                   size = number_probs,
                   y = prop))+
        geom_point()+
        geom_abline(slope = 1,
                    intercept = 0,
                    linetype = 'dashed')+
        xlab("Predicted Probability of Score Event")+
        ylab("Observed Probability of Score Event")+
        facet_wrap(next_score_event_offense ~.)+
        guides(size = 'none')

}
