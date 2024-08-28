# test
targets::tar_load_globals()

library(tidymodels)

# data
tar_load(prepared_pbp)

# create split
split_pbp = 
    prepared_pbp |>
    filter(season >= 2013) |>
    split_seasons(end_train_year = 2017,
                  valid_years = 2)

# show plan
split_pbp |>
    plot_split_plan()

# recipe
pbp_recipe = 
    split_pbp |>
    training() |>
    build_pbp_recipe() |>
    step_zv(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors())

# model
library(glmnet)
multinom_mod =
    multinom_reg(
        mode = "classification",
        engine = "glmnet",
        penalty = 0,
        mixture = NULL
    )

# create workflow
pbp_wflow = 
    workflow() |>
    add_recipe(pbp_recipe) |>
    add_model(multinom_mod)

# fit
pbp_fit = 
    pbp_wflow |>
    fit(
        split_pbp |>
            training()
    )

# set metrics
class_metrics = 
    metric_set(yardstick::roc_auc,
               yardstick::mn_log_loss)

# # predict validation set
# valid_preds = 
#     pbp_fit |>
#     augment(
#         split_pbp |>
#             validation()
#     )
# 
# # assess
# valid_preds |>
#     class_metrics(
#         truth = next_score_event_offense,
#         estimate = .pred_class,
#         .pred_TD:.pred_Opp_TD)

pbp_last_fit = 
    pbp_wflow |>
    last_fit(split = split_pbp |> 
                 validation_set() |> 
                 pluck("splits", 1),
             metrics = class_metrics)

pbp_last_fit |>
    collect_predictions() |>
    plot_pbp_calibration()

test_preds = 
    pbp_last_fit |>
    extract_workflow() |>
    augment(
        split_pbp |>
            testing()
    )

# plot coefficients
pbp_fit |>
    tidy() |>
    filter(term != "(Intercept)") |>
    ggplot(aes(x=estimate,
               color = class,
               y=tidytext::reorder_within(term, estimate, class)))+
    geom_point()+
    facet_wrap(class~.,
               ncol = 4)+
    tidytext::scale_y_reordered()

samp = 
    split_pbp |>
    training() |>
    slice_sample(n = 10000)


v = expand.grid(yards_to_goal = seq(0, 99, 3), 
                down = c(1, 2, 3, 4))

df = 
    map(
        seq(0, 99, 2),
        ~ { 
            samp |>
                mutate(yards_to_goal := .x) |>
                nest(data = -yards_to_goal)
        }
    ) |>
    list_rbind() |>
    unnest(data)

df2 = 
    map(
        c(1,2,3,4),
        ~ {
            df |>
                mutate(down := .x) |>
                nest(data = -c(yards_to_goal, down))
        }
    ) |>
    list_rbind()

est = 
    df2 |>
    unnest(data) |>
    estimate_pbp_effect(fit = pbp_fit)

est |>
    summarize_pbp_effect(vars = c("yards_to_goal", "down"))

# plot probabilities
est |>
    summarize_pbp_effect(vars = c("yards_to_goal", "down")) |>
    pivot_pbp() |>
    mutate(down = factor(down),
           class = factor_class(class)) |>
    ggplot(aes(x=yards_to_goal,
               color = down,
               y=prob))+
    geom_line()+
    scale_color_viridis_d()+
    facet_wrap(class ~., ncol = 4)+
    coord_cartesian(ylim = c(0, 1))+
    ylab("Pr(Outcome)")+
    xlab("Yards to Goal")


