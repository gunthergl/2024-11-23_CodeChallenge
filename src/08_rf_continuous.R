set.seed(42)
ins_modelling <- ins |> dplyr::mutate(
    ClaimAmount_noNA = dplyr::case_when(
        is.na(ClaimAmount) ~ 0,
        TRUE ~ ClaimAmount
    ),
    claims_per_exposure = ClaimAmount_noNA / Exposure,
    log10Plus1_claims_per_exposure = log10((ClaimAmount_noNA / Exposure) + 1),
    # random train-test split
    tvt = sample(c("train", "validation", "test"), size = nrow(ins), replace = TRUE, prob = c(0.7, 0.2, .1))
)

ins_modelling_part <- ins_modelling |> dplyr::sample_n(10000)
# ins_modelling_part <- ins_modelling
pacman::p_install("mlr3", force = FALSE)
ivs <- c("Area", "VehPower", "VehAge", "DrivAge", "BonusMalus", "VehBrand", "VehGas", "Density", "Region")

tasklist_continuous <- list(
    "continuous_v0" = mlr3::as_task_regr(
        ins_modelling_part[, c("claims_per_exposure", ivs)],
        target = "claims_per_exposure",
    ),
    "continuous_v1" = mlr3::as_task_regr(
        ins_modelling_part[, c("log10Plus1_claims_per_exposure", ivs)] |>
            dplyr::mutate(
                VehPower = log10(VehPower),
                BonusMalus = log10(BonusMalus),
                Density = log10(Density)
            ),
        target = "log10Plus1_claims_per_exposure",
    )
)
pacman::p_install("glmnet", force = FALSE)
pacman::p_load("mlr3learners")
pacman::p_load("mlr3tuning")


learners <- list(
    mlr3::lrn("regr.ranger", predict_type = "response")
    # # logistic regression
    # mlr3::lrn("classif.log_reg", predict_type = "prob")
)


# https://stackoverflow.com/questions/68824193/is-it-possible-to-obtain-predictions-on-the-training-data-from-resample-results
learners <- lapply(learners, function(x) {
    x$predict_sets <- c("test", "train")
    return(x)
})

resampled_tasks <- lapply(
    tasklist_continuous,
    function(task_x) {
        data_with_rowids <- task_x$data()
        data_with_rowids[["row_ids"]] <- 1:nrow(data_with_rowids)
        data_with_rowids[["tvt"]] <- ins_modelling_part$tvt
        resampling_train_validation <- mlr3::rsmp("custom")
        resampling_train_validation$instantiate(
            task_x,
            train = list(which(data_with_rowids[["tvt"]] == "train")),
            test = list(which(data_with_rowids[["tvt"]] %in% c("validation", "test")))
        )

        set.seed(327032)
        rr <- mlr3::resample(
            task = task_x,
            learner = learners[[1]],
            resampling = resampling_train_validation,
            # resampling = mlr3::rsmp("cv", folds = 3),
            # resampling = mlr3::rsmp("loo"),
            store_models = TRUE
        )
        preds_train <- rr$predictions("train")
        preds_train_df <- data.table::rbindlist(lapply(preds_train, as.data.table), idcol = "fold")
        preds_valtest <- rr$predictions("test")
        preds_valtest_df <- data.table::rbindlist(lapply(preds_valtest, as.data.table), idcol = "fold")
        preds_df <- dplyr::full_join(
            rbind(
                preds_valtest_df,
                preds_train_df
            ), data_with_rowids,
            by = "row_ids"
        )
        return(
            list(
                rr = rr,
                preds = preds_df |> tibble::as_tibble()
            )
        )
    }
)

# Measure the performance with caret:
calc_perf <- function(preds) {
    preds_df <- preds |> dplyr::mutate(
        residual = truth - response
    )
    perfs <- c(
        "RMSE" = caret::RMSE(preds_df$truth, preds_df$response),
        "MAE" = caret::MAE(preds_df$truth, preds_df$response),
        "Rsquared" = caret::R2(preds_df$truth, preds_df$response)
    )
    return(
        list(
            preds_df = preds_df,
            perfs = perfs
        )
    )
}
preds_perfs <- list(
    "continuous_v0" = calc_perf(resampled_tasks[[1]]$preds |> dplyr::filter(tvt %in% c("validation", "test"))),
    "continuous_v1" = calc_perf(resampled_tasks[[2]]$preds |> dplyr::filter(tvt %in% c("validation", "test")))
)

pacman::p_load("ggplot2")
pdf(file.path("res/07_modelling_continuous.pdf"))
print(
    ggplot(preds_perfs[["continuous_v0"]][["preds_df"]], aes(x = response, y = residual)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        ggpubr::theme_pubr()
)
print(
    ggplot(preds_perfs[["continuous_v1"]][["preds_df"]], aes(x = response, y = residual)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        ggpubr::theme_pubr()
)
dev.off()
