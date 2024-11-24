set.seed(42)
ins_modelling <- ins |> dplyr::mutate(
    ClaimAmount_noNA = dplyr::case_when(
        is.na(ClaimAmount) ~ 0,
        TRUE ~ ClaimAmount
    ),
    ClaimAmount_grouped = cut(
        ClaimAmount_noNA,
        breaks = c(-Inf, 0, 10^(1.8), 10^(1.975), 10^(2.71), 10^(2.83), 10^3, 10^(3.15), Inf),
    ),
    ClaimAmount_binary = cut(
        ClaimAmount_noNA,
        breaks = c(-Inf, 0, Inf),
    ),
    claims_per_exposure = ClaimAmount_noNA / Exposure,
    # random train-test split
    tvt = sample(c("train", "validation", "test"), size = nrow(ins), replace = TRUE, prob = c(0.7, 0.2, .1))
)
plot(density(log10(ins_modelling$ClaimAmount_noNA[ins_modelling$ClaimAmount_noNA > 0])))
abline(v = log10(c(10^3, 10^(3.15))), col = "red")
abline(v = log10(c(10^(1.8), 10^(1.975))), col = "blue")
abline(v = log10(c(10^(2.71), 10^(2.83))), col = "green")

ins_modelling_part <- ins_modelling |> dplyr::sample_n(10000)
ins_modelling_part <- ins_modelling
pacman::p_install("mlr3", force = FALSE)
ivs <- c("Area", "VehPower", "VehAge", "DrivAge", "BonusMalus", "VehBrand", "VehGas", "Density", "Region")

# any_claim_m1 <- stats::glm(ClaimAmount_binary ~ Area + log10(VehPower) + VehAge + DrivAge + (BonusMalus) + VehBrand + VehGas + log10(Density) + Region, data = ins_modelling |> dplyr::filter(tvt == "train"), family = stats::binomial())

tasklist_binary <- list(
    "binary_v0" = mlr3::as_task_classif(
        ins_modelling_part[, c("ClaimAmount_binary", ivs)],
        target = "ClaimAmount_binary",
        positive = levels(ins_modelling[["ClaimAmount_binary"]])[2]
    ),
    "binary_v1" = mlr3::as_task_classif(
        ins_modelling_part[, c("ClaimAmount_binary", ivs)] |>
            dplyr::mutate(
                VehPower = log10(VehPower),
                BonusMalus = log10(BonusMalus),
                Density = log10(Density)
            ),
        target = "ClaimAmount_binary",
        positive = levels(ins_modelling[["ClaimAmount_binary"]])[2]
    )
)

pacman::p_load("mlr3learners")
pacman::p_load("mlr3tuning")


learners <- list(
    mlr3::lrn("classif.ranger", predict_type = "prob")
)


# https://stackoverflow.com/questions/68824193/is-it-possible-to-obtain-predictions-on-the-training-data-from-resample-results
learners <- lapply(learners, function(x) {
    x$predict_sets <- c("test", "train")
    return(x)
})

resampled_tasks <- lapply(
    tasklist_binary,
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
        tmp <- as.data.table(preds_df |> dplyr::filter(tvt == "validation"))
        truth_releveled <- tmp[["truth"]]

        level_pos <- levels(truth_releveled)[1]
        level_neg <- levels(truth_releveled)[2]

        tmp_roc <- pROC::roc(
            response = truth_releveled,
            predictor = tmp[[paste0("prob.", level_pos)]],
            levels = rev(levels(tmp$truth)),
            direction = "<"
        )
        # pROC::ggroc(tmp_roc, legacy.axes = TRUE) + ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") + ggplot2::theme_minimal()
        topleft_index_th <- pROC::coords(tmp_roc, "best", ret = "threshold", best.method = "closest.topleft")[[1]]

        preds_df[["response_topleft"]] <- factor(
            ifelse(
                preds_df[[paste0("prob.", level_pos)]] > topleft_index_th,
                level_pos,
                level_neg
            ),
            levels = levels(preds_df$truth)
        )
        return(
            list(
                rr = rr,
                preds = preds_df |> tibble::as_tibble()
            )
        )
    }
)

pacman::p_install("caret", force = FALSE)
performances <- lapply(resampled_tasks, function(x) {
    x$preds |>
        dplyr::group_by(
            tvt
        ) |>
        dplyr::group_map(
            ~ (function(data, grouping) {
                tmp <- list(caret::confusionMatrix(
                    data$truth,
                    data$response_topleft
                ))
                names(tmp) <- grouping[[1]]
                return(tmp)
            })(.x, .y)
        ) |>
        unlist(recursive = FALSE)
})

dir.create("res/model_RF")
sink("res/model_RF/01_performances_binary.txt")
print(performances[["binary_v0"]][["test"]])
print(performances[["binary_v1"]][["test"]])
sink()
