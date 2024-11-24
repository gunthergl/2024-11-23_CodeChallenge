set.seed(42)
ins_modelling <- ins |> dplyr::mutate(
    ClaimAmount_noNA = dplyr::case_when(
        is.na(ClaimAmount) ~ 0,
        TRUE ~ ClaimAmount
    ),
    ClaimAmount_binary = cut(
        ClaimAmount_noNA,
        breaks = c(-Inf, 0, Inf),
    ),
    claims_per_exposure = ClaimAmount_noNA / Exposure,
    log10Plus1_claims_per_exposure = log10((ClaimAmount_noNA / Exposure) + 1),
    # random train-test split
    tvt = sample(c("train", "validation", "test"), size = nrow(ins), replace = TRUE, prob = c(0.7, 0.2, .1))
)
# hist(ins_modelling$log10Plus1_claims_per_exposure)
summary(ins_modelling$claims_per_exposure)

# Model 1: Can we predict any claim?
binary_m0 <- stats::glm(ClaimAmount_binary ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, data = ins_modelling |> dplyr::filter(tvt == "train"), family = stats::binomial())
summary(binary_m0)
preds_df <- ins_modelling
preds_df[["pred"]] <- predict(binary_m0, ins_modelling, type = "response")

binary_m0_roc <- sapply(c("train", "validation", "test"), function(tvt_x) {
    with(
        preds_df |> dplyr::filter(tvt %in% c(tvt_x)),
        pROC::roc(
            predictor = pred,
            response = ClaimAmount_binary,
            levels = c("(-Inf,0]", "(0, Inf]"),
            direction = "<"
        )
    )
}, simplify = FALSE)
dir.create("res/model_classic", showWarnings = FALSE)
pdf("res/model_classic/binary_m0_roc.pdf")
print(pROC::ggroc(binary_m0_roc))
dev.off()

cutoff_m0 <- pROC::coords(binary_m0_roc[["validation"]], "best", ret = "all", best.method = "youden")
preds_df <- dplyr::mutate(
    preds_df,
    pred_class = factor(ifelse(pred > cutoff_m0[["threshold"]], "(0, Inf]", "(-Inf,0]"), levels = c("(-Inf,0]", "(0, Inf]"))
)
perf_binary_m0_roc <- sapply(c("train", "validation", "test"), function(tvt_x) {
    with(
        preds_df |> dplyr::filter(tvt %in% c(tvt_x)),
        # https://rdrr.io/cran/caret/man/confusionMatrix.html: confusionMatrix(pred, truth)
        caret::confusionMatrix(
            pred_class, # prediction
            ClaimAmount_binary # reference
        )
    )
}, simplify = FALSE)

sink("res/model_classic/binary_m0_roc.txt")
print(perf_binary_m0_roc)
sink()


# Model 2: Can we predict the claim per exposure?
continuous_m0 <- stats::glm(
    log10Plus1_claims_per_exposure ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region,
    data = ins_modelling |> dplyr::filter(tvt == "train"),
    family = stats::gaussian()
)
continuous_m1 <- stats::glm(
    log10Plus1_claims_per_exposure ~ Area + log10(VehPower) + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + log10(Density) + Region,
    data = ins_modelling |> dplyr::filter(tvt == "train"),
    family = stats::gaussian()
)
sink("res/model_classic/continuous_m0_linear.txt")
summary(continuous_m0)
summary(continuous_m1)
cat("RMSE M0\n")
preds_df <- tibble::tibble(
    truth = ins_modelling$log10Plus1_claims_per_exposure,
    pred = predict(continuous_m0, ins_modelling, type = "response"),
    tvt = ins_modelling$tvt
)
sapply(c("train", "validation", "test"), function(tvt_x) {
    with(
        preds_df |> dplyr::filter(tvt %in% c(tvt_x)),
        caret::RMSE(truth, pred)
    )
})
cat("RMSE M1\n")
preds_df <- tibble::tibble(
    truth = ins_modelling$log10Plus1_claims_per_exposure,
    pred = predict(continuous_m1, ins_modelling, type = "response"),
    tvt = ins_modelling$tvt
)
sapply(c("train", "validation", "test"), function(tvt_x) {
    with(
        preds_df |> dplyr::filter(tvt %in% c(tvt_x)),
        caret::RMSE(truth, pred)
    )
})
sink()

# The "z-scores" residuals saved in a GAMLSS object are the normalized (randomized) quantile residuals (see Dunn and Smyth, 1996).
plots_residuals_lm <- function(df_residuals_fitted) {
    return(
        list(
            ggplot(df_residuals_fitted, aes(x = fitted, y = residuals)) +
                geom_hex(aes(fill = log10(after_stat(count))), bins = 150) +
                labs(
                    x = "Fitted Values",
                    y = "Residuals",
                    title = "Against Fitted Values"
                ) +
                ggpubr::theme_pubr(),
            ggplot(df_residuals_fitted, aes(x = rank(fitted), y = residuals)) +
                geom_hex(aes(fill = log10(after_stat(count))), bins = 150) +
                labs(
                    x = "Rank(Fitted Values)",
                    y = "Residuals",
                    title = "Against Fitted Values"
                ) +
                ggpubr::theme_pubr(),
            ggplot(df_residuals_fitted, aes(x = residuals)) +
                geom_density() +
                labs(
                    x = "Residuals",
                    y = "Density",
                    title = "Density Estimate"
                ) +
                ggpubr::theme_pubr(),
            ggplot(df_residuals_fitted |> dplyr::sample_n(1e4), aes(sample = residuals)) +
                stat_qq(distribution = stats::qnorm) +
                labs(
                    x = "Theoretical Quantiles",
                    y = "Sample Quantiles",
                    title = "Normal Q-Q Plot"
                ) +
                ggpubr::theme_pubr() +
                geom_abline(intercept = 0, slope = 1, color = "red")
        )
    )
}

preds_df_lm <- lapply(list(continuous_m0, continuous_m1), function(model) {
    tmp_df <- ins_modelling
    tmp_df[["fitted"]] <- predict(model, ins_modelling, type = "response")
    tmp_df[["residuals"]] <- tmp_df[["log10Plus1_claims_per_exposure"]] - tmp_df[["fitted"]]
    return(tmp_df)
})
pdf("res/model_classic/continuous_m0_residuals.pdf")
plots_residuals_lm(
    preds_df_lm[[1]]
)
dev.off()
pdf("res/model_classic/continuous_m1_residuals.pdf")
plots_residuals_lm(
    preds_df_lm[[2]]
)
dev.off()

#### I think the following is a more correct distribution, unfinished. ####
pacman::p_install("gamlss.dist", force = FALSE)
pacman::p_install("gamlss", force = FALSE)

m0 <- gamlss::gamlss(
    log10Plus1_claims_per_exposure ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region,
    data = ins_modelling |>
        dplyr::filter(tvt == "train") |>
        dplyr::select(
            log10Plus1_claims_per_exposure, Area, VehPower, VehAge, DrivAge, BonusMalus, VehBrand, VehGas, Density, Region
        ),
    family = gamlss.dist::ZAGA
)
summary(m0)
pdf("removeme.pdf")
plot(m0)
dev.off()
# The "z-scores" residuals saved in a GAMLSS object are the normalized (randomized) quantile residuals (see Dunn and Smyth, 1996).