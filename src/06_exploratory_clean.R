pacman::p_load("ggplot2")
pacman::p_install("DataExplorer", force = FALSE)
dir.create("res/03_explore_clean", showWarnings = FALSE)


ins <- qs::qread("intermediate/insurance_clean.qs")


intro <- DataExplorer::plot_intro(ins)
pdf("res/03_explore_clean/01_overview.pdf")
print(intro + ggpubr::theme_pubr())
dev.off()

discrete_vars <- DataExplorer::plot_bar(ins)
pdf("res/03_explore_clean/02_discrete_vars.pdf", width = 10)
print(discrete_vars[[1]] + ggpubr::theme_pubr())
dev.off()

hists <- DataExplorer::plot_histogram(ins)
pdf("res/03_explore_clean/03_hists.pdf", width = 13)
print(hists[[1]] + ggpubr::theme_pubr())
dev.off()


### Try to apply proper transformations to the data
# https://www.service-public.fr/particuliers/vosdroits/F2655?lang=en#:~:text=The%20bonus%2Dmalus%20system%2C%20also,deadline%2C%20depending%20on%20your%20behavior.
# Bonus-malus system:
# Bonus:
#   For each year without a responsible accident, you receive a discount of 5% on your previous year's coefficient.
#   Multiply the previous year's coefficient by 0.95 to obtain the year's coefficient,
#   if there was no accident with a share of responsibility. By default, the coefficient is rounded to 2 decimal places.
#   The maximum reduction is set at 50% (coefficient 0.50).
#   When you reach that level, the coefficient can no longer fall.
# Malus:
#   For each accident responsible, the insured person is subject to an increase of 25%.
#   To determine the coefficient which will result from this increase, the coefficient before the accident is taken and multiplied by 1.25.
#   If you have had a coefficient of 0.50 for at least 3 years, the 1er the responsible accident that occurs does not result in the malus being applied.

# BonusMalus seems to be in percent (50-230) instead of a coefficient (0.5-2.3)
range(ins$BonusMalus)

with(ins |> dplyr::filter(ClaimAmount > 0), hist(log10(ClaimAmount)))
ins_trans <- ins |>
    dplyr::mutate(
        ClaimAmount_log10 = log10(ClaimAmount),
        BonusMalus_small = BonusMalus / 100,
        Density_log10 = log10(Density),
        VehAge_log10 = log10(VehAge),
        VehPower_log10 = log10(VehPower),
        amount_per_exposure = ClaimAmount / Exposure
    )

hists <- DataExplorer::plot_histogram(ins_trans)
pdf("res/03_explore_clean/03_hists_trans.pdf", width = 13)
print(hists[[1]] + ggpubr::theme_pubr())
dev.off()

pdf("res/03_explore_clean/04_hists_claimamount.pdf")
print(
    ggplot(ins_trans |> dplyr::filter(amount_per_exposure > 0), aes(x = amount_per_exposure)) +
        geom_histogram(bins = 50) +
        ggpubr::theme_pubr() +
        scale_x_log10(label = scales::label_comma())
)
dev.off()

ins_trans |>
    dplyr::filter(amount_per_exposure > 0) |>
    dplyr::mutate(amount_per_exposure = log10(amount_per_exposure)) |>
    dplyr::summarise(
        mean = mean(amount_per_exposure),
        median = median(amount_per_exposure),
        sd = sd(amount_per_exposure),
        q05 = quantile(amount_per_exposure, 0.05),
        q95 = quantile(amount_per_exposure, 0.95)
    )

sort(table(ins_trans$ClaimAmount)) |> tail(25)
print(
    ins_trans$ClaimAmount |>
        log10() |>
        cut(50) |>
        table() |>
        sort() |>
        tail(10)
)
# 12613 cases are between 10^3.04 (1096) and 10^3.17 (1479)
