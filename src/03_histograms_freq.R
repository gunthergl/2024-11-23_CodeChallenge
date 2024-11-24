pacman::p_load("ggplot2")
pacman::p_install("DataExplorer", force = FALSE)
dir.create("res/01_first_view", showWarnings = FALSE)
intro <- DataExplorer::plot_intro(df_freq)
pdf("res/01_first_view/01_overview.pdf")
print(intro + ggpubr::theme_pubr())
dev.off()

discrete_vars <- DataExplorer::plot_bar(df_freq)
pdf("res/01_first_view/02_discrete_vars.pdf", width = 10)
print(discrete_vars[[1]] + ggpubr::theme_pubr())
dev.off()
#' Results:
#'   - Area:
#'       - 6 levels.
#'          Probably connected to "Region", I think that "Region" is a sub-level of "Area"
#'       - Visibly low amounts of Area "F", Most "C"
#'  - VehBrand:
#'      - 11 levels.
#'      - B1, B2 and B12 are almost identical in prevalence, far off B3, almost no B14.
#' - VehGas:
#'    - 2 levels: Regular + Diesel.
#'    - Almost identical prevalence.
#' - Region:
#'    - Many levels, heavily skewed. R24 most common, R43 least common.

# Check if Region is a sub-level of Area:
print(df_freq |> dplyr::select(Area, Region) |> dplyr::distinct() |> dplyr::arrange(Region, Area))
print(df_freq |> dplyr::select(Area, Region) |> dplyr::distinct() |> dplyr::arrange(Area), n = 1000)
#    Area  Region
#    <fct> <fct>
#  1 A     R11
#  2 B     R11
#  3 C     R11
#  4 D     R11
#  5 E     R11
#  6 F     R11
# --> No, Region is not a sub-level of Area, nor vice-versa.


hists <- DataExplorer::plot_histogram(df_freq)
pdf("res/01_first_view/03_hists.pdf", width = 13)
print(hists[[1]] + ggpubr::theme_pubr())
dev.off()
#' Results:
#'  BonusMalus:
#'    - Most have 50, then slightly right-skewed. Probably low number of very high (~250) outliers.
#'  - ClaimNb: Number of claims.
#'     - Most have 0 claims, then 2, then 3 claims. 4+ claims are very rare.
#'     - Only 16 IDpol have 4+ claims.
#'  - Density: Number of people per km2.
#'     - Why are there so many with very low density?
#'     - Right-skewed, most have very low density.
#'     - Visible peak at high density; Probably city areas
#' - DrivAge: Age of the driver.
#'     - Slightly right-skewed.
#'     - From histogram is looks like there is a hole at ~40 years? Confirm in detailed.
#' - Exposure: Length of the insurance period.
#'    - Two visible peaks; one around 0.1, one around 1. Very few have more than 1. Detailed!
#' - IDpol: ID of the policy.
#'   - Does almost seem normally distributed, is there a structure behind?

pdf("res/01_first_view/03_hists-detailed.pdf")
print(ggplot(df_freq, aes(x = BonusMalus)) +
    geom_histogram(bins = 100) +
    ggpubr::theme_pubr())
print(ggplot(df_freq, aes(x = log10(BonusMalus))) +
    geom_histogram(bins = 100) +
    ggpubr::theme_pubr())
print(ggplot(df_freq, aes(x = (BonusMalus))) +
    geom_histogram(bins = 100) +
    ggpubr::theme_pubr() +
    xlim(c(100, max(df_freq$BonusMalus))))
print(ggplot(df_freq, aes(x = (ClaimNb))) +
    geom_histogram(bins = 100) +
    ggpubr::theme_pubr() +
    scale_y_log10(labels = scales::label_comma()))
print(ggplot(df_freq |> dplyr::filter(ClaimNb > 2), aes(x = (ClaimNb))) +
    geom_histogram(bins = 100) +
    ggpubr::theme_pubr() +
    ggtitle("Histogram of ClaimNb, filtered for ClaimNb > 2"))
print(ggplot(df_freq |> dplyr::filter(ClaimNb > 2), aes(x = (ClaimNb))) +
    geom_histogram(bins = 100) +
    ggpubr::theme_pubr() +
    ggtitle("Histogram of ClaimNb, filtered for ClaimNb > 2"))
print(ggplot(df_freq, aes(x = Density)) +
    geom_histogram(bins = 100) +
    ggpubr::theme_pubr() +
    scale_y_log10(labels = scales::label_comma()))

tmp <- df_freq |>
    dplyr::count(DrivAge)
tmp <- dplyr::left_join(data.frame(DrivAge = 0:110), tmp, by = "DrivAge") |>
    dplyr::mutate(n = ifelse(is.na(n), 0, n)) |>
    dplyr::mutate(
        zero_values_color = ifelse(n == 0, "zero", "non-zero")
    )
print(ggplot(tmp, aes(x = DrivAge, y = n, col = zero_values_color)) +
    geom_point() +
    ggpubr::theme_pubr())

print(length(unique(df_freq$Exposure)))
# Only 181 unique values?
print(
    ggplot(df_freq |> dplyr::count(Exposure), aes(x = Exposure, y = n)) +
        geom_point() +
        ggpubr::theme_pubr() +
        scale_y_log10(labels = scales::label_comma()) +
        geom_hline(yintercept = 1, col = "red") +
        ggtitle("Number of occurences of unique Exposure values")
)
print(
    ggplot(df_freq |> dplyr::count(Exposure) |>
        dplyr::mutate(Exposure = Exposure * 365), aes(x = Exposure, y = n)) +
        geom_point() +
        ggpubr::theme_pubr() +
        scale_y_log10(labels = scales::label_comma()) +
        geom_hline(yintercept = 1, col = "red") +
        ggtitle("Number of occurences of unique Exposure values") +
        annotate(
            "text",
            x = 365,
            y = 2e5,
            label = "365 days",
            col = "red",
            size = 3,
            hjust = 0,
            vjust = 0
        ) +
        annotate(
            "text",
            x = 29.2,
            y = 5e4,
            label = "29.2 days",
            col = "red",
            size = 3,
            hjust = 0,
            vjust = 0
        ) +
        annotate(
            "text",
            x = 0,
            y = 1,
            label = "n=1",
            size = 5,
            hjust = 0,
            vjust = 0
        )
)


print(
    ggplot(df_freq |> dplyr::count(VehAge), aes(x = VehAge, y = n)) +
        geom_point() +
        ggpubr::theme_pubr() +
        scale_y_log10(labels = scales::label_comma()) +
        geom_hline(yintercept = 1, col = "red") +
        ggtitle("Number of occurences of unique VehAge values",
            subtitle =
                paste0("Range: ", min(df_freq$VehAge), " - ", max(df_freq$VehAge))
        )
)
print(
    ggplot(df_freq |> dplyr::count(VehPower), aes(x = VehPower, y = n)) +
        geom_point() +
        ggpubr::theme_pubr() +
        scale_y_log10(labels = scales::label_comma()) +
        geom_hline(yintercept = 1, col = "red") +
        ggtitle("Number of occurences of unique VehAge values",
            subtitle =
                paste0("Range: ", min(df_freq$VehAge), " - ", max(df_freq$VehAge))
        )
)
dev.off()
#' Result:
#' - BonusMalus: Some very high outliers. I have to identify how to calculate with BonusMalus - french Schadenfreiheitsrabatt. https://en.wikipedia.org/wiki/Bonus%E2%80%93malus
#' - ClaimNb; the log-plot looks well. It looks nice but note that some bars are missing because there is only count=1 for some values.
#'   - Density: Log-plot still looks right-skewed. Interesting that most have very low densities, how is this possible?
#' - DrivAge:
#'     - There is NO hole at any year. The hole was just a visual artifact.
#'     - Why is there noone above 100 years? Is this a data error?
#'     - Why are there 70 exactly 99 years old drivers? Surprisingly many, potentially this is the maximum number insertable in the entry system?
#' - Exposure:
#'    - There are only 181 unique values. This is very low. I have to check if this is a data error or if this is intended.
#'    - Comparably low number of exposure >1.
#'    - There are some values with exposure=0. This is not possible, I have to check if this is a data error or if this is intended.
#' - VehAge:
#'   - The range is from 0 to 100 years.
#'   - There seems to be a gap between 85 and 99+100 years. Notable data entry thing?
#'   - Number of cars decreasing until ~40 years, then similar until ~50, then drop heavily and only <10 cars.
#'   - The number of cars with 99 and 100 years is very high. This is something special. Oldtimer?
#' - VehPower:
#'    - The range is from 4 to 15.
#'    - log(n) plot is decreasing towards vehicle power, but almost uniform. -->LogUniform distribution?
#'

# Calculate the number of days per exposure. Unclear how a "year" is defined.
# I think it is 365 days, because with 365 we get exactly
# 1, 2 and 3 days as well as 0.997, 1.994 and 2.992 days. (So, "almost" each of the first days. )
(unique(df_freq$Exposure) |> sort()) * 365
(unique(df_freq$Exposure) |> sort()) * 365.2422
(unique(df_freq$Exposure) |> sort()) * 360

df_freq |>
    dplyr::mutate(
        Exposure_days = Exposure * 365
    ) |>
    dplyr::count(Exposure_days) |>
    dplyr::arrange(desc(n))


print(tail(df_freq |> dplyr::count(VehAge)))
# # A tibble: 6 × 2
#   VehAge     n
#    <dbl> <int>
# 1     82     1
# 2     83     2
# 3     84     1
# 4     85     1
# 5     99    23
# 6    100    25
