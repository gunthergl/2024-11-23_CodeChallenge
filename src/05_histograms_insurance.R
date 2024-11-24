pacman::p_load("ggplot2")
pacman::p_install("DataExplorer", force = FALSE)
dir.create("res/02_joint_view", showWarnings = FALSE)

intro <- DataExplorer::plot_intro(df_insurance_any)
pdf("res/02_joint_view/01_overview.pdf")
print(intro + ggpubr::theme_pubr())
dev.off()

discrete_vars <- DataExplorer::plot_bar(df_insurance_any)
pdf("res/02_joint_view/02_discrete_vars.pdf", width = 10)
print(discrete_vars[[1]] + ggpubr::theme_pubr())
dev.off()

hists <- DataExplorer::plot_histogram(df_insurance_any)
pdf("res/02_joint_view/03_hists.pdf", width = 13)
print(hists[[1]] + ggpubr::theme_pubr())
dev.off()

# There are MANY more policies with no claims than with claims.
# Does ClaimNb and number of claims correspond?
df_insurance |>
    dplyr::count(ClaimNb)
df_freq |>
    dplyr::count(ClaimNb)

df_insurance |>
    dplyr::filter(ClaimNb == 11) |>
    print(n = 1000)

# Why do ClaimNb and number of IDpols with claims not match?
df_insurance_recalc <- df_insurance |>
    dplyr::group_by(IDpol) |>
    dplyr::summarize(n_claims_recalculated = sum(ClaimAmount > 0, na.rm = TRUE)) |>
    dplyr::ungroup()
df_insurance_recalc_merged <- df_insurance_recalc |>
    dplyr::left_join(df_insurance, by = "IDpol")
not_matching_idpol <- df_insurance_recalc_merged |>
    dplyr::filter(n_claims_recalculated != ClaimNb)




####### I now ASSERT, that the correct data is in df_sev, NOT in df_freq.
####### This is ONLY done for this example analysis, usually I would now go back to the data source and understand what happened.
####### I will now continue with the analysis, assuming that df_sev is the correct data source.
####### For this I will remove all IDpol which have contrary number of claims

# Make sure that I didn't do stupid:
df_freq |>
    dplyr::count(IDpol) |>
    dplyr::count(n)
claims_counted <- df_sev |>
    dplyr::count(IDpol) |>
    dplyr::mutate(
        n = ifelse(is.na(n), 0, n)
    )

table(df_freq$ClaimNb)
# The sum of ClaimNb should actually match the number of claims in df_sev:
print(sum(df_freq$ClaimNb)) # 36102
print(sum(df_sev$ClaimAmount > 0, na.rm = TRUE)) # 26639

df_insurance_clean <- df_insurance |>
    dplyr::filter(!(IDpol %in% not_matching_idpol$IDpol))


# Calculate the number of claims per IDpol according to the ClaimNb
n_insurance_claims <- df_insurance_clean |>
    dplyr::count(ClaimNb) |>
    dplyr::mutate(
        perc = n / sum(n)
    )

# Calculate the number of claims per IDpol according to the ClaimAmount
n_insurance_claims_recalc <- df_insurance_clean |>
    dplyr::group_by(IDpol) |>
    dplyr::summarize(
        n_claims_recalculated = sum(ClaimAmount > 0, na.rm = TRUE)
    )
# For each number of claims, and its prevalence, calculate the resulting number of rows
# after df_insurance_clean holds EACH claim for each IDpol.
# Effectively, the number of claims per IDpol should be the same as the number of rows per IDpol.
tmp <- n_insurance_claims_recalc |>
    dplyr::count(n_claims_recalculated) |>
    dplyr::mutate(
        merged_rows_perID = n * n_claims_recalculated
    )

if (!tmp[["n"]][1] == n_insurance_claims[["n"]][1] ||
    !all(tmp[["merged_rows_perID"]][-1] == n_insurance_claims[["n"]][-1])) {
    stop("The number of claims per IDpol does not match the number of rows per IDpol.")
}

qs::qsave(df_insurance_clean, "intermediate/insurance_clean.qs")
