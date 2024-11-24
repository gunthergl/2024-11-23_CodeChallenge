# df_freq
# df_sev


## Merge the two datasets
df_joint <- df_freq |> dplyr::full_join(df_sev, by = "IDpol") # why is this different?
df_joint <- df_freq |> dplyr::left_join(df_sev, by = "IDpol") # why is this different?
idpol_with_claim <- df_joint |> dplyr::filter(!is.na(ClaimAmount))
anyDuplicated(df_sev$IDpol)
claims_per_idpol <- dplyr::count(df_sev, IDpol) |> dplyr::arrange(desc(n))

df_freq |> dplyr::filter(IDpol %in% head(claims_per_idpol$IDpol, 10))
# claim IDpols which are not in the frequency dataset
missing_freq_ids <- setdiff(df_sev$IDpol, df_freq$IDpol)
df_sev |>
    dplyr::filter(IDpol %in% missing_freq_ids) |>
    dplyr::count(IDpol)
# # A tibble: 6 × 2
#     IDpol     n
#     <dbl> <int>
# 1 2220367    24
# 2 2227533    25
# 3 2262511    66
# 4 2277846    23
# 5 2282134    36
# 6 2286775    21

### There are some polices which have MANY claims, but they are not in the frequency dataset.
### I will omit them.
### Side idea: Predict the features of these missing policies?

claims_in_freq <- df_sev |> dplyr::filter(IDpol %in% df_freq$IDpol)
# Use only the claims where the IDpol is in the frequency dataset
df_insurance <- dplyr::left_join(df_freq, claims_in_freq, by = "IDpol")
df_insurance |>
    dplyr::filter(!is.na(ClaimAmount)) |>
    dplyr::count(IDpol) |>
    dplyr::arrange(desc(n))

# Note that some of the IDpols are now duplicated if there are multiple claims!


# How many mappable claims are there?
df_insurance |>
    dplyr::filter(!is.na(ClaimAmount)) |>
    dplyr::summarize(n = dplyr::n())
# # A tibble: 1 × 1
#       n
#   <int>
# 1 26444
# How many IDpols have any claims?
df_insurance |>
    dplyr::filter(!is.na(ClaimAmount)) |>
    dplyr::count(IDpol) |>
    dplyr::summarize(n = dplyr::n())
# # A tibble: 1 × 1
#       n
#   <int>
# 1 24944



df_insurance_any <- df_insurance |>
    dplyr::group_by(IDpol) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup()
if (!nrow(df_insurance_any) == nrow(df_freq)) {
    stop("The number of rows in the original df_freq and the joint, deduplicated data frame is not the same.")
}

df_insurance_any <- df_insurance_any |>
    dplyr::mutate(
        anyClaim = ifelse(is.na(ClaimAmount), FALSE, TRUE),
    )
