# "intermediate/freMTPL2freq.arff"
# "intermediate/freMTPL2sev.arff"

pacman::p_install("farff", force=FALSE)
df_freq <- farff::readARFF("intermediate/freMTPL2freq.arff") |> tibble::as_tibble()
df_sev <- farff::readARFF("intermediate/freMTPL2sev.arff") |> tibble::as_tibble()


# freMTPL2freq:
# • IDpol: ID des Vertrags
# • ClaimNb: Anzahl Schäden im Versicherungszeitraum
# • Exposure: Länge des Versicherungszeitraums (in Jahren) [Komponente der Zielvariable]
# • Area: Area-Code des Versicherungsnehmers [unabhängige Variable]
# • VehPower: Leistung des versicherten Kfz [unabhängige Variable]
# • VehAge: Alter des versicherten Kfz [unabhängige Variable]
# • DrivAge: Alter des Versicherungsnehmers [unabhängige Variable]
# • BonusMalus: Schadenfreiheitsrabatt (französische Entsprechung der Schadenfreiheitsklasse) [unabhängige Variable]
# • VehBrand: Marke des versicherten Kfz [unabhängige Variable]
# • VehGas: Antrieb des versicherten Kfz [unabhängige Variable]
# • Density: Anzahl der Einwohner pro km2 im Wohnort des Versicherungsnehmers [unabhängige Variable]
# • Region: Region des Versicherungsnehmers [unabhängige Variable]

# freMTPL2sev:
# • IDpol: ID des Vertrags
# • ClaimAmount: Schadenshöhe im Versicherungszeitraum [Komponente der Zielvariable]
# • ClaimInd: Indikator für Schaden (1 = Schaden, 0 = kein Schaden) [Komponente der Zielvariable]
