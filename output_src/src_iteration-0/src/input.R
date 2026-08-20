source("H:/utils/m_functions.R")
source("H:/utils/demog_functions.R")
library(data.table)

med_vars <- c("RINPERSOON", "ATC4")#, "DDD", "Datumaflevering")
atc4_codes <- c("N05A", "N05B", "N05C", "N06A", "N06B", "N06D")

productcode_sggz <- c(20, 21)#, "51") # 51 is langdurige ggz in zvw
productcode_bggz <-c(30, 80)
diagcode_sggz <- c(1, 2, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) # 5, 6 onbekend
cols_vektis <- c("RINPERSOON", "Productcode", "Diagnosecode", "begindatum_prestatie",
                 "einddatum_prestatie")
cols_zpm <- c("RINPERSOON", "ZPMDiagnose", "ZPMBegindatumprestatie")


vektis_years <- 2014:2021
vektis_report_years <- 2016:2021
zpm_years <- 2022:2024


# table to map codes from GGZ vektisdecl and GGZ ZPM to coherent groups and names
rename_dt <- data.table(
  name = c(
    "schizofrenie", "depr_stemming_stoornis", "bipolaire_stemming_stoornis",
    "angststoornis", "persoonlijkheidstoornis", "middelger_versl_stoornis",
    "neurobio_ontw_stoornis", "neurocog_stoornis", "som_symp_stoornis",
    "voeding_eet_stoornis", "restgroep", "onbekend"),
  code_decl = list(
    c("d_10"), c("d_11"), c("d_12"), 
    c("d_13"), c("d_14"), c("d_8", "d_9"), 
    c("d_1", "d_2", "d_3"), c("d_7"), c("d_15"),
    c("d_16"), c("d_4"), c("d_111", "d_999")
  ),
  code_zpm = list(
    c("SG2"), c("SG4"), c("SG3"),
    c("SG5", "SG6", "SG7"), c("SG18"), c("SG16"),
    c("SG1"), c("SG17"), c("SG9"),
    c("SG10"), c("SG22", "SG23"), c("999_geendiag")
  )
)