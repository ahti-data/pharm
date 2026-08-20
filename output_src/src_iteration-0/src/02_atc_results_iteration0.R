# Medication use outcomes for iteration 0
source("src/input.R")

dt <- r_parquet_get_dt("data/raw/00_atc_2024.parquet")
dt[, c("inkomen_klasse", "seswoa_cat") := NULL]
atc_cols <- setdiff(names(dt), c("rinpersoon", "med_class", "year", "combi"))

demog <- r_parquet_get_dt("H:/data/demog/2023/rin_demog_v2.parquet")

demog <- merge(demog, dt, by = "rinpersoon", all.x = T)

demog[, (atc_cols) := lapply(.SD, nafill, fill=0), .SDcols = atc_cols]
demog[is.na(med_class), med_class := "0"]

groepen <- list("total" = NULL,
                "seswoa" = "seswoa_cat",
                "inkomen" = "inkomen_klasse",
                "opleiding" = "hbopl",
                "leeftijd" = "leeftijd_groep")

prevs <- list()

demog[, leeftijd_groep := fcase(
  leeftijd < 18, "0-17",
  leeftijd < 30, "18-29", 
  leeftijd < 50, "30-49",
  leeftijd < 66, "50-65",
  leeftijd < 76, "66-75",
  leeftijd < 86, "76-85",
  leeftijd >= 86, "86+"
)]

# @outcome 1-5, 7-11
for (groep in names(groepen)) {
  g <- groepen[[groep]]
  print(g)
  prevs[[groep]] <- demog[, .(
    n = .N,
    prev_N05A = sum(N05A == 1) / .N * 1000,
    prev_N05B = sum(N05B == 1) / .N * 1000,
    prev_N05C = sum(N05C == 1) / .N * 1000,
    prev_N06A = sum(N06A == 1) / .N * 1000,
    prev_N06B = sum(N06B == 1) / .N * 1000,
    prev_N06D = sum(N06D == 1) / .N * 1000,
    prev_monofarmacie = sum(med_class == "1") / .N * 1000,
    prev_polyfarmacie_2 = sum(med_class == "2") / .N * 1000,
    prev_polyfarmacie_3 = sum(med_class == "3") / .N * 1000,
    prev_polyfarmacie_4plus = sum(med_class == "4+") / .N * 1000
  ), by = g]
}

# Count (most occuring) ATC combinations
# @outcome 21
combi_counts <- dt[, .N, by = combi][order(-N)]


openxlsx::write.xlsx(
  c(prevs, list("ATC_combinaties" = combi_counts)),
  file = "data/results/iteration0/iteration0_atc.xlsx"
)
