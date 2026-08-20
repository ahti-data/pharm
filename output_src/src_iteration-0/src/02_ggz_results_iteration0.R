# GGZ data
source("src/input.R")

ggz24 <- haven::read_sav("G:/GezondheidWelzijn/GGZZPMPRESTATIETAB/GGZZPMPrestatie2024TABV1.sav",
                         n_max = Inf, 
                         col_select = c("RINPERSOON", "ZPMZorgvraagtypering",
                                        "ZPMDiagnose", "ZPMPrestatiecode",
                                        "ZPMVergoedBedragZorgverzekeringswet"))
ggz24 <- format_data(ggz24)

demog <- r_parquet_get_dt("H:/data/demog/2023/rin_demog_v2.parquet",
                          columns = c("rinpersoon", "geslacht", "leeftijd",
                                      "hbopl", "inkomen_klasse", "seswoa_cat"))

# Some exploration
# ggz24[, unique(zpmzorgvraagtypering)]
# ggz24[, unique(zpmdiagnose)]
# 
# # missing zorgvraagtypering
# ggz24[zpmzorgvraagtypering == "geenZT", .N]
# 
# # missing diagnoses
# ggz24[zpmdiagnose == "999_geendiag", .N]
# 
# # both missing
# ggz24[zpmdiagnose == "999_geendiag" & zpmzorgvraagtypering == "geenZT", .N]  
# 
# # distribution of zorgvraagtypering for missing diagnoses
# ggz24[zpmdiagnose == "999_geendiag" & zpmzorgvraagtypering != "geenZT", .N, 
#       by = zpmzorgvraagtypering][order(-N)]
# 
# # distribution of diagnoses for missing zorgvraagtypering
# ggz24[zpmdiagnose != "999_geendiag" & zpmzorgvraagtypering == "geenZT", .N,
#       by = zpmdiagnose][order(-N)]
      

# Define dummies: basis ggz, spec. ggz and verblijfsdag 
ggz24[, bg := as.integer(substr(trimws(zpmdiagnose), 1, 2) == "BG")]
ggz24[, sg := as.integer(substr(trimws(zpmdiagnose), 1, 2) == "SG")]
ggz24[, vd := as.integer(substr(trimws(zpmprestatiecode), 1, 2) == "VD")]
ggz24[, sg_vd := fifelse(sg == 1 & vd == 1, 1, 0)]
ggz24[, sg_no_vd := fifelse(sg == 1 & vd == 0, 1, 0)]

# merge ses, income to ggz 
ggz24 <- merge(ggz24, demog, by = "rinpersoon", all.x = T)

# we miss same people for in komen as for ses
# drop people that are not in stapeling for equal compariison of outcomes
ggz24 <- ggz24[!is.na(seswoa_cat)]

gc()

#throw away BG and VD combination
ggz24 <- ggz24[! (bg == 1 & vd == 1)]

ggz24[, leeftijd_groep := fcase(
  leeftijd < 30, "17-29", # no one is younger than 17
  leeftijd < 50, "30-49",
  leeftijd < 66, "50-65",
  leeftijd < 76, "66-75",
  leeftijd < 86, "76-85",
  leeftijd >= 86, "86+"
)]

# cost and usage per type
totaal <- list()
per_groep <- list()

dummies <- c(bg = "bg", sg_no_vd = "sg_no_vd", sg_vd = "sg_met_vd")
groepen <- c(ses = "seswoa_cat",
             inkomen = "inkomen_klasse",
             opleiding = "hbopl",
             leeftijd = "leeftijd_groep")

for (d in names(dummies)) {
  sub <- ggz24[get(d) == 1]
  label <- dummies[[d]]
  
  # @outcome 12, 13
  totaal[[d]] <- data.table(
    categorie = label,
    totale_kosten = sub[, sum(zpmvergoedbedragzorgverzekeringswet, na.rm = T)],
    gebruikers = sub[, uniqueN(rinpersoon)]
  )
  
  # @outcome 15, 16, 18, 19
  for (gr in names(groepen)) {
    g <- groepen[[gr]]
    per_groep[[gr]][[d]] <- sub[, .(
      categorie = label,
      totale_kosten = sum(zpmvergoedbedragzorgverzekeringswet, na.rm = T),
      gebruikers = uniqueN(rinpersoon)
    ), by = g]
  }
}

# @outcome 14
tab_totaal <- rbindlist(totaal)
tab_totaal[, gem_kosten_per_gebr := totale_kosten / gebruikers]

# @outcome 17, 20
for (gr in names(groepen)) {
  per_groep[[gr]] <- rbindlist(per_groep[[gr]])
  per_groep[[gr]][, gem_kosten_per_gebr := totale_kosten / gebruikers]
  setorderv(per_groep[[gr]], c("categorie", groepen[[gr]]))
}


openxlsx::write.xlsx(c(list(totaal = tab_totaal), per_groep), 
                     "data/results/iteration0/iteration0_ggz.xlsx", overwrite = T)