source("src/input.R")

# Read and combine vektisdecl 2014-2021
dt_combined <- data.table()
for (yr in vektis_years) { 
  
  path <- paste0("G:/GezondheidWelzijn/GGZDECLVEKTIS/geconverteerdeerde bestanden/GGZDECLVEKTIS",yr,"V1.parquet")
  print(path)
  dt <- r_parquet_get_dt(path) 

  dt <- format_data(dt)
  dt[, `:=` (start = as.integer(substr(trimws(as.character(begindatum_prestatie)), 1, 4)),
             eind = as.integer(substr(trimws(as.character(einddatum_prestatie)), 1, 4))
             )]
  # rm NA and sanity check
  dt <- dt[!is.na(start) & !is.na(eind) & eind >= start & start %between% c(2012L, 2025L)]
  
  
  # print all non_numeric codes
  #print(dt[!is.na(diagnosecode) & is.na(as.numeric(diagnosecode)), unique(diagnosecode)])
  
  # make numeric
  dt[, diagnosecode := as.numeric(diagnosecode)]
  
  # Set all onbekend codes to 1 number
  dt[(diagnosecode %in% c(5:6) | diagnosecode > 16) & diagnosecode != 999, diagnosecode := 111]
  
  # Set all geen of ontbrekende primaire diagnose to 1 number
  dt[is.na(diagnosecode) | diagnosecode == 0, diagnosecode := 999]
  
  # We only need unique treatment types per year
  dt <- unique(dt[, .(rinpersoon, productcode, diagnosecode, start, eind)])
  dt_combined <- rbindlist(list(dt_combined, dt), use.names = T)
}
gc()

# Almost all basis ggz producten have diagnose code onbekend
dt_combined[productcode %in% productcode_bggz, .N, by = diagnosecode]

# Spec ggz also has quit a lot of onbekende diagnoses
dt_combined[productcode %in% productcode_sggz, .N, by = diagnosecode]

# Potentially skip rows with unwanted diagnosecode/productcode combinaties
# drops onbekende diagnoses, maybe we want to keep those
#dt_combined <- dt_combined[!(productcode %in% productcode_sggz & !(diagnosecode %in% diagcode_sggz))]

# This drops people with spec ggz diagnoses that dont have a spec ggz productcode
dt_combined <- dt_combined[!(diagnosecode %in% diagcode_sggz & !(productcode %in% productcode_sggz))]


# If person has a declaratie starting in year j or earlier and ending in year j or later
# That person has had a treatment in year j (which you wouldnt pick up by just reading in the file per year)
# These lines of code go through all years and check if person has had a treatment
# in year j
# example: treatment starts in 2016, ends in 2017, loop catches the treatment for year 2016
# and for 2017. so you get 2 rows in the rbindlist result for single treatment, 1 for 2016, 1 for 2017
pj_oud <- rbindlist(lapply(vektis_report_years, function(j) {
  s <- dt_combined[start <= j & eind >= j]
  unique(s[, .(rinpersoon, productcode, diagnosecode)])[, jaar := j]
}), use.names = T)


pj <- unique(pj_oud[, .(rinpersoon, jaar, diagnosecode)])
pj[, `:=` (kolom = paste0("d_", diagnosecode), aanwezig = 1)]

w <- dcast(pj, rinpersoon + jaar ~ kolom, value.var = "aanwezig", fill = 0)

setindex(w, NULL)
arrow::write_parquet(w, "data/raw/00_ggz_diagnoses_tot_2021.parquet")

# Save overview of diagnosecode/productcode combinations
diag_product_combis <- pj_oud[, .N, 
                              by = c("diagnosecode", "productcode")][(order(productcode, -N))]
openxlsx::write.xlsx(diag_product_combis, "data/diag_productcode_combi.xlsx")


# Count GGZ gebruik
tel_oud <- rbindlist(list(
  pj_oud[, .(n = uniqueN(rinpersoon)), by = .(jaar, productcode, diagnosecode)],#[order(jaar, -n)]
  pj_oud[, .(n = uniqueN(rinpersoon)), by = .(jaar, diagnosecode)][, productcode := "alle"],
  pj_oud[, .(n = uniqueN(rinpersoon)), by = .(jaar, productcode)][, diagnosecode := "totaal"],
  pj_oud[, .(n = uniqueN(rinpersoon)), by = .(jaar)][, `:=` (productcode = "alle", diagnosecode = "totaal")]
), use.names = T)[, systeem := "2016-2021"]




#### GGZ ZPM ####
dt_combined <- data.table()
for (yr in zpm_years) { #CR: same suggestion for rbindlist(lapply()) to save time
  path <- paste0("G:/GezondheidWelzijn/GGZZPMPRESTATIETAB/GGZZPMPrestatie",yr,"TABV1.sav") #CR: consider converting to parquet in data folder for speed
  print(path)
  dt <- haven::read_sav(path, n_max = Inf, col_select = cols_zpm)
  dt <- format_data(dt)
  dt[, start := as.integer(substr(trimws(as.character(zpmbegindatumprestatie)), 1, 4))]
  
  dt <- unique(dt[, .(rinpersoon, zpmdiagnose, start)])
  
  dt[, jaar := yr]
  dt_combined <- rbindlist(list(dt_combined, dt), use.names = T)
  rm(dt)
  gc()
}

# Check that every year only has prestaties that started in that year
assertthat::assert_that(all.equal(dt_combined$start, dt_combined$jaar))
dt_combined[, start := NULL]

# check if all diagnoses are there and no extra unexpected ones
assertthat::assert_that(dt_combined[, uniqueN(zpmdiagnose)]  == 19)

dt_combined[, aanwezig := 1L]
w <- dcast(dt_combined, rinpersoon + jaar ~ zpmdiagnose, value.var = "aanwezig", fill = 0)

setindex(w, NULL)
arrow::write_parquet(w, "data/raw/00_ggz_diagnoses_vanaf_2022.parquet")
