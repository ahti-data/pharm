# Prevalences of spec ggz 
source("src/input.R")

diag16_21 <- r_parquet_get_dt("data/raw/00_ggz_diagnoses_tot_2021.parquet")
diag22_24 <- r_parquet_get_dt("data/raw/00_ggz_diagnoses_vanaf_2022.parquet")
diag22_24[, c("BG1", "BG2", "BG3", "BG4") := NULL] # We dont look at basis ggz here

# check matches
# for (i in seq_len(nrow(rename_dt))) {
#   source <- intersect(rename_dt[["code_decl"]][[i]], names(diag16_21))
#   cat(i, rename_dt$name[i], length(source), "matche(es):", paste(source, collapse = ","), "\n")
# }
# 
# for (i in seq_len(nrow(rename_dt))) {
#   source <- intersect(rename_dt[["code_zpm"]][[i]], names(diag22_24))
#   cat(i, rename_dt$name[i], length(source), "matche(es):", paste(source, collapse = ","), "\n")
# }

rename_func <- function(dt, col_codes, remove_source = T) {
  # transform col names with rename table. Either only change column name
  # or combine multiple columns into 1
  # With remove_source, you can decide to keep only new cols or keep original diagnoses
  wide <- copy(dt)
  for (i in seq_len(nrow(rename_dt))) {
    source <- intersect(rename_dt[[col_codes]][[i]], names(wide))
    if (!length(source)) next
    
    name <- rename_dt$name[i]
    
    if (length(source) == 1) {
      if (source != name) wide[, (name) := get(source)]
    } else {
      wide[, (name) := as.integer(rowSums(.SD) > 0), .SDcols = source]
    }
    if (remove_source) wide[, (setdiff(source, name)) := NULL]
  }
  wide[]
}

wide_oud <- rename_func(diag16_21, "code_decl", remove_source = T)
wide_nieuw <- rename_func(diag22_24, "code_zpm", remove_source = T)

# Can check this if remove_source = T for wide_oud and wide_nieuw
assertthat::assert_that(all.equal(names(wide_oud), names(wide_nieuw)))

stap_cols <- c("rinpersoon", "leeftijd", "seswoa_cat", "inkomen_klasse", "hbopl")
stap_years_oud <- 2016:2021
stap_years_nieuw <- 2022:2024

compute_prev <- function(dt, diag_cols, by = NULL) {
  # Count number of diagnoses and compute prevalence per 1000 
  out <- dt[, c(lapply(.SD, sum), .(n_pop = .N)), .SDcols = diag_cols, by = by]
  out[, (diag_cols) := lapply(.SD, function(x) 1000 * x / n_pop), .SDcols = diag_cols]
  out[]
}
  
prevalentie <- function(wide, jaren, 
                        groepen = c("seswoa_cat", "inkomen_klasse",
                                    "leeftijd_groep", "hbopl")) {
  diag_cols <- setdiff(names(wide), c("rinpersoon", "jaar"))
  res <- list()
  
  for (i in seq_along(jaren)) {
    j <- jaren[i]
    path <- paste0("H:/data/demog/",j-1,"/rin_demog.parquet")
    
    if (j == 2024) {
      path <- "H:/data/demog/2023/rin_demog_v2.parquet"
      cat("jaar:", j, "pad:", path, "\n")
      pop <- r_parquet_get_dt(path, columns = stap_cols) 
    } else {
      cat("jaar:", j, "pad:", path, "\n")
      pop <- r_parquet_get_dt(path, columns = stap_cols)
    }
    # Drop people younger than 17. Keep in 17 because those people will become
    # 18 in year of ggz and can thus have ggz diagnsoe
    pop <- pop[leeftijd > 16]
    
    # Same age groups as used in IBO project
    pop[, leeftijd_groep := fcase(
      leeftijd < 30, "17-29",
      leeftijd < 50, "30-49",
      leeftijd < 66, "50-65",
      leeftijd < 76, "66-75",
      leeftijd < 86, "76-85",
      leeftijd >= 86, "86+"
    )]
    w <- wide[jaar == j, c("rinpersoon", diag_cols), with = F]
    
    pop <- merge(pop, w, all.x = T, by = "rinpersoon")
    
    pop[, (diag_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0L, x)), 
        .SDcols = diag_cols]
   
    # Count number of people per diagnose and compute prevalences
    # @outcome 22-34
    res$totaal[[i]] <- compute_prev(pop, diag_cols)[, jaar := j]
    for (g in groepen) res[[g]][[i]] <- compute_prev(pop, diag_cols, by = g)[, jaar := j]
    
  }
  lapply(res, function(l) {
    out<- rbindlist(l, use.names = T, fill = T)
    setcolorder(out, c("jaar", "n_pop", setdiff(names(out), c("jaar", "n_pop"))))
    out
  })
}

prev_nieuw <- prevalentie(wide_nieuw, stap_years_nieuw)
nieuw_total <- prev_nieuw$totaal
nieuw_ses <- prev_nieuw$seswoa_cat
nieuw_income <- prev_nieuw$inkomen_klasse
nieuw_leeftijd <- prev_nieuw$leeftijd_groep
nieuw_opleiding <- prev_nieuw$hbopl

prev_oud <- prevalentie(wide_oud, stap_years_oud)
oud_total <- prev_oud$totaal
oud_ses <- prev_oud$seswoa_cat
oud_income <- prev_oud$inkomen_klasse
oud_leeftijd <- prev_oud$leeftijd_groep
oud_opleiding <- prev_oud$hbopl
gc()


# Bind old and new years together
# Can only do this when old columns are removed, so only do this at the end
bind <- setNames(
  lapply(names(prev_oud), function(name)
    rbindlist(list(prev_oud[[name]], prev_nieuw[[name]]), use.names = T, fill = T)),
  names(prev_oud)
)

# Write 
openxlsx::write.xlsx(bind, "data/results/prevalenties.xlsx")

