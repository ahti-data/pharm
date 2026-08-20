## Medicijn tab
source("src/input.R")

demog <- r_parquet_get_dt("H:/data/demog/2023/rin_demog_v2.parquet")
#demog <- add_seswoa(demog, 2023)
# setindex(demog, NULL)
# arrow::write_parquet(demog, "H:/data/demog/2023/rin_demog_v2.parquet")

demog <- demog[, .(rinpersoon, inkomen_klasse, seswoa_cat)]#CR: pass columns to r_parquet_get_dt, it saves loading time & code lines

n_max = Inf
med_use <- function(years) {
  
  dt <- data.table()
  for (yr in years) {
    
    path <- "G:/GezondheidWelzijn/MEDICIJNDATUMTAB/"
    
    med_path <- get_path_newest(path, yr, extension = ".SAV")
    print(med_path)
    
    med_dt <- haven::read_sav(med_path, col_select = med_vars, n_max = n_max)
    med_dt <- format_data(med_dt)
    med_dt <- med_dt[atc4 %chin% atc4_codes]
    
    # Make dummies for each ATC code
    med_wide <- dcast(med_dt, rinpersoon ~ atc4, fun.aggregate = length, 
                      value.var = "atc4")
    # Extra check to set all dummies to 0 or 1, in case of registration errors
    med_wide[, (atc4_codes) := lapply(.SD, function(x) as.integer(x > 0)), 
             .SDcols = atc4_codes]
    
    # Number of ATC codes pp
    med_wide[, n_meds := rowSums(.SD, na.rm = T), .SDcols = atc4_codes]
    
    # Make groups
    med_wide[, med_class := fcase(
      n_meds == 1, "1",
      n_meds == 2, "2",
      n_meds == 3, "3",
      n_meds >= 4, "4+"
    )]
    
    med_wide[, year := yr]
    dt <- rbindlist(list(dt, med_wide), use.names = T)
  }
  return(dt)
}

med <- med_use(2024)

# Add an ATC combination var
med[, combi := apply(.SD == 1, 1, function(x) {
  if (!any(x)) "geen" else paste(atc4_codes[x], collapse = "_")
}), .SDcols = atc4_codes]

med <- merge(med, demog, by = "rinpersoon", all.x = T)

setindex(med, NULL)
arrow::write_parquet(med, "data/raw/00_atc_2024.parquet")

