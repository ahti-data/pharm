#' Data loading and reshaping for the pharm/GGZ dashboard.
#'
#' The four source workbooks (`data/*.xlsx`) are outputs of the `pharm`
#' repo's iteration-0 pipeline (see the top-level `README.md`): already
#' aggregated, non-traceable prevalence/cost tables built from CBS-linked
#' microdata inside the CBS Remote Access environment. Nothing here reads
#' individual-level data -- every function below only reshapes the
#' already-aggregated sheets into long, chart-friendly tibbles.
#'
#' Every load_*() function returns the exact-population data with its
#' subgroup/category columns as plain character -- factor ordering (so a
#' chart's x-axis/legend follows a meaningful order instead of alphabetical)
#' is applied later, in `app.R`'s own reactives, via [pharm_order_subgroep()],
#' right before the same ordered data feeds both the plot and
#' `chart_data_downloads_server()` (see CLAUDE.md's "Preserving the plotted
#' bar/category order" convention).

PHARM_DATA_DIR   <- "data"
PHARM_ATC_FILE   <- file.path(PHARM_DATA_DIR, "iteration0_atc.xlsx")
PHARM_GGZ_FILE   <- file.path(PHARM_DATA_DIR, "iteration0_ggz.xlsx")
PHARM_PREV_FILE  <- file.path(PHARM_DATA_DIR, "prevalenties.xlsx")
PHARM_ANGST_FILE <- file.path(PHARM_DATA_DIR, "prevalenties_angst_22_24.xlsx")

#' Uitsplitsingen ("dimensies") beschikbaar op elk tabblad: de bevolking als
#' geheel, of uitgesplitst naar een van de vier demografische/SES-kenmerken
#' die in alle vier bronbestanden terugkomen.
DIM_CHOICES <- c(
  "Totale bevolking"   = "totaal",
  "SES-WOA"             = "seswoa",
  "Inkomensklasse"      = "inkomen",
  "Opleidingsniveau"    = "opleiding",
  "Leeftijdsgroep"      = "leeftijd"
)

#' Raw column name holding the subgroup value for each non-"totaal" dimension.
DIM_COL_MAP <- c(
  seswoa    = "seswoa_cat",
  inkomen   = "inkomen_klasse",
  opleiding = "hbopl",
  leeftijd  = "leeftijd_groep"
)

#' Dictionary `scope` matching each dimension's raw column (see
#' `data/metadata/dictionary_seed.R`) -- used both for on-screen relabeling
#' and for `chart_data_downloads_server()`'s `category_scope`/`series_scope`.
DIM_SCOPE_MAP <- c(
  seswoa    = "seswoa_cat",
  inkomen   = "inkomen_klasse",
  opleiding = "hbopl",
  leeftijd  = "leeftijd_groep",
  totaal    = "subgroep_totaal"
)

ATC_KLASSE_COLS <- c("N05A", "N05B", "N05C", "N06A", "N06B", "N06D")
ATC_POLY_COLS   <- c("monofarmacie", "polyfarmacie_2", "polyfarmacie_3", "polyfarmacie_4plus")

GGZ_CATEGORIE_ORDER <- c("bg", "sg_no_vd", "sg_met_vd")

DIAGNOSEGROEP_COLS <- c(
  "schizofrenie", "depr_stemming_stoornis", "bipolaire_stemming_stoornis",
  "angststoornis", "persoonlijkheidstoornis", "middelger_versl_stoornis",
  "neurobio_ontw_stoornis", "neurocog_stoornis", "som_symp_stoornis",
  "voeding_eet_stoornis", "restgroep", "onbekend"
)

SG_GROEP_COLS <- c("SG5", "SG6", "SG7")

SESWOA_ORDER    <- c("0-10%", "10-20%", "20-35%", "35-50%", "50-75%", "75-100%", "Onbekend")
INKOMEN_ORDER   <- c("tot_120", "120_160", "160_200", "200_240", "240_280", "280_400", "400+",
                      "student", "Onbekend_institutioneel")
OPLEIDING_ORDER <- c("basisonderwijs, vmbo, mbo1", "havo, vwo, mbo2-4", "hbo, wo", "onbekend")
LEEFTIJD_ORDER_FULL    <- c("0-17", "18-29", "30-49", "50-65", "66-75", "76-85", "86+")
LEEFTIJD_ORDER_17PLUS  <- c("17-29", "30-49", "50-65", "66-75", "76-85", "86+")

#' Harmonize `hbopl` labels across years: `prevalenties.xlsx` and
#' `prevalenties_angst_22_24.xlsx` use the short codes "lager"/"middelbaar"/
#' "hoger"/"onbekend" for 2016-2023 and the longer codes also used by
#' `iteration0_atc.xlsx`/`iteration0_ggz.xlsx` from 2024 onward. Recoding the
#' short codes to their long equivalent keeps one continuous series per
#' education level instead of the time series silently splitting into two
#' unrelated categories at the 2024 boundary.
#' @param x Character vector of raw `hbopl` values.
#' @return Character vector using the long codes throughout.
harmonize_hbopl <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "lager"      ~ "basisonderwijs, vmbo, mbo1",
    x == "middelbaar"  ~ "havo, vwo, mbo2-4",
    x == "hoger"       ~ "hbo, wo",
    TRUE               ~ x
  )
}

pharm_read_sheet <- function(path, sheet) {
  tibble::as_tibble(readxl::read_excel(path, sheet = sheet))
}

#' Attach a `subgroep` character column derived from whichever raw column
#' this dimension actually uses (or the literal `"Totaal"` for `dim ==
#' "totaal"`), harmonizing `hbopl` along the way.
#' @param df One dimension's sheet, already read.
#' @param dim One of `names(DIM_CHOICES)`'s values.
pharm_attach_subgroep <- function(df, dim) {
  if (identical(dim, "totaal")) {
    df$subgroep <- "Totaal"
    return(df)
  }
  raw_col <- DIM_COL_MAP[[dim]]
  df$subgroep <- as.character(df[[raw_col]])
  if (identical(dim, "opleiding")) {
    df$subgroep <- harmonize_hbopl(df$subgroep)
  }
  df
}

#' Order a subgroup column as a factor, so a chart's x-axis follows a
#' meaningful order (percentile bands low-to-high, income bands low-to-high,
#' age bands young-to-old, education low-to-high) instead of alphabetical.
#' Any value not in the expected order (there shouldn't be one) is appended
#' at the end rather than dropped.
#' @param x Character vector of subgroup values.
#' @param dim One of `names(DIM_CHOICES)`'s values.
#' @param leeftijd_order Age-band order to use -- [LEEFTIJD_ORDER_FULL] for a
#'   whole-population sheet (includes "0-17"), [LEEFTIJD_ORDER_17PLUS] for a
#'   17+ population sheet (GGZ/prevalence/anxiety sheets).
pharm_order_subgroep <- function(x, dim, leeftijd_order = LEEFTIJD_ORDER_17PLUS) {
  order <- switch(dim,
    seswoa    = SESWOA_ORDER,
    inkomen   = INKOMEN_ORDER,
    opleiding = OPLEIDING_ORDER,
    leeftijd  = leeftijd_order,
    "Totaal"
  )
  extra <- base::setdiff(unique(x), order)
  factor(x, levels = c(order, extra))
}

#' Relabel a (possibly ordered-factor) column through the shared Dictionary,
#' preserving factor level order -- same approach as
#' `relabel_column()`/`identity_fallback` in `utils/chart_downloads.R`, so an
#' unmapped raw value (e.g. a subgroup band not yet in the dictionary) is
#' left exactly as-is on screen rather than run through
#' `dictionary_default_prettify()`'s generic title-caser, which would mangle
#' a dash-range label like `"76-85"` into `"76 85"`.
#' @param x Character or factor vector.
#' @param scope Dictionary scope (see `data/metadata/dictionary_seed.R`).
pharm_relabel_factor <- function(x, scope) {
  identity_fn <- function(v) v
  relabeled <- dictionary_relabel(x, scope = scope, fallback = identity_fn)
  if (is.factor(x)) {
    relabeled <- factor(relabeled, levels = dictionary_relabel(levels(x), scope = scope, fallback = identity_fn))
  }
  relabeled
}

#' A categorical fill/colour palette of exactly `n` colours, built from
#' `ahti_branding$scale_discrete` (`data/metadata/brand_colors.R`) and
#' interpolated with [grDevices::colorRampPalette()] whenever a chart needs
#' more categories (e.g. all 6 ATC classes, or several diagnosis groups at
#' once) than the brand palette's 5 base colours.
#' @param n Number of distinct categories.
pharm_fill_palette <- function(n) {
  base_colors <- ahti_branding$scale_discrete
  if (n <= length(base_colors)) return(base_colors[seq_len(n)])
  grDevices::colorRampPalette(base_colors)(n)
}

#' Plain-notation number formatter -- ggplot's default axis labels fall back
#' to scientific notation ("8e+05") for large values; `scales::label_number()`
#' always renders fixed/plain notation instead, with Dutch thousands/decimal
#' separators (`.`/`,`). Used both as a `scale_*_continuous(labels = ...)`
#' argument (call with no `accuracy`, so it auto-picks a sensible decimal
#' count from the actual axis breaks) and, called directly on a vector with
#' an explicit `accuracy`, to format the same numbers for the data table
#' under each chart.
#' @param accuracy Passed through to `scales::label_number()`; `NULL`
#'   (default) auto-picks precision, a number fixes it (e.g. `1` for whole
#'   numbers, `0.1` for one decimal).
pharm_number_labels <- function(accuracy = NULL) {
  scales::label_number(accuracy = accuracy, big.mark = ".", decimal.mark = ",")
}

#' Plain-notation formatter for a numeric year axis/column (e.g. `jaar` as a
#' continuous x-scale) -- same "never scientific" guarantee as
#' [pharm_number_labels()], but without a thousands separator, since a year
#' like 2016 is not a quantity and `pharm_number_labels(1)(2016)` would
#' otherwise misleadingly render it as "2.016".
pharm_year_labels <- function() {
  scales::label_number(accuracy = 1, big.mark = "")
}

#' Long-format prevalentie (per 1.000 personen) van psychofarmacagebruik,
#' per uitsplitsing en per ATC-klasse of mono-/polyfarmaciegroep. Source:
#' `iteration0_atc.xlsx`'s `total`/`seswoa`/`inkomen`/`opleiding`/`leeftijd`
#' sheets (see `src/02_atc_results_iteration0.R` in the pharm repo).
#' @return Tibble with columns `dim`, `subgroep`, `n`, `metric_group`
#'   (`"atc"`/`"poly"`), `klasse`, `prevalentie`.
load_atc_prevalentie <- function(path = PHARM_ATC_FILE) {
  sheet_by_dim <- c(totaal = "total", seswoa = "seswoa", inkomen = "inkomen",
                     opleiding = "opleiding", leeftijd = "leeftijd")

  rows <- lapply(names(sheet_by_dim), function(dim) {
    df <- pharm_read_sheet(path, sheet_by_dim[[dim]])
    names(df) <- sub("^prev_", "", names(df))
    df <- pharm_attach_subgroep(df, dim)

    metric_cols <- base::intersect(names(df), c(ATC_KLASSE_COLS, ATC_POLY_COLS))
    long <- tidyr::pivot_longer(
      df, cols = dplyr::all_of(metric_cols),
      names_to = "klasse", values_to = "prevalentie"
    )
    long$metric_group <- ifelse(long$klasse %in% ATC_KLASSE_COLS, "atc", "poly")
    long$dim <- dim
    long[, c("dim", "subgroep", "n", "metric_group", "klasse", "prevalentie")]
  })

  dplyr::bind_rows(rows)
}

#' Meest voorkomende combinaties van gelijktijdig gebruikte ATC-klassen
#' (`ATC_combinaties` sheet), aflopend gesorteerd op aantal personen. Rows
#' with a missing count in the source (fewer than a handful of combinations
#' occur at all) are dropped.
#' @return Tibble with columns `combi` (underscore-joined raw ATC codes) and
#'   `N` (number of personen), sorted descending by `N`.
load_atc_combinaties <- function(path = PHARM_ATC_FILE) {
  df <- pharm_read_sheet(path, "ATC_combinaties")
  df <- df[!is.na(df$N), , drop = FALSE]
  df[order(-df$N), , drop = FALSE]
}

#' Long-format GGZ-kosten en -gebruik, per uitsplitsing en GGZ-categorie
#' (basis-GGZ / specialistische GGZ zonder verblijf / met verblijf). Source:
#' `iteration0_ggz.xlsx`'s `totaal`/`ses`/`inkomen`/`opleiding`/`leeftijd`
#' sheets (see `src/02_ggz_results_iteration0.R` in the pharm repo).
#'
#' The source sheets only record the subgroup's 17+ population size
#' (`N_17+`) once per subgroup -- on its "bg" row, `NA` on the "sg_no_vd"/
#' "sg_met_vd" rows of the same subgroup -- since it doesn't vary by GGZ
#' category. `n_17plus` below is filled in across every category row of the
#' same `(dim, subgroep)` group, so a prevalence (`gebruikers / n_17plus *
#' 1000`) can be computed for every GGZ category, not just "bg".
#' @return Tibble with columns `dim`, `subgroep`, `categorie`,
#'   `totale_kosten`, `gebruikers`, `gem_kosten_per_gebr`, `n_17plus`.
load_ggz_kosten <- function(path = PHARM_GGZ_FILE) {
  sheet_by_dim <- c(totaal = "totaal", seswoa = "ses", inkomen = "inkomen",
                     opleiding = "opleiding", leeftijd = "leeftijd")

  rows <- lapply(names(sheet_by_dim), function(dim) {
    df <- pharm_read_sheet(path, sheet_by_dim[[dim]])
    df <- pharm_attach_subgroep(df, dim)
    df$dim <- dim
    df$n_17plus <- df[["N_17+"]]
    df[, c("dim", "subgroep", "categorie", "totale_kosten", "gebruikers", "gem_kosten_per_gebr", "n_17plus")]
  })

  combined <- dplyr::bind_rows(rows)
  group_key <- paste(combined$dim, combined$subgroep, sep = "")
  combined$n_17plus <- ave(combined$n_17plus, group_key, FUN = function(v) v[!is.na(v)][1])
  combined
}

#' Long-format prevalentie (per 1.000 personen) van GGZ-diagnosegroepen per
#' jaar (2016-2024), per uitsplitsing. Source: `prevalenties.xlsx`'s
#' `totaal`/`seswoa_cat`/`inkomen_klasse`/`hbopl`/`leeftijd_groep` sheets (see
#' `src/01_compute_ggz_prevalences.R` in the pharm repo). Vanaf 2022 is de
#' onderliggende registratie omgezet van Vektis-declaraties naar
#' ZPM-prestaties, wat een trendbreuk in vooral de categorie "onbekend" kan
#' verklaren -- zie ook `load_angst_subgroepen()`.
#' @return Tibble with columns `dim`, `jaar`, `subgroep`, `n_pop`,
#'   `diagnosegroep`, `prevalentie`.
load_prevalentie_diagnoses <- function(path = PHARM_PREV_FILE) {
  sheet_by_dim <- c(totaal = "totaal", seswoa = "seswoa_cat", inkomen = "inkomen_klasse",
                     opleiding = "hbopl", leeftijd = "leeftijd_groep")

  rows <- lapply(names(sheet_by_dim), function(dim) {
    df <- pharm_read_sheet(path, sheet_by_dim[[dim]])
    df <- pharm_attach_subgroep(df, dim)
    df$dim <- dim

    metric_cols <- base::intersect(names(df), DIAGNOSEGROEP_COLS)
    long <- tidyr::pivot_longer(
      df, cols = dplyr::all_of(metric_cols),
      names_to = "diagnosegroep", values_to = "prevalentie"
    )
    long[, c("dim", "jaar", "subgroep", "n_pop", "diagnosegroep", "prevalentie")]
  })

  dplyr::bind_rows(rows)
}

#' Long-format prevalentie (per 1.000 personen) van de drie ZPM-
#' subdiagnosegroepen (SG5/SG6/SG7) die samen de diagnosegroep
#' "angststoornis" vormen, per jaar (2022-2024) en uitsplitsing. Source:
#' `prevalenties_angst_22_24.xlsx`. Geen klinische omschrijving per
#' subgroep was beschikbaar in de brondata -- zie
#' `data/metadata/dictionary_seed.R`.
#' @return Tibble with columns `dim`, `jaar`, `subgroep`, `n_pop`,
#'   `sg_groep`, `prevalentie`.
load_angst_subgroepen <- function(path = PHARM_ANGST_FILE) {
  sheet_by_dim <- c(totaal = "totaal", seswoa = "seswoa_cat", inkomen = "inkomen_klasse",
                     opleiding = "hbopl", leeftijd = "leeftijd_groep")

  rows <- lapply(names(sheet_by_dim), function(dim) {
    df <- pharm_read_sheet(path, sheet_by_dim[[dim]])
    df <- pharm_attach_subgroep(df, dim)
    df$dim <- dim

    metric_cols <- base::intersect(names(df), SG_GROEP_COLS)
    long <- tidyr::pivot_longer(
      df, cols = dplyr::all_of(metric_cols),
      names_to = "sg_groep", values_to = "prevalentie"
    )
    long[, c("dim", "jaar", "subgroep", "n_pop", "sg_groep", "prevalentie")]
  })

  dplyr::bind_rows(rows)
}
