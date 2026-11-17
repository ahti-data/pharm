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

#' The "ses analysis" source workbooks: one per domain (ATC, GGZ-kosten,
#' psychische-prevalentie) per leeftijd/geslacht-uitsplitsing -- see
#' `load_atc_ses()`/`load_ggz_kosten_ses()`/`load_prevalentie_ses()`. Every
#' one of the 8 `SES_BREAKDOWN_VARS` lives in all four files of its domain,
#' one sheet each.
PHARM_ATC_SES_FILES <- list(
  geen              = file.path(PHARM_DATA_DIR, "atc_ses.xlsx"),
  leeftijd          = file.path(PHARM_DATA_DIR, "atc_ses_lft.xlsx"),
  geslacht          = file.path(PHARM_DATA_DIR, "atc_ses_gesl.xlsx"),
  leeftijd_geslacht = file.path(PHARM_DATA_DIR, "atc_ses_lft_gesl.xlsx")
)
PHARM_PREV_SES_FILES <- list(
  geen              = file.path(PHARM_DATA_DIR, "prev_ses.xlsx"),
  leeftijd          = file.path(PHARM_DATA_DIR, "prev_ses_lft.xlsx"),
  geslacht          = file.path(PHARM_DATA_DIR, "prev_ses_gesl.xlsx"),
  leeftijd_geslacht = file.path(PHARM_DATA_DIR, "prev_ses_lft_gesl.xlsx")
)
PHARM_GGZ_SES_FILES <- list(
  geen              = file.path(PHARM_DATA_DIR, "gebr_cost_ses.xlsx"),
  leeftijd          = file.path(PHARM_DATA_DIR, "gebr_cost_ses_lft.xlsx"),
  geslacht          = file.path(PHARM_DATA_DIR, "gebr_cost_ses_gesl.xlsx"),
  leeftijd_geslacht = file.path(PHARM_DATA_DIR, "gebr_cost_ses_lft_gesl.xlsx")
)
PHARM_TELLINGEN_FILE <- file.path(PHARM_DATA_DIR, "tellingen_algemeen_final.xlsx")

#' Sheetnaam per SES-variabele, per domein -- de sheetnamen lopen net uiteen
#' (bv. ATC's "seswoa_cat_moeder" vs GGZ's "seswoa_moeder"), maar de ruwe
#' kolomnaam ín elke sheet is altijd gelijk aan de bijbehorende
#' `DIM_COL_MAP`-waarde.
SES_SHEET_BY_VAR_ATC <- c(
  seswoa_cat5 = "seswoa_5", seswoa_cat6 = "seswoa_6", seswoa_cat10 = "seswoa_10",
  seswoa_buurt_cat5 = "seswoa_buurt_5", seswoa_buurt_cat6 = "seswoa_buurt_6",
  seswoa_buurt_cat10 = "seswoa_buurt_10",
  seswoa_cat_moeder = "seswoa_cat_moeder", opleiding_moeder = "opleiding_moeder"
)
SES_SHEET_BY_VAR_PREV <- c(
  seswoa_cat5 = "seswoa_cat5", seswoa_cat6 = "seswoa_cat6", seswoa_cat10 = "seswoa_cat10",
  seswoa_buurt_cat5 = "seswoa_buurt_cat5", seswoa_buurt_cat6 = "seswoa_buurt_cat6",
  seswoa_buurt_cat10 = "seswoa_buurt_cat10",
  seswoa_cat_moeder = "seswoa_cat_moeder", opleiding_moeder = "opleiding_moeder"
)
SES_SHEET_BY_VAR_GGZ <- c(
  seswoa_cat5 = "seswoa_5", seswoa_cat6 = "seswoa_6", seswoa_cat10 = "seswoa_10",
  seswoa_buurt_cat5 = "seswoa_buurt_5", seswoa_buurt_cat6 = "seswoa_buurt_6",
  seswoa_buurt_cat10 = "seswoa_buurt_10",
  seswoa_cat_moeder = "seswoa_moeder", opleiding_moeder = "opleiding_moeder"
)

#' Uitsplitsingen ("dimensies") beschikbaar op elk tabblad: de bevolking als
#' geheel, of uitgesplitst naar een van de demografische/SES-kenmerken die in
#' de bronbestanden terugkomen.
#'
#' `seswoa_old6` is de oorspronkelijke SES-WOA-uitsplitsing (6-delig, uit
#' `iteration0_atc.xlsx`/`iteration0_ggz.xlsx`/`prevalenties.xlsx`) -- zo
#' genoemd om hem te onderscheiden van de nieuwe `seswoa_cat6` hieronder, die
#' dezelfde percentielgrenzen gebruikt maar op een andere (op tientallen
#' afgeronde) manier is berekend en dus net andere getallen geeft. De 8
#' `*_ses`-dimensies erna komen uit de losse "ses analysis"-bronbestanden
#' (zie `load_atc_ses()`/`load_prevalentie_ses()`/`load_ggz_kosten_ses()`) en
#' bieden fijnere/andere SES-varianten: SES-WOA op persoons- vs buurtniveau,
#' in 5/6/10-delige percentielgroepen, en SES-WOA/opleidingsniveau van de
#' moeder. Elke van deze 8 ondersteunt een optionele extra uitsplitsing naar
#' leeftijd en/of geslacht (zie `*_SES_VARS` en de "verder uitsplitsen"-UI in
#' app.R) -- `seswoa_old6` en de andere oorspronkelijke dimensies niet.
DIM_CHOICES <- c(
  "Totale bevolking"              = "totaal",
  "SES-WOA (oorspronkelijke indeling)" = "seswoa_old6",
  "Inkomensklasse"                 = "inkomen",
  "Opleidingsniveau"                = "opleiding",
  "Leeftijdsgroep"                   = "leeftijd",
  "SES-WOA (persoon, 5-delig)"        = "seswoa_cat5",
  "SES-WOA (persoon, 6-delig)"         = "seswoa_cat6",
  "SES-WOA (persoon, 10-delig)"         = "seswoa_cat10",
  "SES-WOA (buurt, 5-delig)"              = "seswoa_buurt_cat5",
  "SES-WOA (buurt, 6-delig)"               = "seswoa_buurt_cat6",
  "SES-WOA (buurt, 10-delig)"               = "seswoa_buurt_cat10",
  "SES-WOA moeder"                            = "seswoa_cat_moeder",
  "Opleidingsniveau moeder"                    = "opleiding_moeder"
)

#' Welke `DIM_CHOICES`-waarden de 8 nieuwe "ses analysis"-variabelen zijn --
#' alleen díe ondersteunen de optionele extra uitsplitsing naar leeftijd/
#' geslacht (zie `*_data_for()`/de "verder uitsplitsen"-UI in app.R) en
#' komen uit `load_atc_ses()`/`load_prevalentie_ses()`/`load_ggz_kosten_ses()`
#' in plaats van de oorspronkelijke per-domein data.
SES_BREAKDOWN_VARS <- c(
  "seswoa_cat5", "seswoa_cat6", "seswoa_cat10",
  "seswoa_buurt_cat5", "seswoa_buurt_cat6", "seswoa_buurt_cat10",
  "seswoa_cat_moeder", "opleiding_moeder"
)

#' Raw column name holding the subgroup value for each non-"totaal" dimension.
DIM_COL_MAP <- c(
  seswoa_old6        = "seswoa_cat",
  inkomen            = "inkomen_klasse",
  opleiding          = "hbopl",
  leeftijd           = "leeftijd_groep",
  seswoa_cat5        = "seswoa_cat5",
  seswoa_cat6        = "seswoa_cat6",
  seswoa_cat10       = "seswoa_cat10",
  seswoa_buurt_cat5  = "seswoa_buurt_cat5",
  seswoa_buurt_cat6  = "seswoa_buurt_cat6",
  seswoa_buurt_cat10 = "seswoa_buurt_cat10",
  seswoa_cat_moeder  = "seswoa_cat_moeder",
  opleiding_moeder   = "opleiding_moeder"
)

#' Dictionary `scope` matching each dimension's raw column (see
#' `data/metadata/dictionary_seed.R`) -- used both for on-screen relabeling
#' and for `chart_data_downloads_server()`'s `category_scope`/`series_scope`.
#'
#' `inkomen` deliberately uses the scope "inkomen_klasse_pct", not the
#' column name "inkomen_klasse" -- a live dashboard's `state/dictionary.json`
#' only ever *adds* seed entries missing by `(raw_key, scope)`, it never
#' overwrites an existing one (see `dictionary_fill_missing_seed()` in
#' `utils/dictionary.R`). The original seed had "WML" in every
#' inkomen_klasse label; once that had been written to a deployed
#' `state/dictionary.json`, changing only the label text in
#' `dictionary_seed.R` would never reach an already-running dashboard. A
#' new scope name is a clean (raw_key, scope) pair the stored file has
#' never seen, so it always seeds fresh. For the same reason, every new SES
#' dimension below gets its own scope even where its category labels happen
#' to coincide with an existing one (e.g. `seswoa_cat6` vs `seswoa_old6`'s
#' `seswoa_cat`) -- they're different underlying variables.
DIM_SCOPE_MAP <- c(
  seswoa_old6        = "seswoa_cat",
  inkomen            = "inkomen_klasse_pct",
  opleiding          = "hbopl",
  leeftijd           = "leeftijd_groep",
  totaal             = "subgroep_totaal",
  seswoa_cat5        = "seswoa_cat5",
  seswoa_cat6        = "seswoa_cat6",
  seswoa_cat10       = "seswoa_cat10",
  seswoa_buurt_cat5  = "seswoa_buurt_cat5",
  seswoa_buurt_cat6  = "seswoa_buurt_cat6",
  seswoa_buurt_cat10 = "seswoa_buurt_cat10",
  seswoa_cat_moeder  = "seswoa_cat_moeder",
  opleiding_moeder   = "opleiding_moeder_cat"
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

SESWOA_OLD6_ORDER <- c("0-10%", "10-20%", "20-35%", "35-50%", "50-75%", "75-100%", "Onbekend")
INKOMEN_ORDER   <- c("tot_120", "120_160", "160_200", "200_240", "240_280", "280_400", "400+",
                      "student", "Onbekend_institutioneel")
OPLEIDING_ORDER <- c("basisonderwijs, vmbo, mbo1", "havo, vwo, mbo2-4", "hbo, wo", "onbekend")
LEEFTIJD_ORDER_FULL    <- c("0-17", "18-29", "30-49", "50-65", "66-75", "76-85", "86+")
LEEFTIJD_ORDER_17PLUS  <- c("17-29", "30-49", "50-65", "66-75", "76-85", "86+")

# De 8 nieuwe "ses analysis"-variabelen (zie SES_BREAKDOWN_VARS hierboven).
# seswoa_cat6/buurt_cat6 delen dezelfde percentielgrenzen als SESWOA_OLD6_ORDER
# (zelfde indeling, net anders berekend -- zie DIM_CHOICES's eigen doc-comment),
# maar krijgen bewust hun eigen constante, niet een hergebruikte verwijzing,
# zodat de twee onafhankelijk kunnen worden aangepast.
SESWOA_CAT5_ORDER  <- c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%", "Onbekend")
SESWOA_CAT6_ORDER  <- c("0-10%", "10-20%", "20-35%", "35-50%", "50-75%", "75-100%", "Onbekend")
SESWOA_CAT10_ORDER <- c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%",
                         "60-70%", "70-80%", "80-90%", "90-100%", "Onbekend")
SESWOA_BUURT_CAT5_ORDER  <- SESWOA_CAT5_ORDER
SESWOA_BUURT_CAT6_ORDER  <- SESWOA_CAT6_ORDER
SESWOA_BUURT_CAT10_ORDER <- SESWOA_CAT10_ORDER
# "geen_waarde_bekend" (moeder zelf onbekend) staat na "Onbekend" (moeder
# bekend, haar SES-WOA-waarde niet) -- beide ontbrekend, maar om een andere
# reden, dus apart gehouden in plaats van samengevoegd.
SESWOA_CAT_MOEDER_ORDER  <- c("0-10%", "10-20%", "20-35%", "35-50%", "50-75%", "75-100%",
                               "Onbekend", "geen_waarde_bekend")
OPLEIDING_MOEDER_ORDER   <- c("basisonderwijs, vmbo, mbo1", "havo, vwo, mbo2-4", "hbo, wo",
                               "onbekend", "geen_waarde_bekend")

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

#' Order constant for one of `names(DIM_CHOICES)`'s values -- shared by
#' [pharm_order_subgroep()] and [pharm_dim_categories()] so the two can never
#' silently drift apart.
#' @param dim One of `names(DIM_CHOICES)`'s values.
#' @param leeftijd_order Age-band order to use for `dim == "leeftijd"`.
pharm_dim_order <- function(dim, leeftijd_order = LEEFTIJD_ORDER_17PLUS) {
  switch(dim,
    seswoa_old6        = SESWOA_OLD6_ORDER,
    inkomen            = INKOMEN_ORDER,
    opleiding          = OPLEIDING_ORDER,
    leeftijd           = leeftijd_order,
    seswoa_cat5        = SESWOA_CAT5_ORDER,
    seswoa_cat6        = SESWOA_CAT6_ORDER,
    seswoa_cat10       = SESWOA_CAT10_ORDER,
    seswoa_buurt_cat5  = SESWOA_BUURT_CAT5_ORDER,
    seswoa_buurt_cat6  = SESWOA_BUURT_CAT6_ORDER,
    seswoa_buurt_cat10 = SESWOA_BUURT_CAT10_ORDER,
    seswoa_cat_moeder  = SESWOA_CAT_MOEDER_ORDER,
    opleiding_moeder   = OPLEIDING_MOEDER_ORDER,
    character(0)
  )
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
  order <- pharm_dim_order(dim, leeftijd_order)
  if (length(order) == 0) order <- "Totaal"
  extra <- base::setdiff(unique(x), order)
  factor(x, levels = c(order, extra))
}

#' Raw subgroup category codes available for a non-"totaal" dimension, in
#' the same order [pharm_order_subgroep()] plots them in -- used to build a
#' per-dimension category picker in app.R (e.g. deselect "student" within
#' inkomensklasse without leaving the dimension entirely).
#' @param dim One of `names(DIM_CHOICES)`'s values, excluding `"totaal"`.
#' @param leeftijd_order Age-band order to use; see [pharm_order_subgroep()].
pharm_dim_categories <- function(dim, leeftijd_order = LEEFTIJD_ORDER_17PLUS) {
  pharm_dim_order(dim, leeftijd_order)
}

#' UI for the optional "verder uitsplitsen naar leeftijd en/of geslacht"
#' picker, shown only while the chart's "Uitsplitsing naar" is one of
#' `SES_BREAKDOWN_VARS` (see `ses_breakdown_for()`). Two plain `selectInput`s
#' rather than checkboxes, so "neither" is always an unambiguous, valid
#' choice ("Alle leeftijden"/"Beide geslachten") instead of a state the UI
#' has to forbid.
#' @param id_leeftijd,id_geslacht Input ids for the two selects.
#' @param leeftijd_order Age-band order to offer (`LEEFTIJD_ORDER_FULL` for
#'   ATC/tellingen, `LEEFTIJD_ORDER_17PLUS` for GGZ-kosten/prevalentie -- see
#'   each domain's own `load_*_ses()`).
ses_breakdown_picker_ui <- function(id_leeftijd, id_geslacht, leeftijd_order) {
  leeftijd_choices <- stats::setNames(
    leeftijd_order, as.character(pharm_relabel_factor(leeftijd_order, scope = "leeftijd_groep"))
  )
  shiny::tagList(
    shiny::selectInput(id_leeftijd, "Leeftijd", choices = c("Alle leeftijden" = "", leeftijd_choices)),
    shiny::selectInput(id_geslacht, "Geslacht", choices = c("Beide geslachten" = "", "Mannen" = "Mannen", "Vrouwen" = "Vrouwen"))
  )
}

#' Which of `load_atc_ses()`/`load_prevalentie_ses()`/`load_ggz_kosten_ses()`/
#' `load_tellingen_algemeen()`'s four `breakdown` values to use, given the
#' "verder uitsplitsen"-picker's current leeftijd/geslacht selections (`""`
#' meaning "not chosen", as `ses_breakdown_picker_ui()`'s own "Alle
#' leeftijden"/"Beide geslachten" options send).
#' @param leeftijd_val,geslacht_val The two selects' current values.
ses_breakdown_for <- function(leeftijd_val, geslacht_val) {
  has_leeftijd <- nzchar(tc_or(leeftijd_val, ""))
  has_geslacht <- nzchar(tc_or(geslacht_val, ""))
  if (has_leeftijd && has_geslacht) "leeftijd_geslacht"
  else if (has_leeftijd) "leeftijd"
  else if (has_geslacht) "geslacht"
  else "geen"
}

#' Filter one domain's `load_*_ses()`/`load_tellingen_algemeen()` output down
#' to one SES-variabele, the breakdown [ses_breakdown_for()] resolves to, and
#' (when chosen) one specific leeftijd/geslacht value.
#' @param df One of those functions' own output (needs `dim`, `breakdown`,
#'   `leeftijd_groep`, `geslacht` columns).
#' @param dim SES-variabele (one of `SES_BREAKDOWN_VARS`).
#' @param leeftijd_val,geslacht_val The picker's current values (`""` = not
#'   chosen -- see [ses_breakdown_for()]).
ses_filter <- function(df, dim, leeftijd_val, geslacht_val) {
  breakdown <- ses_breakdown_for(leeftijd_val, geslacht_val)
  out <- df[df$dim == dim & df$breakdown == breakdown, ]
  if (nzchar(tc_or(leeftijd_val, ""))) out <- out[out$leeftijd_groep == leeftijd_val, ]
  if (nzchar(tc_or(geslacht_val, ""))) out <- out[out$geslacht == geslacht_val, ]
  out
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
  sheet_by_dim <- c(totaal = "total", seswoa_old6 = "seswoa", inkomen = "inkomen",
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
  sheet_by_dim <- c(totaal = "totaal", seswoa_old6 = "ses", inkomen = "inkomen",
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
  sheet_by_dim <- c(totaal = "totaal", seswoa_old6 = "seswoa_cat", inkomen = "inkomen_klasse",
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
  sheet_by_dim <- c(totaal = "totaal", seswoa_old6 = "seswoa_cat", inkomen = "inkomen_klasse",
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

#' Apply the pharm repo's own CBS afrondings-/onderdrukkingsstap
#' (`onderdruk()`/`onderdruk_gebruikers()` in its `src/ses_analysis_make_output.R`,
#' which the uploaded `atc_ses.xlsx`/`prev_ses.xlsx` hadn't been run through
#' yet, unlike their leeftijd/geslacht-uitgesplitste tegenhangers): round
#' `n_col` to the nearest ten, recompute every prevalence column in
#' `prev_cols` from rounded counts, and suppress (`NA`) any cell whose real
#' underlying count would fall below `drempel`. A no-op to call again on data
#' that already went through this step (every cell is already `>= drempel`
#' and a multiple of ten in `n_col`, so nothing here would change it) -- but
#' it is only actually called on the two sheets that needed it (see
#' [load_atc_ses()]/[load_prevalentie_ses()]), to avoid any numerical drift
#' from recomputing an already-rounded prevalence a second time.
#' @param df Data frame with a count column `n_col` and one or more
#'   prevalence-per-`per` columns in `prev_cols`.
pharm_onderdruk <- function(df, n_col, prev_cols, drempel = 10, per = 1000) {
  below <- which(df[[n_col]] < drempel)
  if (length(below) > 0) df[below, c(n_col, prev_cols)] <- NA_real_
  for (p in prev_cols) {
    tel <- round(df[[n_col]] * df[[p]] / per)
    df[[p]] <- round(tel, -1) / round(df[[n_col]], -1) * per
    suppress <- which(tel < drempel)
    if (length(suppress) > 0) df[[p]][suppress] <- NA_real_
  }
  df[[n_col]] <- round(df[[n_col]], -1)
  df
}

#' Shared row-builder for [load_atc_ses()]/[load_prevalentie_ses()]/
#' [load_ggz_kosten_ses()]: attach `subgroep`/`dim`/`breakdown` to one
#' already-read SES-variabele sheet, and fill in `leeftijd_groep`/`geslacht`
#' as `NA` when this breakdown file doesn't carry them (so every breakdown's
#' rows bind into one consistently-shaped tibble).
#' @param raw Already-read sheet (see [pharm_read_sheet()]).
#' @param dim SES-variabele (one of `SES_BREAKDOWN_VARS`).
#' @param breakdown One of `names(PHARM_ATC_SES_FILES)` (`"geen"`/
#'   `"leeftijd"`/`"geslacht"`/`"leeftijd_geslacht"`).
pharm_read_ses_sheet_from_df <- function(raw, dim, breakdown) {
  df <- pharm_attach_subgroep(raw, dim)
  df$dim <- dim
  df$breakdown <- breakdown
  if (!("leeftijd_groep" %in% names(df))) df$leeftijd_groep <- NA_character_
  if (!("geslacht" %in% names(df))) df$geslacht <- NA_character_
  df
}

#' Same as [pharm_read_ses_sheet_from_df()], reading the sheet itself first.
#' @param path One breakdown file's path.
#' @param sheet Sheet name for this SES-variabele in this file.
#' @inheritParams pharm_read_ses_sheet_from_df
pharm_read_ses_sheet <- function(path, sheet, dim, breakdown) {
  pharm_read_ses_sheet_from_df(pharm_read_sheet(path, sheet), dim, breakdown)
}

#' Long-format prevalentie (per 1.000 personen) van psychofarmacagebruik per
#' SES-variabele (zie `SES_BREAKDOWN_VARS`), optioneel verder uitgesplitst
#' naar leeftijd en/of geslacht. Combineert de vier "ses analysis"-
#' bronbestanden (één per leeftijd/geslacht-combinatie, zie
#' `PHARM_ATC_SES_FILES`) tot één long tibble met een extra `breakdown`-kolom.
#' @return Tibble met kolommen `breakdown`, `dim`, `subgroep`,
#'   `leeftijd_groep`, `geslacht`, `n`, `metric_group`, `klasse`,
#'   `prevalentie`.
load_atc_ses <- function(files = PHARM_ATC_SES_FILES) {
  rows <- lapply(names(files), function(breakdown) {
    path <- files[[breakdown]]
    sub_rows <- lapply(names(SES_SHEET_BY_VAR_ATC), function(dim) {
      raw <- pharm_read_sheet(path, SES_SHEET_BY_VAR_ATC[[dim]])
      names(raw) <- sub("^prev_", "", names(raw))
      metric_cols <- base::intersect(names(raw), c(ATC_KLASSE_COLS, ATC_POLY_COLS))
      if (identical(breakdown, "geen")) raw <- pharm_onderdruk(raw, "n", metric_cols)

      df <- pharm_read_ses_sheet_from_df(raw, dim, breakdown)
      long <- tidyr::pivot_longer(
        df, cols = dplyr::all_of(metric_cols),
        names_to = "klasse", values_to = "prevalentie"
      )
      long$metric_group <- ifelse(long$klasse %in% ATC_KLASSE_COLS, "atc", "poly")
      long[, c("breakdown", "dim", "subgroep", "leeftijd_groep", "geslacht", "n", "metric_group", "klasse", "prevalentie")]
    })
    dplyr::bind_rows(sub_rows)
  })

  dplyr::bind_rows(rows)
}

#' Long-format prevalentie (per 1.000 personen) van de 12 psychische-
#' diagnosegroepen per SES-variabele (zie `SES_BREAKDOWN_VARS`), optioneel
#' verder uitgesplitst naar leeftijd en/of geslacht. Combineert de vier
#' "ses analysis"-bronbestanden (zie `PHARM_PREV_SES_FILES`) tot één long
#' tibble met een extra `breakdown`-kolom. Alle sheets zijn één jaar (2024) --
#' anders dan `load_prevalentie_diagnoses()` is er dus geen `jaar`-reeks om
#' over te plotten.
#' @return Tibble met kolommen `breakdown`, `dim`, `subgroep`,
#'   `leeftijd_groep`, `geslacht`, `n_pop`, `diagnosegroep`, `prevalentie`.
load_prevalentie_ses <- function(files = PHARM_PREV_SES_FILES) {
  rows <- lapply(names(files), function(breakdown) {
    path <- files[[breakdown]]
    sub_rows <- lapply(names(SES_SHEET_BY_VAR_PREV), function(dim) {
      raw <- pharm_read_sheet(path, SES_SHEET_BY_VAR_PREV[[dim]])
      metric_cols <- base::intersect(names(raw), DIAGNOSEGROEP_COLS)
      if (identical(breakdown, "geen")) raw <- pharm_onderdruk(raw, "n_pop", metric_cols)

      df <- pharm_read_ses_sheet_from_df(raw, dim, breakdown)
      long <- tidyr::pivot_longer(
        df, cols = dplyr::all_of(metric_cols),
        names_to = "diagnosegroep", values_to = "prevalentie"
      )
      long[, c("breakdown", "dim", "subgroep", "leeftijd_groep", "geslacht", "n_pop", "diagnosegroep", "prevalentie")]
    })
    dplyr::bind_rows(sub_rows)
  })

  dplyr::bind_rows(rows)
}

#' Long-format GGZ-kosten en -gebruik per SES-variabele (zie
#' `SES_BREAKDOWN_VARS`), optioneel verder uitgesplitst naar leeftijd en/of
#' geslacht. Combineert de vier "ses analysis"-bronbestanden (zie
#' `PHARM_GGZ_SES_FILES`) tot één long tibble met een extra `breakdown`-kolom.
#' Anders dan [load_atc_ses()]/[load_prevalentie_ses()] zijn alle vier
#' bronbestanden hier al door de eigen afrondings-/onderdrukkingsstap
#' gehaald (ook de "geen"-variant), dus [pharm_onderdruk()] is hier niet nodig.
#' Bevat geen populatie-noemer (zoals `n_17plus` in `load_ggz_kosten()`) --
#' de brondata heeft die hier niet, dus een "prevalentie"-maatstaf is voor
#' deze uitsplitsing niet beschikbaar (alleen `totale_kosten`/`gebruikers`/
#' `gem_kosten_per_gebr`).
#' @return Tibble met kolommen `breakdown`, `dim`, `subgroep`,
#'   `leeftijd_groep`, `geslacht`, `categorie`, `totale_kosten`, `gebruikers`,
#'   `gem_kosten_per_gebr`.
load_ggz_kosten_ses <- function(files = PHARM_GGZ_SES_FILES) {
  rows <- lapply(names(files), function(breakdown) {
    path <- files[[breakdown]]
    sub_rows <- lapply(names(SES_SHEET_BY_VAR_GGZ), function(dim) {
      df <- pharm_read_ses_sheet(path, SES_SHEET_BY_VAR_GGZ[[dim]], dim, breakdown)
      df[, c("breakdown", "dim", "subgroep", "leeftijd_groep", "geslacht",
             "categorie", "totale_kosten", "gebruikers", "gem_kosten_per_gebr")]
    })
    dplyr::bind_rows(sub_rows)
  })

  dplyr::bind_rows(rows)
}

#' Long-format bevolkingstellingen (N) per SES-variabele en -categorie (zie
#' `SES_BREAKDOWN_VARS`), optioneel verder uitgesplitst naar leeftijd en/of
#' geslacht. Source: `tellingen_algemeen_final.xlsx`'s vier sheets (één per
#' leeftijd/geslacht-combinatie) -- elk al in lang formaat (`variabele`/
#' `categorie`/`N`), dus hier alleen samengevoegd tot één tibble met een
#' `breakdown`-kolom en een hernoemde `leeftijd_groep` (de brondata noemt hem
#' `leeftijd_groep_ibo`).
#' @return Tibble met kolommen `breakdown`, `dim`, `subgroep`,
#'   `leeftijd_groep`, `geslacht`, `n`.
load_tellingen_algemeen <- function(path = PHARM_TELLINGEN_FILE) {
  sheet_by_breakdown <- c(
    geen = "seswoa", leeftijd = "seswoa_leeftijd",
    geslacht = "seswoa_geslacht", leeftijd_geslacht = "seswoa_leeftijd_geslacht"
  )

  rows <- lapply(names(sheet_by_breakdown), function(breakdown) {
    df <- pharm_read_sheet(path, sheet_by_breakdown[[breakdown]])
    if ("leeftijd_groep_ibo" %in% names(df)) {
      names(df)[names(df) == "leeftijd_groep_ibo"] <- "leeftijd_groep"
    }
    if (!("leeftijd_groep" %in% names(df))) df$leeftijd_groep <- NA_character_
    if (!("geslacht" %in% names(df))) df$geslacht <- NA_character_
    df$breakdown <- breakdown
    df$dim <- df$variabele
    df$subgroep <- as.character(df$categorie)
    df$n <- df$N
    df[, c("breakdown", "dim", "subgroep", "leeftijd_groep", "geslacht", "n")]
  })

  dplyr::bind_rows(rows)
}
