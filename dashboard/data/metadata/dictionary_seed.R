#' Dictionary seed entries for the pharm/GGZ dashboard -- overrides the
#' template's empty default (see `utils/dictionary.R`) with pretty Dutch
#' labels for every raw code that appears in `data/iteration0_atc.xlsx`,
#' `data/iteration0_ggz.xlsx`, `data/prevalenties.xlsx` and
#' `data/prevalenties_angst_22_24.xlsx` (see `utils/pharm_data.R` for how
#' these raw values reach the charts). Sourced from `app.R` right after
#' `utils/dictionary.R`, so this definition is the one every later
#' `dictionary_seed_entries()` call in the running app resolves to (see
#' CLAUDE.md's note on overriding a utils/-defined hook from a separately
#' sourced file).
#'
#' A user edit made from the Dictionary tab always wins over any entry here
#' (see `dictionary_set_entry()`), and any label not covered here (e.g. a
#' raw SES-WOA/inkomensklasse/leeftijdsgroep band) simply passes through
#' unchanged on screen and in every export -- the dashboard's own charts call
#' `dictionary_relabel(..., fallback = function(x) x)` rather than the
#' generic title-caser default, specifically so a code like "76-85" or
#' "0-10%" is never mangled into "76 85"/"0 10%" by
#' `dictionary_default_prettify()`.
dictionary_seed_entries <- function() {
  entry <- function(raw_key, scope, pretty_label) {
    list(raw_key = raw_key, scope = scope, pretty_label = pretty_label)
  }

  klasse <- list(
    entry("N05A", "klasse", "Antipsychotica (N05A)"),
    entry("N05B", "klasse", "Anxiolytica (N05B)"),
    entry("N05C", "klasse", "Hypnotica en sedativa (N05C)"),
    entry("N06A", "klasse", "Antidepressiva (N06A)"),
    entry("N06B", "klasse", "Psychostimulantia / ADHD-middelen (N06B)"),
    entry("N06D", "klasse", "Anti-dementiemiddelen (N06D)"),
    entry("monofarmacie", "klasse", "Monofarmacie (1 middel)"),
    entry("polyfarmacie_2", "klasse", "Polyfarmacie (2 middelen)"),
    entry("polyfarmacie_3", "klasse", "Polyfarmacie (3 middelen)"),
    entry("polyfarmacie_4plus", "klasse", "Polyfarmacie (4+ middelen)")
  )

  categorie <- list(
    entry("bg", "categorie", "Basis-GGZ"),
    entry("sg_no_vd", "categorie", "Specialistische GGZ (ambulant)"),
    entry("sg_met_vd", "categorie", "Specialistische GGZ (met verblijf)")
  )

  diagnosegroep <- list(
    entry("schizofrenie", "diagnosegroep", "Schizofrenie (en psychotische stoornissen)"),
    entry("depr_stemming_stoornis", "diagnosegroep", "Depressieve stemmingsstoornis"),
    entry("bipolaire_stemming_stoornis", "diagnosegroep", "Bipolaire stemmingsstoornis"),
    entry("angststoornis", "diagnosegroep", "Angststoornis"),
    entry("persoonlijkheidstoornis", "diagnosegroep", "Persoonlijkheidsstoornis"),
    entry("middelger_versl_stoornis", "diagnosegroep", "Middelengerelateerde verslavingsstoornis"),
    entry("neurobio_ontw_stoornis", "diagnosegroep", "Neurobiologische ontwikkelingsstoornis"),
    entry("neurocog_stoornis", "diagnosegroep", "Neurocognitieve stoornis"),
    entry("som_symp_stoornis", "diagnosegroep", "Somatisch-symptoomstoornis"),
    entry("voeding_eet_stoornis", "diagnosegroep", "Voedings- en eetstoornis"),
    entry("restgroep", "diagnosegroep", "Restgroep (overige diagnoses)"),
    entry("onbekend", "diagnosegroep", "Onbekend / geen diagnose geregistreerd")
  )

  sg_groep <- list(
    entry("SG5", "sg_groep", "Angststoornis - subgroep SG5"),
    entry("SG6", "sg_groep", "Angststoornis - subgroep SG6"),
    entry("SG7", "sg_groep", "Angststoornis - subgroep SG7")
  )

  # Harmonized `hbopl` codes (see `harmonize_hbopl()` in `utils/pharm_data.R`)
  # -- `prevalenties.xlsx`/`prevalenties_angst_22_24.xlsx` use "lager" /
  # "middelbaar" / "hoger" / "onbekend" for 2016-2023 and the longer codes
  # below from 2024 onward; `harmonize_hbopl()` recodes the old labels to
  # these same longer codes so one dictionary entry (and one chart series)
  # covers both.
  hbopl <- list(
    entry("basisonderwijs, vmbo, mbo1", "hbopl", "Basisonderwijs, vmbo, mbo1"),
    entry("havo, vwo, mbo2-4", "hbopl", "Havo, vwo, mbo2-4"),
    entry("hbo, wo", "hbopl", "Hbo, wo"),
    entry("onbekend", "hbopl", "Onbekend")
  )

  inkomen_klasse <- list(
    entry("tot_120", "inkomen_klasse", "< 120% WML"),
    entry("120_160", "inkomen_klasse", "120–160% WML"),
    entry("160_200", "inkomen_klasse", "160–200% WML"),
    entry("200_240", "inkomen_klasse", "200–240% WML"),
    entry("240_280", "inkomen_klasse", "240–280% WML"),
    entry("280_400", "inkomen_klasse", "280–400% WML"),
    entry("400+", "inkomen_klasse", "≥ 400% WML"),
    entry("student", "inkomen_klasse", "Student"),
    entry("Onbekend_institutioneel", "inkomen_klasse", "Onbekend / institutioneel")
  )

  seswoa_cat <- list(
    entry("0-10%", "seswoa_cat", "0–10% (laagste SES-WOA)"),
    entry("10-20%", "seswoa_cat", "10–20%"),
    entry("20-35%", "seswoa_cat", "20–35%"),
    entry("35-50%", "seswoa_cat", "35–50%"),
    entry("50-75%", "seswoa_cat", "50–75%"),
    entry("75-100%", "seswoa_cat", "75–100% (hoogste SES-WOA)"),
    entry("Onbekend", "seswoa_cat", "Onbekend")
  )

  leeftijd_groep <- list(
    entry("0-17", "leeftijd_groep", "0–17 jaar"),
    entry("17-29", "leeftijd_groep", "17–29 jaar"),
    entry("18-29", "leeftijd_groep", "18–29 jaar"),
    entry("30-49", "leeftijd_groep", "30–49 jaar"),
    entry("50-65", "leeftijd_groep", "50–65 jaar"),
    entry("66-75", "leeftijd_groep", "66–75 jaar"),
    entry("76-85", "leeftijd_groep", "76–85 jaar"),
    entry("86+", "leeftijd_groep", "86 jaar en ouder")
  )

  c(klasse, categorie, diagnosegroep, sg_groep, hbopl, inkomen_klasse, seswoa_cat, leeftijd_groep)
}
