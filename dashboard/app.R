# Dashboard: psychofarmacagebruik en GGZ-zorggebruik (iteratie 0)
#
# Toont resultaten van de pharm-pipeline (zie het repo-brede README.md), in
# vier tabbladen:
#   1. Medicatiegebruik (ATC)            -- prevalentie van gebruik van
#      psychofarmaca (antipsychotica, anxiolytica, hypnotica/sedativa,
#      antidepressiva, ADHD-middelen, anti-dementiemiddelen) en van mono-/
#      polyfarmacie, plus de meest voorkomende combinaties van klassen.
#   2. GGZ zorggebruik & kosten          -- totale kosten, aantal gebruikers
#      en gemiddelde kosten per gebruiker voor basis-GGZ en specialistische
#      GGZ (met/zonder verblijf), jaar 2024.
#   3. Prevalentie psychische aandoeningen -- prevalentie van 12 psychische
#      diagnosegroepen per jaar (2016-2024), met een apart tabblad voor de
#      uitsplitsing van angststoornis naar ZPM-subgroep (2022-2024).
# Elk tabblad is uit te splitsen naar de totale bevolking of naar SES-WOA,
# inkomensklasse, opleidingsniveau of leeftijdsgroep. Alle cijfers zijn al
# geaggregeerd binnen de beveiligde CBS Remote Access-omgeving -- dit
# dashboard leest uitsluitend de niet-herleidbare Excel-outputs in data/.

source("data/metadata/brand_colors.R")
source("utils/format_thinkcell_download.R")
source("utils/slide_download.R")
source("utils/template_admin.R")
source("utils/favorites.R")
source("utils/export_history.R")
source("utils/chart_downloads.R")
source("utils/dictionary.R")
source("data/metadata/dictionary_seed.R")
source("utils/dictionary_admin.R")
source("utils/tab_theme.R")
source("utils/pharm_data.R")

library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)

DASHBOARD_TITLE <- "ahti — Psychofarmacagebruik en GGZ-zorggebruik (iteratie 0)"

# Static data, read once at app startup -- every source workbook is a small,
# already-aggregated iteration-0 output (see utils/pharm_data.R), not
# per-session state, so there is nothing to gain from re-reading it per
# reactive.
ATC_PREVALENTIE_DATA <- load_atc_prevalentie()
ATC_COMBINATIES_DATA <- load_atc_combinaties()
GGZ_KOSTEN_DATA       <- load_ggz_kosten()
PREV_DIAGNOSE_DATA    <- load_prevalentie_diagnoses()
ANGST_DATA            <- load_angst_subgroepen()

pharm_dim_label <- function(dim) {
  names(DIM_CHOICES)[DIM_CHOICES == dim]
}

pharm_pretty <- function(raw_key, scope) {
  dictionary_pretty(raw_key, scope = scope, fallback = function(v) v)
}

ATC_KLASSE_CHOICES <- setNames(ATC_KLASSE_COLS, vapply(ATC_KLASSE_COLS, pharm_pretty, character(1), scope = "klasse"))
ATC_POLY_CHOICES   <- setNames(ATC_POLY_COLS, vapply(ATC_POLY_COLS, pharm_pretty, character(1), scope = "klasse"))
DIAGNOSEGROEP_CHOICES <- setNames(
  DIAGNOSEGROEP_COLS,
  vapply(DIAGNOSEGROEP_COLS, pharm_pretty, character(1), scope = "diagnosegroep")
)
DIAGNOSEGROEP_DEFAULT <- c("depr_stemming_stoornis", "angststoornis", "persoonlijkheidstoornis", "middelger_versl_stoornis")
SG_GROEP_CHOICES <- setNames(SG_GROEP_COLS, vapply(SG_GROEP_COLS, pharm_pretty, character(1), scope = "sg_groep"))

ui <- fluidPage(
  tc_tab_color_theme(ahti_branding),
  titlePanel("Psychofarmacagebruik en GGZ-zorggebruik — ahti dashboard"),
  p(
    "Dit dashboard toont resultaten van iteratie 0 van het onderzoek van ",
    "het Amsterdam health & technology institute (ahti) naar medicatiegebruik ",
    "en GGZ-zorggebruik bij psychische aandoeningen, op basis van CBS-microdata. ",
    "Alle cijfers zijn geaggregeerd en niet tot personen herleidbaar."
  ),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      "Medicatiegebruik (ATC)",
      tabsetPanel(
        id = "atc_subtabs",
        tabPanel(
          "Prevalentie per klasse",
          br(),
          sidebarLayout(
            sidebarPanel(
              selectInput("atc_dim", "Uitsplitsing naar", choices = DIM_CHOICES, selected = "totaal"),
              radioButtons(
                "atc_metric_group", "Indicator",
                choices = c("Medicatieklasse (ATC)" = "atc", "Mono-/polyfarmacie" = "poly")
              ),
              uiOutput("atc_klasse_picker"),
              helpText(
                "Prevalentie van gebruik van psychofarmaca (N05A-N06D) per 1.000 personen, ",
                "en van mono- versus polyfarmacie (gelijktijdig gebruik van 1, 2, 3 of 4+ klassen)."
              )
            ),
            mainPanel(
              plotOutput("atc_plot"),
              h4("Data in de grafiek"),
              tableOutput("atc_table"),
              br(),
              chart_data_downloads_ui("atc_prevalentie_dl", chart_type = "grouped_bar")
            )
          )
        ),
        tabPanel(
          "Meest voorkomende combinaties",
          br(),
          sidebarLayout(
            sidebarPanel(
              sliderInput("combi_top_n", "Aantal combinaties", min = 5, max = 30, value = 15, step = 1),
              helpText(
                "Combinaties van medicatieklassen die dezelfde persoon in hetzelfde jaar ",
                "gelijktijdig gebruikte, aflopend gesorteerd op aantal personen."
              )
            ),
            mainPanel(
              plotOutput("combi_plot", height = "550px"),
              h4("Data in de grafiek"),
              tableOutput("combi_table"),
              br(),
              chart_data_downloads_ui("atc_combi_dl", chart_type = "bar")
            )
          )
        )
      )
    ),
    tabPanel(
      "GGZ zorggebruik & kosten",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput("ggz_dim", "Uitsplitsing naar", choices = DIM_CHOICES, selected = "totaal"),
          radioButtons(
            "ggz_metric", "Maatstaf",
            choices = c(
              "Totale kosten (mln euro)" = "totale_kosten",
              "Aantal gebruikers" = "gebruikers",
              "Gemiddelde kosten per gebruiker (euro)" = "gem_kosten_per_gebr",
              "Prevalentie (per 1.000 personen, 17+)" = "prevalentie"
            )
          ),
          helpText(
            "Basis-GGZ, specialistische GGZ (ambulant) en specialistische GGZ (met verblijf), ",
            "op basis van GGZ ZPM-prestatiedata 2024. Prevalentie = aantal gebruikers gedeeld ",
            "door de 17+ bevolking van de gekozen uitsplitsing, keer 1.000."
          )
        ),
        mainPanel(
          plotOutput("ggz_plot"),
          h4("Data in de grafiek"),
          tableOutput("ggz_table"),
          br(),
          chart_data_downloads_ui("ggz_kosten_dl", chart_type = "grouped_bar")
        )
      )
    ),
    tabPanel(
      "Prevalentie psychische aandoeningen",
      tabsetPanel(
        id = "prev_subtabs",
        tabPanel(
          "Alle diagnosegroepen (2016–2024)",
          br(),
          sidebarLayout(
            sidebarPanel(
              selectInput("prev_dim", "Uitsplitsing naar", choices = DIM_CHOICES, selected = "totaal"),
              conditionalPanel(
                "input.prev_dim == 'totaal'",
                checkboxGroupInput(
                  "prev_diagnoses", "Diagnosegroep(en)",
                  choices = DIAGNOSEGROEP_CHOICES, selected = DIAGNOSEGROEP_DEFAULT
                )
              ),
              conditionalPanel(
                "input.prev_dim != 'totaal'",
                selectInput("prev_diagnose_single", "Diagnosegroep", choices = DIAGNOSEGROEP_CHOICES)
              ),
              checkboxInput("prev_show_zpm_line", "Toon startjaar ZPM-prestatiemodel (2022)", value = TRUE),
              helpText(
                "Let op: vanaf 2022 is de onderliggende registratie omgezet van Vektis-declaraties ",
                "naar GGZ ZPM-prestaties. Dit kan een trendbreuk geven, vooral in de categorie ",
                "'onbekend / geen diagnose geregistreerd'."
              )
            ),
            mainPanel(
              plotOutput("prev_plot"),
              h4("Data in de grafiek"),
              tableOutput("prev_table"),
              br(),
              chart_data_downloads_ui("prev_diag_dl", chart_type = "line")
            )
          )
        ),
        tabPanel(
          "Angststoornis naar subgroep (2022–2024)",
          br(),
          sidebarLayout(
            sidebarPanel(
              selectInput("angst_dim", "Uitsplitsing naar", choices = DIM_CHOICES, selected = "totaal"),
              conditionalPanel(
                "input.angst_dim != 'totaal'",
                selectInput("angst_sg", "Subgroep angststoornis", choices = SG_GROEP_CHOICES)
              ),
              helpText(
                "SG5, SG6 en SG7 zijn de drie ZPM-subdiagnosegroepen waaruit de diagnosegroep ",
                "'angststoornis' is opgebouwd. Een klinische omschrijving per subgroep was niet ",
                "beschikbaar in de brondata; alleen 2022-2024 (ZPM-periode) is uitgesplitst."
              )
            ),
            mainPanel(
              plotOutput("angst_plot"),
              h4("Data in de grafiek"),
              tableOutput("angst_table"),
              br(),
              chart_data_downloads_ui("angst_dl", chart_type = "grouped_bar")
            )
          )
        )
      )
    ),
    tabPanel(
      "Favorites",
      br(),
      favorites_panel_ui("favorites")
    ),
    tabPanel(
      "Export history",
      br(),
      export_history_panel_ui("export_history")
    ),
    tabPanel(
      "Manage templates",
      br(),
      template_admin_ui("template_admin")
    ),
    tabPanel(
      "Dictionary",
      br(),
      dictionary_admin_ui("dictionary")
    )
  )
)

server <- function(input, output, session) {
  tc_register_app_context(
    input = input,
    dashboard_title = DASHBOARD_TITLE,
    nav_id = "main_tabs",
    subtab_by_tab = c(
      "Medicatiegebruik (ATC)" = "atc_subtabs",
      "Prevalentie psychische aandoeningen" = "prev_subtabs"
    ),
    dl_option_prefixes = c(
      "atc_prevalentie_dl" = "^atc_",
      "atc_combi_dl" = "^combi_",
      "ggz_kosten_dl" = "^ggz_",
      "prev_diag_dl" = "^prev_",
      "angst_dl" = "^angst_"
    )
  )

  # Read one chart option. `sel` is a stored option set being replayed from a
  # favorite or history entry (keyed by input id, exactly as tc_ctx_selections()
  # captured it); NULL means "use the live input". Every chart's data/title/scope
  # below goes through this, so the same code serves the on-screen figure and a
  # rebuild of a favorite saved with different options -- see the `data_for`
  # argument on chart_data_downloads_server().
  opt <- function(sel, id) {
    if (!is.null(sel) && !is.null(sel[[id]])) return(sel[[id]])
    input[[id]]
  }

  ## -- Medicatiegebruik (ATC): prevalentie per klasse ------------------------

  output$atc_klasse_picker <- renderUI({
    choices <- if (identical(input$atc_metric_group, "poly")) ATC_POLY_CHOICES else ATC_KLASSE_CHOICES
    checkboxGroupInput("atc_klassen", "Selecteer klasse(n)", choices = choices, selected = choices)
  })

  atc_subgroep_scope <- function(sel = NULL) {
    DIM_SCOPE_MAP[[opt(sel, "atc_dim")]]
  }

  atc_title <- function(sel = NULL) {
    metric_label <- if (identical(opt(sel, "atc_metric_group"), "poly")) "mono-/polyfarmacie" else "medicatieklasse"
    paste0("Prevalentie van gebruik naar ", metric_label, " — ", pharm_dim_label(opt(sel, "atc_dim")))
  }

  atc_data_for <- function(sel = NULL) {
    dim <- opt(sel, "atc_dim")
    metric_group <- opt(sel, "atc_metric_group")
    klassen <- opt(sel, "atc_klassen")
    req(klassen)
    df <- ATC_PREVALENTIE_DATA[
      ATC_PREVALENTIE_DATA$dim == dim &
        ATC_PREVALENTIE_DATA$metric_group == metric_group &
        ATC_PREVALENTIE_DATA$klasse %in% klassen,
    ]
    klasse_levels <- if (identical(metric_group, "poly")) ATC_POLY_COLS else ATC_KLASSE_COLS
    df$klasse <- factor(df$klasse, levels = base::intersect(klasse_levels, unique(df$klasse)))
    leeftijd_order <- if (identical(dim, "leeftijd")) LEEFTIJD_ORDER_FULL else LEEFTIJD_ORDER_17PLUS
    df$subgroep <- pharm_order_subgroep(df$subgroep, dim, leeftijd_order)
    df
  }

  atc_data_raw <- reactive(atc_data_for(NULL))

  atc_plot_data <- reactive({
    df <- atc_data_raw()
    df$klasse <- pharm_relabel_factor(df$klasse, scope = "klasse")
    df$subgroep <- pharm_relabel_factor(df$subgroep, scope = atc_subgroep_scope())
    df
  })

  output$atc_plot <- renderPlot({
    df <- atc_plot_data()
    ggplot(df, aes(x = subgroep, y = prevalentie, fill = klasse)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = setNames(pharm_fill_palette(nlevels(df$klasse)), levels(df$klasse))) +
      scale_y_continuous(labels = pharm_number_labels()) +
      labs(
        title = atc_title(), x = pharm_dim_label(input$atc_dim),
        y = "Prevalentie per 1.000 personen", fill = NULL
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$atc_table <- renderTable({
    df <- atc_plot_data()
    data.frame(
      Uitsplitsing = as.character(df$subgroep),
      Klasse = as.character(df$klasse),
      `Prevalentie per 1.000` = pharm_number_labels(0.1)(df$prevalentie),
      check.names = FALSE
    )
  })

  chart_data_downloads_server(
    id = "atc_prevalentie_dl",
    data = atc_data_raw,
    chart_type = "grouped_bar",
    category_col = "subgroep",
    series_col = "klasse",
    value_col = "prevalentie",
    filename_prefix = "atc_prevalentie",
    agg_fun = NULL,
    category_scope = atc_subgroep_scope,
    series_scope = "klasse",
    figure_title = atc_title,
    data_for = atc_data_for,
    source_output = "iteration0_atc.xlsx",
    source_mtime = tc_format_source_mtime(PHARM_ATC_FILE)
  )

  ## -- Medicatiegebruik (ATC): meest voorkomende combinaties -----------------

  combi_title <- function(sel = NULL) {
    paste0("Top ", opt(sel, "combi_top_n"), " meest voorkomende combinaties van medicatieklassen")
  }

  combi_data_for <- function(sel = NULL) {
    df <- utils::head(ATC_COMBINATIES_DATA, opt(sel, "combi_top_n"))
    df$combi <- factor(df$combi, levels = rev(df$combi))
    df$serie <- "Aantal personen"
    df
  }

  combi_data_raw <- reactive(combi_data_for(NULL))

  combi_plot_data <- reactive({
    df <- combi_data_raw()
    levels_raw <- levels(df$combi)
    pretty_levels <- vapply(strsplit(levels_raw, "_"), function(codes) {
      paste(pharm_relabel_factor(codes, scope = "klasse"), collapse = " + ")
    }, character(1))
    df$combi_label <- factor(as.character(df$combi), levels = levels_raw, labels = pretty_levels)
    df
  })

  output$combi_plot <- renderPlot({
    df <- combi_plot_data()
    ggplot(df, aes(x = combi_label, y = N)) +
      geom_col(fill = ahti_branding$colors$helder_blauw) +
      coord_flip() +
      scale_y_continuous(labels = pharm_number_labels()) +
      labs(title = combi_title(), x = NULL, y = "Aantal personen") +
      theme_minimal()
  })

  output$combi_table <- renderTable({
    df <- combi_plot_data()
    data.frame(
      Combinatie = as.character(df$combi_label),
      `Aantal personen` = pharm_number_labels(1)(df$N),
      check.names = FALSE
    )
  })

  chart_data_downloads_server(
    id = "atc_combi_dl",
    data = combi_data_raw,
    chart_type = "bar",
    category_col = "combi",
    series_col = "serie",
    value_col = "N",
    filename_prefix = "atc_combinaties",
    agg_fun = NULL,
    series_scope = "combi_serie",
    figure_title = combi_title,
    data_for = combi_data_for,
    source_output = "iteration0_atc.xlsx",
    source_sheet = "ATC_combinaties",
    source_mtime = tc_format_source_mtime(PHARM_ATC_FILE)
  )

  ## -- GGZ zorggebruik & kosten ----------------------------------------------

  ggz_subgroep_scope <- function(sel = NULL) {
    DIM_SCOPE_MAP[[opt(sel, "ggz_dim")]]
  }

  ggz_metric_label <- function(sel = NULL) {
    switch(
      opt(sel, "ggz_metric"),
      totale_kosten = "Totale zorgkosten (mln euro)",
      gebruikers = "Aantal gebruikers",
      gem_kosten_per_gebr = "Gemiddelde kosten per gebruiker (euro)",
      prevalentie = "Prevalentie per 1.000 personen (17+)"
    )
  }

  ggz_title <- function(sel = NULL) {
    paste0(ggz_metric_label(sel), " naar GGZ-categorie — ", pharm_dim_label(opt(sel, "ggz_dim")))
  }

  ggz_data_for <- function(sel = NULL) {
    dim <- opt(sel, "ggz_dim")
    metric <- opt(sel, "ggz_metric")
    df <- GGZ_KOSTEN_DATA[GGZ_KOSTEN_DATA$dim == dim, ]
    df$categorie <- factor(df$categorie, levels = GGZ_CATEGORIE_ORDER)
    df$subgroep <- pharm_order_subgroep(df$subgroep, dim, LEEFTIJD_ORDER_17PLUS)
    df$waarde <- if (identical(metric, "totale_kosten")) {
      df$totale_kosten / 1e6
    } else if (identical(metric, "prevalentie")) {
      df$gebruikers / df$n_17plus * 1000
    } else {
      df[[metric]]
    }
    df
  }

  ggz_data_raw <- reactive(ggz_data_for(NULL))

  ggz_plot_data <- reactive({
    df <- ggz_data_raw()
    df$categorie <- pharm_relabel_factor(df$categorie, scope = "categorie")
    df$subgroep <- pharm_relabel_factor(df$subgroep, scope = ggz_subgroep_scope())
    df
  })

  output$ggz_plot <- renderPlot({
    df <- ggz_plot_data()
    ggplot(df, aes(x = subgroep, y = waarde, fill = categorie)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = setNames(pharm_fill_palette(nlevels(df$categorie)), levels(df$categorie))) +
      scale_y_continuous(labels = pharm_number_labels()) +
      labs(title = ggz_title(), x = pharm_dim_label(input$ggz_dim), y = NULL, fill = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$ggz_table <- renderTable({
    df <- ggz_plot_data()
    accuracy <- if (input$ggz_metric %in% c("gebruikers", "gem_kosten_per_gebr")) 1 else 0.1
    out <- data.frame(
      Uitsplitsing = as.character(df$subgroep),
      `GGZ-categorie` = as.character(df$categorie),
      check.names = FALSE
    )
    out[[ggz_metric_label()]] <- pharm_number_labels(accuracy)(df$waarde)
    out
  })

  chart_data_downloads_server(
    id = "ggz_kosten_dl",
    data = ggz_data_raw,
    chart_type = "grouped_bar",
    category_col = "subgroep",
    series_col = "categorie",
    value_col = "waarde",
    filename_prefix = "ggz_kosten",
    agg_fun = NULL,
    category_scope = ggz_subgroep_scope,
    series_scope = "categorie",
    figure_title = ggz_title,
    data_for = ggz_data_for,
    source_output = "iteration0_ggz.xlsx",
    source_mtime = tc_format_source_mtime(PHARM_GGZ_FILE)
  )

  ## -- Prevalentie psychische aandoeningen: alle diagnosegroepen -------------

  prev_groep_scope <- function(sel = NULL) {
    if (identical(opt(sel, "prev_dim"), "totaal")) return("diagnosegroep")
    DIM_SCOPE_MAP[[opt(sel, "prev_dim")]]
  }

  prev_title <- function(sel = NULL) {
    if (identical(opt(sel, "prev_dim"), "totaal")) {
      "Prevalentie van diagnosegroepen (per 1.000 personen), 2016–2024"
    } else {
      diag_label <- pharm_pretty(opt(sel, "prev_diagnose_single"), scope = "diagnosegroep")
      paste0(
        "Prevalentie van '", diag_label, "' (per 1.000 personen) naar ",
        pharm_dim_label(opt(sel, "prev_dim")), ", 2016–2024"
      )
    }
  }

  prev_data_for <- function(sel = NULL) {
    dim <- opt(sel, "prev_dim")
    df <- PREV_DIAGNOSE_DATA[PREV_DIAGNOSE_DATA$dim == dim, ]
    if (identical(dim, "totaal")) {
      diagnoses <- opt(sel, "prev_diagnoses")
      req(diagnoses)
      df <- df[df$diagnosegroep %in% diagnoses, ]
      df$groep <- factor(df$diagnosegroep, levels = base::intersect(DIAGNOSEGROEP_COLS, unique(df$diagnosegroep)))
    } else {
      diagnose_single <- opt(sel, "prev_diagnose_single")
      req(diagnose_single)
      df <- df[df$diagnosegroep == diagnose_single, ]
      df$groep <- pharm_order_subgroep(df$subgroep, dim, LEEFTIJD_ORDER_17PLUS)
    }
    df$jaar <- as.integer(df$jaar)
    df[, c("dim", "jaar", "groep", "prevalentie")]
  }

  prev_data_raw <- reactive(prev_data_for(NULL))

  prev_plot_data <- reactive({
    df <- prev_data_raw()
    df$groep <- pharm_relabel_factor(df$groep, scope = prev_groep_scope())
    df
  })

  output$prev_plot <- renderPlot({
    df <- prev_plot_data()
    p <- ggplot(df, aes(x = jaar, y = prevalentie, color = groep, group = groep))
    if (isTRUE(input$prev_show_zpm_line)) {
      # Drawn before the data lines/points (added below) so it sits behind
      # them as a background reference marker instead of obscuring the data.
      p <- p +
        geom_vline(xintercept = 2022, linetype = "dashed", color = ahti_branding$colors$midden_grijs) +
        annotate(
          "text", x = 2022, y = Inf, label = "Start ZPM-prestatiemodel (2022)",
          hjust = -0.02, vjust = 1.5, size = 3, color = ahti_branding$colors$midden_grijs
        )
    }
    p +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_color_manual(values = setNames(pharm_fill_palette(nlevels(df$groep)), levels(df$groep))) +
      scale_x_continuous(labels = pharm_year_labels()) +
      scale_y_continuous(labels = pharm_number_labels()) +
      labs(title = prev_title(), x = "Jaar", y = "Prevalentie per 1.000 personen", color = NULL) +
      theme_minimal()
  })

  output$prev_table <- renderTable({
    df <- prev_plot_data()
    data.frame(
      Jaar = as.character(df$jaar),
      Groep = as.character(df$groep),
      `Prevalentie per 1.000` = pharm_number_labels(0.1)(df$prevalentie),
      check.names = FALSE
    )
  })

  chart_data_downloads_server(
    id = "prev_diag_dl",
    data = prev_data_raw,
    chart_type = "line",
    category_col = "jaar",
    series_col = "groep",
    value_col = "prevalentie",
    filename_prefix = "ggz_prevalentie_diagnosegroepen",
    agg_fun = NULL,
    category_scope = "jaar",
    series_scope = prev_groep_scope,
    figure_title = prev_title,
    data_for = prev_data_for,
    source_output = "prevalenties.xlsx",
    source_mtime = tc_format_source_mtime(PHARM_PREV_FILE)
  )

  ## -- Prevalentie psychische aandoeningen: angststoornis naar subgroep -----

  angst_groep_scope <- function(sel = NULL) {
    if (identical(opt(sel, "angst_dim"), "totaal")) return("sg_groep")
    DIM_SCOPE_MAP[[opt(sel, "angst_dim")]]
  }

  angst_title <- function(sel = NULL) {
    if (identical(opt(sel, "angst_dim"), "totaal")) {
      "Prevalentie van angststoornis-subgroepen (per 1.000 personen), 2022–2024"
    } else {
      paste0(
        "Prevalentie van subgroep ", opt(sel, "angst_sg"), " (per 1.000 personen) naar ",
        pharm_dim_label(opt(sel, "angst_dim")), ", 2022–2024"
      )
    }
  }

  angst_data_for <- function(sel = NULL) {
    dim <- opt(sel, "angst_dim")
    df <- ANGST_DATA[ANGST_DATA$dim == dim, ]
    if (identical(dim, "totaal")) {
      df$groep <- factor(df$sg_groep, levels = SG_GROEP_COLS)
    } else {
      sg <- opt(sel, "angst_sg")
      req(sg)
      df <- df[df$sg_groep == sg, ]
      df$groep <- pharm_order_subgroep(df$subgroep, dim, LEEFTIJD_ORDER_17PLUS)
    }
    df$jaar <- as.integer(df$jaar)
    df[, c("dim", "jaar", "groep", "prevalentie")]
  }

  angst_data_raw <- reactive(angst_data_for(NULL))

  angst_plot_data <- reactive({
    df <- angst_data_raw()
    df$groep <- pharm_relabel_factor(df$groep, scope = angst_groep_scope())
    df
  })

  output$angst_plot <- renderPlot({
    df <- angst_plot_data()
    ggplot(df, aes(x = factor(jaar), y = prevalentie, fill = groep)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = setNames(pharm_fill_palette(nlevels(df$groep)), levels(df$groep))) +
      scale_y_continuous(labels = pharm_number_labels()) +
      labs(title = angst_title(), x = "Jaar", y = "Prevalentie per 1.000 personen", fill = NULL) +
      theme_minimal()
  })

  output$angst_table <- renderTable({
    df <- angst_plot_data()
    data.frame(
      Jaar = as.character(df$jaar),
      Groep = as.character(df$groep),
      `Prevalentie per 1.000` = pharm_number_labels(0.1)(df$prevalentie),
      check.names = FALSE
    )
  })

  chart_data_downloads_server(
    id = "angst_dl",
    data = angst_data_raw,
    chart_type = "grouped_bar",
    category_col = "jaar",
    series_col = "groep",
    value_col = "prevalentie",
    filename_prefix = "angststoornis_subgroepen",
    agg_fun = NULL,
    category_scope = "jaar",
    series_scope = angst_groep_scope,
    figure_title = angst_title,
    data_for = angst_data_for,
    source_output = "prevalenties_angst_22_24.xlsx",
    source_mtime = tc_format_source_mtime(PHARM_ANGST_FILE)
  )

  favorites_panel_server("favorites")
  export_history_panel_server("export_history")
  template_admin_server("template_admin")
  dictionary_admin_server("dictionary")
}

shinyApp(ui = ui, server = server)
