# Template Shiny app
#
# Put project data in the data/ folder and replace the placeholders below
# with your dashboard logic.

source("data/metadata/brand_colors.R")
source("utils/format_thinkcell_download.R")
source("utils/slide_download.R")
source("utils/template_admin.R")
source("utils/favorites.R")
source("utils/export_history.R")
source("utils/chart_downloads.R")
source("utils/dictionary.R")
source("utils/dictionary_admin.R")
source("utils/tab_theme.R")
source("utils/auth.R")

library(shiny)
library(shinymanager)
library(dplyr)
library(ggplot2)

# Add any project-specific data import helpers here.
load_project_data <- function() {
  NULL
}

# Example plot data: replace with your filtered/aggregated chart data in real
# dashboards. Read from a committed CSV (rather than inlined) so this scaffold
# also demonstrates the data-source provenance wiring -- source_output +
# source_mtime on chart_data_downloads_server() below -- that a real dashboard
# uses to stamp where its data came from and when it was last edited.
EXAMPLE_DATA_FILE <- "data/example_revenue.csv"
example_revenue_data <- function() {
  tibble::as_tibble(utils::read.csv(EXAMPLE_DATA_FILE, stringsAsFactors = FALSE))
}

# Chart metadata for think-cell export wiring.
EXAMPLE_CHART_TYPE <- "stacked_bar"
EXAMPLE_CATEGORY_COL <- "quarter"
EXAMPLE_SERIES_COL <- "product"
EXAMPLE_VALUE_COL <- "revenue"

ui <- fluidPage(
  if (is_auth_enabled()) auth_ui_head(),
  tc_tab_color_theme(ahti_branding),
  titlePanel("Dashboard template"),
  tabsetPanel(
    tabPanel(
      "Example chart",
      fluidRow(
        column(
          width = 12,
          h3("Example chart"),
          p(
            "This example shows the download pattern: raw plot data, think-cell ",
            "formatted data, and (when a matching template exists) a PowerPoint ",
            "slide + table + log as one ZIP. Star a chart to add it to Favorites."
          ),
          plotOutput("example_chart"),
          br(),
          chart_data_downloads_ui("example_downloads", chart_type = EXAMPLE_CHART_TYPE)
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
  if (is_auth_enabled()) {
    auth <- setup_dashboard_auth(session)
  } else {
    show_app_without_auth(session)
  }

  # Placeholder wiring for load_project_data() (a no-op stub above) -- a real
  # dashboard replaces both with its actual data loading and drops the
  # invisible(data) return at the bottom of this function once that data is
  # actually used by real reactives instead of just being held onto.
  data <- load_project_data()

  # Raw data, exactly as the underlying source provides it -- fed to
  # chart_data_downloads_server() below, which applies dictionary_relabel()
  # to its own copy for downloads, gated by that module's own "Format from
  # dictionary" checkbox (see utils/chart_downloads.R). The chart itself
  # always renders dictionary-formatted labels (see plot_data_pretty below),
  # independent of that checkbox.
  plot_data <- reactive({
    example_revenue_data()
  })

  plot_data_pretty <- reactive({
    plot_data() %>%
      mutate(
        !!EXAMPLE_CATEGORY_COL := dictionary_relabel(.data[[EXAMPLE_CATEGORY_COL]], scope = EXAMPLE_CATEGORY_COL),
        !!EXAMPLE_SERIES_COL := dictionary_relabel(.data[[EXAMPLE_SERIES_COL]], scope = EXAMPLE_SERIES_COL)
      )
  })

  output$example_chart <- renderPlot({
    plot_data_pretty() %>%
      ggplot(aes(
        x = .data[[EXAMPLE_CATEGORY_COL]],
        y = .data[[EXAMPLE_VALUE_COL]],
        fill = .data[[EXAMPLE_SERIES_COL]]
      )) +
      geom_col(position = "stack") +
      scale_fill_manual(values = ahti_branding$scale_discrete) +
      labs(
        title = "Revenue by quarter",
        x = "Quarter",
        y = "Revenue",
        fill = "Product"
      ) +
      theme_minimal()
  })

  chart_data_downloads_server(
    id = "example_downloads",
    data = plot_data,
    chart_type = EXAMPLE_CHART_TYPE,
    category_col = EXAMPLE_CATEGORY_COL,
    series_col = EXAMPLE_SERIES_COL,
    value_col = EXAMPLE_VALUE_COL,
    filename_prefix = "revenue_chart",
    agg_fun = NULL,
    # Data-source provenance: the file the chart data came from and its
    # last-edited date. Stamped into every export's A1 corner-cell log and
    # shown as the "Source data updated" line in the download panel. A real
    # dashboard points these at its actual source workbook/sheet (see
    # tc_format_source_mtime() and CLAUDE.md's source_output/source_sheet
    # convention); omit both for a chart with no external source file.
    source_output = basename(EXAMPLE_DATA_FILE),
    source_mtime = tc_format_source_mtime(EXAMPLE_DATA_FILE)
  )

  favorites_panel_server("favorites")
  export_history_panel_server("export_history")
  template_admin_server("template_admin")
  dictionary_admin_server("dictionary")

  invisible(data)
}

if (is_auth_enabled()) {
  shinyApp(secure_app(ui, enable_admin = TRUE), server)
} else {
  shinyApp(ui = ui, server = server)
}
