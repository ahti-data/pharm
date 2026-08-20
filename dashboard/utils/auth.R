#' Shared shinymanager authentication helpers for deployed dashboards.
#'
#' Production uses the shared credentials DB on the Shiny server. When that file
#' is missing (typical local development), auth is skipped automatically.

AUTH_DB_PATH <- Sys.getenv("SHINY_AUTH_DB", "/srv/shiny-server/passwords.sqlite")
AUTH_DB_PASSPHRASE <- Sys.getenv("SHINY_AUTH_DB_PASSPHRASE", "")

#' Whether the shared credentials database is available.
is_auth_enabled <- function() {
  file.exists(AUTH_DB_PATH)
}

#' Head tags that hide the app until login and per-app access succeed.
auth_ui_head <- function() {
  shiny::tags$head(
    shiny::tags$style("body { visibility: hidden; }"),
    shiny::tags$script(shiny::HTML("
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('show_app', function(x) {
          document.body.style.visibility = 'visible';
        });
        Shiny.addCustomMessageHandler('no_access_redirect', function(x) {
          document.body.style.visibility = 'visible';
          document.body.innerHTML = '<div style=\"display:flex;align-items:center;justify-content:center;height:100vh;flex-direction:column;background:white;\"><h1>Geen toegang</h1><p>Je hebt geen toegang tot deze app.</p><p>Je wordt teruggestuurd naar het loginscherm...</p></div>';
          setTimeout(function(){ location.reload(); }, 5000);
        });
      });
    "))
  )
}

#' Check whether a user may access this dashboard folder.
#'
#' @param user_apps Value from the credentials `apps` column (`"all"` or comma-separated).
#' @param app_name Folder name of the deployed app (typically `basename(getwd())`).
user_has_app_access <- function(user_apps, app_name) {
  if (is.null(user_apps) || !nzchar(user_apps)) {
    return(FALSE)
  }
  if (identical(user_apps, "all")) {
    return(TRUE)
  }
  app_name %in% strsplit(user_apps, ",", fixed = TRUE)[[1]]
}

auth_check_credentials <- function() {
  args <- list(db = AUTH_DB_PATH)
  if (nzchar(AUTH_DB_PASSPHRASE)) {
    args$passphrase <- AUTH_DB_PASSPHRASE
  }
  do.call(shinymanager::check_credentials, args)
}

#' Set up shinymanager auth and per-app access checks.
#'
#' @return List with `res_auth` (shinymanager reactiveValues) and `auth_ok` (reactiveVal).
setup_dashboard_auth <- function(session) {
  res_auth <- shinymanager::secure_server(
    check_credentials = auth_check_credentials()
  )
  auth_ok <- shiny::reactiveVal(FALSE)

  shiny::observe({
    shiny::req(shiny::reactiveValuesToList(res_auth)$user)
    auth_info <- shiny::reactiveValuesToList(res_auth)
    user_apps <- auth_info$apps
    app_name <- basename(getwd())

    if (user_has_app_access(user_apps, app_name)) {
      auth_ok(TRUE)
      session$sendCustomMessage("show_app", TRUE)
    } else {
      session$sendCustomMessage("no_access_redirect", TRUE)
    }
  })

  list(res_auth = res_auth, auth_ok = auth_ok)
}

#' Reveal the app when auth is disabled (local development).
show_app_without_auth <- function(session) {
  session$sendCustomMessage("show_app", TRUE)
}
