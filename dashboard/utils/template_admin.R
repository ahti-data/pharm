#' Template management UI, decoupled from git deploys.
#'
#' Lets someone add a new think-cell `.pptx` template without a developer
#' committing it to the repo. Uploads are written to the runtime uploads dir
#' ([tc_custom_templates_dir()], `state/template_uploads/` in production), which
#' the deploy workflow never syncs (see CLAUDE.md), so they survive a redeploy
#' automatically. Reuses the existing template resolution logic in
#' `utils/slide_download.R` rather than duplicating it — this module only
#' widens where those functions look.

#' Sanitize an uploaded file name to a safe, flat `.pptx` basename.
#'
#' Strips any directory components (defends against path traversal via a
#' crafted upload name) and replaces anything but alphanumerics/`-`/`_` with
#' `_`, keeping the extension.
#' @param name Original file name from `fileInput`.
#' @return Sanitized base name, always ending in `.pptx`.
tmpl_sanitize_filename <- function(name) {
  name <- basename(as.character(name))
  ext  <- tools::file_ext(name)
  stem <- tools::file_path_sans_ext(name)
  stem <- gsub("[^A-Za-z0-9_-]+", "_", stem)
  if (!nzchar(stem)) stem <- "template"
  paste0(stem, ".pptx")
}

#' Whether a file looks like a real `.pptx` (a zip archive) rather than
#' something merely renamed to end in `.pptx`.
#' @param path Path to the uploaded temp file.
tmpl_looks_like_pptx <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  magic <- tryCatch(readBin(con, "raw", n = 2), error = function(e) raw(0))
  length(magic) == 2 && magic[1] == as.raw(0x50) && magic[2] == as.raw(0x4B) # "PK"
}

#' Copy a validated upload into the runtime uploads dir, avoiding name
#' collisions. The actual copy runs under [tc_with_file_lock()] (see
#' `utils/slide_download.R`), keyed to the uploads dir itself, so two
#' concurrent uploads (e.g. targeting the same sanitized filename) can't
#' interleave their `file.copy()` calls. The dir-exists/writable check stays
#' outside the lock so a missing/unwritable directory still returns the
#' usual graceful error instead of a lock-acquire failure.
#' @param tmp_path Path to the uploaded temp file (from `fileInput`).
#' @param original_name Original file name as selected by the user.
#' @param templates_dir Optional base templates directory override.
#' @return list(ok, message, filename).
tmpl_save_upload <- function(tmp_path, original_name, templates_dir = NULL) {
  ext <- tolower(tools::file_ext(original_name))
  if (!identical(ext, "pptx")) {
    return(list(ok = FALSE, message = "Only .pptx files are accepted.", filename = NA_character_))
  }
  if (!tmpl_looks_like_pptx(tmp_path)) {
    return(list(ok = FALSE, message = "File does not look like a valid .pptx (not a zip archive).",
                filename = NA_character_))
  }

  custom_dir <- tc_custom_templates_dir(templates_dir)
  if (is.null(custom_dir) || is.na(custom_dir)) {
    return(list(ok = FALSE, message = "Could not resolve a templates/ directory to upload into.",
                filename = NA_character_))
  }
  if (!dir.exists(custom_dir)) {
    dir.create(custom_dir, recursive = TRUE, showWarnings = FALSE)
  }
  # The uploads directory is created at runtime under templates/, which on a
  # deployed server may not be writable by the Shiny process. Detect that here
  # instead of reporting a false "Uploaded" (the copy below would silently fail
  # and the template would never appear in the list).
  if (!dir.exists(custom_dir)) {
    return(list(ok = FALSE, filename = NA_character_, message = sprintf(
      "Could not create the uploads folder '%s'. The app may not have write access there.",
      custom_dir)))
  }

  filename <- tmpl_sanitize_filename(original_name)
  dest <- file.path(custom_dir, filename)
  tc_with_file_lock(file.path(custom_dir, ".lock"), function() {
    copied <- file.copy(tmp_path, dest, overwrite = TRUE)
    if (!isTRUE(copied) || !file.exists(dest)) {
      return(list(ok = FALSE, filename = NA_character_, message = sprintf(
        "Could not save the upload to '%s'. The folder may not be writable by the app.",
        dest)))
    }
    list(ok = TRUE, message = sprintf("Uploaded '%s'.", filename), filename = filename)
  })
}

#' Whether a file looks like a real PNG (checks the 8-byte PNG signature)
#' rather than something merely renamed to end in `.png`.
#' @param path Path to the uploaded temp file.
tmpl_looks_like_png <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  magic <- tryCatch(readBin(con, "raw", n = 8), error = function(e) raw(0))
  png_sig <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
  length(magic) == 8 && identical(magic, png_sig)
}

#' Save an uploaded preview screenshot for a named template.
#'
#' Previews are purely cosmetic (see `utils/slide_download.R`), so this never
#' touches the templates themselves -- just adds/replaces a thumbnail image,
#' named after the template it belongs to.
#' @param tmp_path Path to the uploaded temp file (from `fileInput`).
#' @param template_name The `.pptx` file name this preview belongs to (as
#'   chosen from [tc_list_templates()]).
#' @param templates_dir Optional base templates directory override.
#' @return list(ok, message).
tmpl_save_preview_upload <- function(tmp_path, template_name, templates_dir = NULL) {
  if (is.null(template_name) || is.na(template_name) || !nzchar(template_name)) {
    return(list(ok = FALSE, message = "Choose which template this preview belongs to."))
  }
  if (!tmpl_looks_like_png(tmp_path)) {
    return(list(ok = FALSE, message = "File does not look like a valid PNG image."))
  }

  preview_dir <- tc_custom_previews_dir(templates_dir)
  if (is.null(preview_dir) || is.na(preview_dir)) {
    return(list(ok = FALSE, message = "Could not resolve a previews directory to upload into."))
  }
  if (!dir.exists(preview_dir)) {
    dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(preview_dir)) {
    return(list(ok = FALSE, message = sprintf(
      "Could not create the previews folder '%s'. The app may not have write access there.",
      preview_dir)))
  }

  dest <- file.path(preview_dir, tc_preview_filename(template_name))
  copied <- file.copy(tmp_path, dest, overwrite = TRUE)
  if (!isTRUE(copied) || !file.exists(dest)) {
    return(list(ok = FALSE, message = sprintf(
      "Could not save the preview to '%s'. The folder may not be writable by the app.",
      dest)))
  }

  list(ok = TRUE, message = sprintf("Saved preview for '%s'.", template_name))
}

#' Bundle every currently available template into one `.zip` so someone can
#' download a base template, edit it in PowerPoint, and re-upload it.
#'
#' Includes the built-in `templates/` set plus any uploaded overrides in the
#' runtime uploads dir (deduplicated by name, the uploaded copy winning — the
#' same effective set [tc_list_templates()] shows), so what you download is
#' exactly what the dashboard would use. Always produces a valid `.zip`; if no
#' templates are found it contains a short README instead.
#' @param zip_path Output `.zip` path (the `file` from a downloadHandler).
#' @param templates_dir Optional base templates directory override.
#' @return `zip_path` (invisibly).
tmpl_build_templates_zip <- function(zip_path, templates_dir = NULL) {
  work <- tempfile("templates_dl_")
  dir.create(work)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(work, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  files <- tc_list_templates(templates_dir)
  copied <- character(0)
  for (f in files) {
    src <- tc_resolve_template_path(f, templates_dir)
    if (!is.na(src)) {
      file.copy(src, file.path(work, f), overwrite = TRUE)
      copied <- c(copied, f)
    }
  }
  if (length(copied) == 0) {
    writeLines(
      "No slide templates were found to download.",
      file.path(work, "README.txt")
    )
  }

  zip_files <- basename(list.files(work, full.names = TRUE))
  zip_path_abs <- normalizePath(zip_path, winslash = "/", mustWork = FALSE)
  setwd(work)
  utils::zip(zipfile = zip_path_abs, files = zip_files, flags = "-q -X")
  invisible(zip_path_abs)
}

#' UI for the template management panel.
#' @param id Module id.
template_admin_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Slide templates"),
    shiny::p(
      class = "text-muted",
      "Upload a new think-cell .pptx template here — no git commit needed. ",
      "Uploaded templates take precedence over a built-in template of the same name."
    ),
    shiny::fileInput(ns("upload"), "Upload a .pptx template", accept = ".pptx"),
    shiny::uiOutput(ns("status")),

    shiny::h5("Preview image"),
    shiny::p(
      class = "text-muted",
      "Add or replace a screenshot for a template, so it's easier to tell templates ",
      "apart at a glance without opening each one."
    ),
    shiny::uiOutput(ns("preview_template_select")),
    shiny::fileInput(ns("preview_upload"), "Preview image (.png)", accept = ".png"),
    shiny::actionButton(ns("save_preview"), "Save preview", class = "btn-default"),
    shiny::uiOutput(ns("preview_status")),

    shiny::h5("Available templates"),
    shiny::p(
      class = "text-muted",
      "Download the current templates to edit a base template, then upload your ",
      "edited copy above (same file name to replace it, a new name to add it)."
    ),
    shiny::downloadButton(ns("download_templates"), "Download templates (.zip)",
                          class = "btn-default"),
    shiny::uiOutput(ns("template_list"))
  )
}

#' Server logic for the template management panel.
#' @param id Module id.
#' @param templates_dir Optional base templates directory override.
template_admin_server <- function(id, templates_dir = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    status_rv <- shiny::reactiveValues(message = NULL, ok = NA)
    refresh_trigger <- shiny::reactiveVal(0)

    shiny::observeEvent(input$upload, {
      req_file <- input$upload
      shiny::req(req_file)

      result <- tmpl_save_upload(req_file$datapath, req_file$name, templates_dir)
      status_rv$message <- result$message
      status_rv$ok <- result$ok
      if (isTRUE(result$ok)) {
        refresh_trigger(shiny::isolate(refresh_trigger()) + 1)
      }
    })

    output$status <- shiny::renderUI({
      shiny::req(status_rv$message)
      cls <- if (isTRUE(status_rv$ok)) "text-success" else "text-danger"
      shiny::tags$p(class = cls, status_rv$message)
    })

    preview_status_rv <- shiny::reactiveValues(message = NULL, ok = NA)

    output$preview_template_select <- shiny::renderUI({
      refresh_trigger()
      shiny::selectInput(
        session$ns("preview_template"), "Template",
        choices = tc_list_templates(templates_dir)
      )
    })

    shiny::observeEvent(input$save_preview, {
      shiny::req(input$preview_upload)
      result <- tmpl_save_preview_upload(
        input$preview_upload$datapath, input$preview_template, templates_dir
      )
      preview_status_rv$message <- result$message
      preview_status_rv$ok <- result$ok
      if (isTRUE(result$ok)) {
        refresh_trigger(shiny::isolate(refresh_trigger()) + 1)
      }
    })

    output$preview_status <- shiny::renderUI({
      shiny::req(preview_status_rv$message)
      cls <- if (isTRUE(preview_status_rv$ok)) "text-success" else "text-danger"
      shiny::tags$p(class = cls, preview_status_rv$message)
    })

    output$download_templates <- shiny::downloadHandler(
      filename = function() paste0("slide_templates_", Sys.Date(), ".zip"),
      content = function(file) {
        tmpl_build_templates_zip(file, templates_dir)
      }
    )

    output$template_list <- shiny::renderUI({
      refresh_trigger()
      files <- tc_list_templates(templates_dir)
      if (length(files) == 0) {
        return(shiny::tags$p(class = "text-muted", "No templates available yet."))
      }
      custom_dir <- tc_custom_templates_dir(templates_dir)
      rows <- lapply(files, function(f) {
        is_custom    <- !is.na(custom_dir) && file.exists(file.path(custom_dir, f))
        preview_uri  <- tc_preview_data_uri(f, templates_dir)
        thumb <- if (!is.na(preview_uri)) {
          shiny::tags$img(
            src = preview_uri,
            style = paste(
              "width:160px; height:90px; object-fit:contain; background:#fff;",
              "border:1px solid #E4E7EE; border-radius:4px;"
            )
          )
        } else {
          shiny::tags$div(
            style = paste(
              "width:160px; height:90px; display:flex; align-items:center;",
              "justify-content:center; background:#F7F8FA; border:1px dashed #D1D5DB;",
              "border-radius:4px; font-size:11px; color:#9CA3AF; text-align:center;"
            ),
            "No preview"
          )
        }
        shiny::tags$div(
          style = paste(
            "display:flex; align-items:center; gap:12px; padding:8px 0;",
            "border-bottom:1px solid #eee;"
          ),
          thumb,
          shiny::tags$div(
            shiny::tags$strong(f),
            shiny::tags$div(
              style = "font-size:12px; color:#6B7280;",
              if (is_custom) "Uploaded" else "Built-in"
            )
          )
        )
      })
      do.call(shiny::tagList, rows)
    })

    invisible(refresh_trigger)
  })
}
