library(testthat)

make_fake_pptx <- function(path) {
  con <- file(path, "wb")
  writeBin(as.raw(c(0x50, 0x4B, 0x03, 0x04)), con)
  close(con)
}

make_fake_png <- function(path) {
  con <- file(path, "wb")
  writeBin(as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0xFF, 0xFF)), con)
  close(con)
}

test_that("filenames are sanitized to a flat, safe .pptx basename", {
  expect_equal(tmpl_sanitize_filename("My Template.pptx"), "My_Template.pptx")
  expect_equal(tmpl_sanitize_filename("../../etc/passwd.pptx"), "passwd.pptx")
  expect_equal(tmpl_sanitize_filename("weird!@#chars.pptx"), "weird_chars.pptx")
})

test_that("only zip-signed files are accepted as .pptx", {
  tmp_ok <- tempfile(fileext = ".pptx")
  make_fake_pptx(tmp_ok)
  expect_true(tmpl_looks_like_pptx(tmp_ok))

  tmp_bad <- tempfile(fileext = ".pptx")
  writeLines("not a zip", tmp_bad)
  expect_false(tmpl_looks_like_pptx(tmp_bad))
})

test_that("uploads are rejected when not a .pptx or not zip-signed", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)

  tmp_txt <- tempfile(fileext = ".txt")
  writeLines("hello", tmp_txt)
  res <- tmpl_save_upload(tmp_txt, "notes.txt", templates_dir)
  expect_false(res$ok)

  tmp_fake <- tempfile(fileext = ".pptx")
  writeLines("not a zip", tmp_fake)
  res2 <- tmpl_save_upload(tmp_fake, "fake.pptx", templates_dir)
  expect_false(res2$ok)
})

test_that("uploads default to a runtime-writable dir outside templates/ (production path)", {
  # With no explicit templates_dir, uploads must NOT target the deploy-owned
  # templates/ dir (read-only for the app user on a server) but a runtime-
  # writable location -- the same reason state/favorites.json works.
  uploads <- tempfile("uploads_")
  old <- Sys.getenv("SHINY_TEMPLATE_UPLOADS_DIR", unset = NA)
  Sys.setenv(SHINY_TEMPLATE_UPLOADS_DIR = uploads)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_TEMPLATE_UPLOADS_DIR") else Sys.setenv(SHINY_TEMPLATE_UPLOADS_DIR = old)
    unlink(uploads, recursive = TRUE)
  }, add = TRUE)

  expect_equal(
    normalizePath(tc_custom_templates_dir(), winslash = "/", mustWork = FALSE),
    normalizePath(uploads, winslash = "/", mustWork = FALSE)
  )

  tmp_upload <- tempfile(fileext = ".pptx")
  make_fake_pptx(tmp_upload)
  res <- tmpl_save_upload(tmp_upload, "runtime_deck.pptx")  # no templates_dir -> production path
  expect_true(res$ok)
  expect_true(file.exists(file.path(uploads, "runtime_deck.pptx")))
  expect_true("runtime_deck.pptx" %in% tc_list_templates())
  expect_equal(
    normalizePath(tc_resolve_template_path("runtime_deck.pptx"), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(uploads, "runtime_deck.pptx"), winslash = "/", mustWork = FALSE)
  )
})

test_that("a failed write is reported as an error, not a false success", {
  # Point the templates dir at a *file*, so the templates/custom/ subdir can't
  # be created -- standing in for a non-writable directory on the server.
  fake_templates <- tempfile(fileext = ".notadir")
  writeLines("x", fake_templates)

  tmp_upload <- tempfile(fileext = ".pptx")
  make_fake_pptx(tmp_upload)

  res <- tmpl_save_upload(tmp_upload, "team_deck.pptx", fake_templates)
  expect_false(res$ok)
  expect_true(is.na(res$filename))
  expect_match(res$message, "write access|writable|Could not")
})

test_that("a valid upload lands in templates/custom/ and is listed with precedence", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  # a built-in template with the same name as the upload, to prove precedence
  writeLines("builtin", file.path(templates_dir, "team_deck.pptx"))

  tmp_upload <- tempfile(fileext = ".pptx")
  make_fake_pptx(tmp_upload)
  res <- tmpl_save_upload(tmp_upload, "team_deck.pptx", templates_dir)

  expect_true(res$ok)
  expect_true(file.exists(file.path(templates_dir, "custom", "team_deck.pptx")))

  # listing shows one entry (deduplicated by name)
  files <- tc_list_templates(templates_dir)
  expect_equal(sum(files == "team_deck.pptx"), 1)

  # resolution prefers the uploaded (custom) copy
  resolved <- tc_resolve_template_path("team_deck.pptx", templates_dir)
  expect_true(grepl("custom", resolved, fixed = TRUE))
})

test_that("repeated sequential uploads still succeed under the new file lock", {
  # tmpl_save_upload()'s copy step now runs under tc_with_file_lock() (see
  # utils/slide_download.R); if the lock weren't released after each call,
  # a later call in the same process would hang/time out rather than fail
  # an assertion.
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)

  for (i in 1:3) {
    tmp_upload <- tempfile(fileext = ".pptx")
    make_fake_pptx(tmp_upload)
    res <- tmpl_save_upload(tmp_upload, sprintf("deck_%d.pptx", i), templates_dir)
    expect_true(res$ok)
  }
  expect_setequal(tc_list_templates(templates_dir), c("deck_1.pptx", "deck_2.pptx", "deck_3.pptx"))
})

test_that("uploads survive being listed even when no custom dir exists yet", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  writeLines("builtin", file.path(templates_dir, "only_builtin.pptx"))
  expect_equal(tc_list_templates(templates_dir), "only_builtin.pptx")
})

test_that("preview file name mirrors the template's stem with a .png extension", {
  expect_equal(tc_preview_filename("template_v_bar.pptx"), "template_v_bar.png")
  expect_equal(tc_preview_filename("My Template.PPTX"), "My Template.png")
})

test_that("only PNG-signed files are accepted as previews", {
  tmp_ok <- tempfile(fileext = ".png")
  make_fake_png(tmp_ok)
  expect_true(tmpl_looks_like_png(tmp_ok))

  tmp_bad <- tempfile(fileext = ".png")
  writeLines("not a png", tmp_bad)
  expect_false(tmpl_looks_like_png(tmp_bad))
})

test_that("a preview upload requires naming the template it belongs to", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  tmp_png <- tempfile(fileext = ".png")
  make_fake_png(tmp_png)

  res <- tmpl_save_preview_upload(tmp_png, "", templates_dir)
  expect_false(res$ok)

  res2 <- tmpl_save_preview_upload(tmp_png, NA_character_, templates_dir)
  expect_false(res2$ok)
})

test_that("a non-PNG preview upload is rejected", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  tmp_fake <- tempfile(fileext = ".png")
  writeLines("not a png", tmp_fake)

  res <- tmpl_save_preview_upload(tmp_fake, "template_v_bar.pptx", templates_dir)
  expect_false(res$ok)
  expect_match(res$message, "PNG")
})

test_that("a valid preview upload is resolvable and renders as a data URI", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  make_fake_pptx(file.path(templates_dir, "template_v_bar.pptx"))

  tmp_png <- tempfile(fileext = ".png")
  make_fake_png(tmp_png)
  res <- tmpl_save_preview_upload(tmp_png, "template_v_bar.pptx", templates_dir)
  expect_true(res$ok)

  resolved <- tc_resolve_preview_path("template_v_bar.pptx", templates_dir)
  expect_false(is.na(resolved))
  expect_true(file.exists(resolved))

  uri <- tc_preview_data_uri("template_v_bar.pptx", templates_dir)
  expect_true(startsWith(uri, "data:image/png;base64,"))
})

test_that("a template with no preview resolves to NA, not an error", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  make_fake_pptx(file.path(templates_dir, "template_line.pptx"))

  expect_true(is.na(tc_resolve_preview_path("template_line.pptx", templates_dir)))
  expect_true(is.na(tc_preview_data_uri("template_line.pptx", templates_dir)))
})

test_that("a built-in preview is used when no custom override exists", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  make_fake_pptx(file.path(templates_dir, "template_pie.pptx"))
  dir.create(file.path(templates_dir, "previews"), recursive = TRUE)
  make_fake_png(file.path(templates_dir, "previews", "template_pie.png"))

  resolved <- tc_resolve_preview_path("template_pie.pptx", templates_dir)
  expect_false(is.na(resolved))
  expect_true(grepl(file.path("previews", "template_pie.png"), resolved, fixed = TRUE))
})

test_that("a custom preview override wins over a built-in preview of the same name", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  make_fake_pptx(file.path(templates_dir, "template_pie.pptx"))
  dir.create(file.path(templates_dir, "previews"), recursive = TRUE)
  make_fake_png(file.path(templates_dir, "previews", "template_pie.png"))
  dir.create(file.path(templates_dir, "custom", "previews"), recursive = TRUE)
  make_fake_png(file.path(templates_dir, "custom", "previews", "template_pie.png"))

  resolved <- tc_resolve_preview_path("template_pie.pptx", templates_dir)
  expect_true(grepl(file.path("custom", "previews"), resolved, fixed = TRUE))
})

test_that("tmpl_build_templates_zip bundles the effective template set", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  make_fake_pptx(file.path(templates_dir, "template_line.pptx"))
  make_fake_pptx(file.path(templates_dir, "template_v_bar.pptx"))
  # a custom override of one built-in, to prove the custom copy wins
  dir.create(file.path(templates_dir, "custom"))
  make_fake_pptx(file.path(templates_dir, "custom", "template_v_bar.pptx"))

  z <- tempfile(fileext = ".zip")
  tmpl_build_templates_zip(z, templates_dir)
  files <- utils::unzip(z, list = TRUE)$Name

  expect_setequal(files, c("template_line.pptx", "template_v_bar.pptx"))
})

test_that("tmpl_build_templates_zip still produces a valid zip when empty", {
  skip_if_not(nzchar(Sys.which("zip")), "system zip not available")
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  z <- tempfile(fileext = ".zip")
  tmpl_build_templates_zip(z, templates_dir)
  files <- utils::unzip(z, list = TRUE)$Name
  expect_true("README.txt" %in% files)
})
