#' Render a report book
#'
#' Runs bookdown in the book directory, writes the pages to `_book/` (removing
#' the previous ones) and lists the results files the pages link to in
#' `_book/noxreports_manifest.txt`, which [nox_bundle()] packs.
#'
#' @param book_dir Book directory created by [nox_init()].
#' @param quiet Passed to [bookdown::render_book()].
#' @export
nox_render <- function(book_dir = ".", quiet = TRUE) {
  book_dir <- fs::path_abs(book_dir)
  if (!file.exists(file.path(book_dir, "report_config.yml"))) {
    stop("No report_config.yml in ", book_dir, "; create the book with `noxreports init`.", call. = FALSE)
  }
  old <- setwd(book_dir)
  on.exit(setwd(old), add = TRUE)
  unlink("_book", recursive = TRUE)
  reset_state()
  bookdown::render_book("index.Rmd", envir = new.env(parent = globalenv()), quiet = quiet)
  manifest_write(file.path("_book", "noxreports_manifest.txt"))

  book <- load_book("report_config.yml")
  listed <- vapply(book$comparisons, function(x) file.path(x$group, x$dir), "")
  found <- vapply(discover_comparisons(book$results_path), function(x) file.path(x$group, x$dir), "")
  if (length(extra <- setdiff(found, listed))) {
    message("Not in report_config.yml (add them to show them): ", paste(extra, collapse = ", "))
  }
  message("Rendered ", file.path(book_dir, "_book", "index.html"))
  invisible(file.path(book_dir, "_book", "index.html"))
}
