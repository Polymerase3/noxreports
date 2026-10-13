#' Zip a rendered book for sharing
#'
#' Packs `_book/` and the results files listed in its manifest (only those the
#' pages link to) into one zip. Paths inside the zip start at the directory
#' holding both the book and the results, so the links work once unzipped:
#' open `<project>/<project>_book/_book/index.html`.
#'
#' @param book_dir Book directory, rendered with [nox_render()].
#' @param out Zip file; defaults to `<book_dir>/<book name>.zip`.
#' @export
nox_bundle <- function(book_dir = ".", out = NULL) {
  book_dir <- fs::path_abs(book_dir)
  manifest <- file.path(book_dir, "_book", "noxreports_manifest.txt")
  if (!file.exists(manifest)) {
    stop("No ", manifest, "; render the book with `noxreports render` first.", call. = FALSE)
  }
  book <- load_book(file.path(book_dir, "report_config.yml"))
  refs <- as.character(fs::path_norm(file.path(book_dir, readLines(manifest))))
  if (length(missing <- refs[!file.exists(refs)])) {
    stop("Linked results files are missing (re-render the book):\n", paste(missing, collapse = "\n"),
         call. = FALSE)
  }
  pages <- list.files(file.path(book_dir, "_book"), recursive = TRUE, full.names = TRUE)
  root <- fs::path_dir(fs::path_common(c(book_dir, book$results_path)))
  out <- fs::path_abs(out %||% file.path(book_dir, paste0(basename(book_dir), ".zip")))
  unlink(out)
  files <- as.character(fs::path_rel(c(pages, refs), root))
  zip::zip(out, files = files, root = root, mode = "mirror")
  message(sprintf("Wrote %s (%d files, %.1f MB). Unzip it and open %s", out, length(files),
                  file.size(out) / 1e6, fs::path_rel(file.path(book_dir, "_book", "index.html"), root)))
  invisible(out)
}
