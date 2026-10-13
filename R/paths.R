#' Paths into the phipflow results
#'
#' Results are linked in place, never copied. Paths passed to the helpers are
#' relative to `results_dir` of `report_config.yml`, e.g.
#' `file.path(cmp_base(cmp), "enrichment_counts.svg")`.
#'
#' * `res_fs()` is where the file is on disk, for reading it while rendering.
#' * `res_web()` is the link written into the page. Pages live in `_book/`, one
#'   level below the book directory, hence the leading `..`.
#' * `use_file()` returns the link for an existing file and records it in the
#'   manifest that `noxreports bundle` packs; `NULL` when the file is missing.
#' * `cmp_base()` is a comparison's directory, `<group>/<g1>_vs._<g2>`.
#'
#' @param rel Path relative to the results directory.
#' @param cmp One element of `book$comparisons`.
#' @param book Book loaded by [load_book()].
#' @export
res_fs <- function(rel, book = current_book()) file.path(book$results_path, rel)

#' @rdname res_fs
#' @export
res_web <- function(rel, book = current_book()) {
  p <- file.path(book$results_dir, rel)
  if (!fs::is_absolute_path(p)) p <- file.path("..", p)
  utils::URLencode(p)
}

#' @rdname res_fs
#' @export
use_file <- function(rel, book = current_book()) {
  if (!file.exists(res_fs(rel, book))) return(NULL)
  .nox$refs <- union(.nox$refs, file.path(book$results_dir, rel))
  res_web(rel, book)
}

#' @rdname res_fs
#' @export
cmp_base <- function(cmp) file.path(cmp$group, cmp$dir)

manifest_write <- function(path) {
  writeLines(sort(unique(.nox$refs)), path)
}
