#' Load a book's configuration
#'
#' Reads `report_config.yml`, resolves its paths against the book directory and
#' reads the metadata CSV (all columns as text; the first column holds the
#' sample IDs). The result becomes the default `book` for every helper. Called
#' by `_common.R` before each chapter; repeated calls are cached until the file
#' changes.
#'
#' The returned list holds the YAML fields plus:
#' * `dir`: absolute path of the book directory;
#' * `results_path`: absolute path of the results directory;
#' * `meta`: the metadata as a data frame;
#' * `comparisons[[i]]$index`: position, used for the (i), (ii), ... numbering.
#'
#' @param config Path to `report_config.yml`.
#' @export
load_book <- function(config = "report_config.yml") {
  config <- fs::path_abs(config)
  key <- paste(config, file.mtime(config))
  if (identical(.nox$key, key)) return(.nox$book)

  book <- yaml::read_yaml(config)
  book$dir <- as.character(fs::path_dir(config))
  book$results_path <- as.character(fs::path_norm(file.path(book$dir, book$results_dir)))
  meta_path <- fs::path_norm(file.path(book$dir, book$metadata))
  book$meta <- as.data.frame(data.table::fread(meta_path, colClasses = "character"))
  for (i in seq_along(book$comparisons)) book$comparisons[[i]]$index <- i

  .nox$book <- book
  .nox$key <- key
  book
}

#' @rdname load_book
#' @export
current_book <- function() {
  if (is.null(.nox$book)) stop("No book loaded: call load_book() first.", call. = FALSE)
  .nox$book
}
