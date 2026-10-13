#' Format p-values and numbers for display
#'
#' `fmt_p()` uses scientific notation below 1e-4 and three significant digits
#' otherwise; `fmt_num()` uses a fixed number of decimals. Both return "NA" for
#' missing values.
#'
#' @param x Numeric vector.
#' @param digits Number of decimals.
#' @export
fmt_p <- function(x) {
  vapply(as.numeric(x), function(p) {
    if (is.na(p)) "NA" else if (p < 1e-4) sprintf("%.2e", p) else as.character(signif(p, 3))
  }, character(1), USE.NAMES = FALSE)
}

#' @rdname fmt_p
#' @export
fmt_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

#' Identifiers and file names
#'
#' `slugify()` turns text into an HTML id (lower case, runs of other characters
#' become one hyphen, trimmed). `phipflow_slug()` reproduces the file-name
#' sanitizer phipflow uses for per-feature plots (`02-run_phiper_analysis.R`):
#' runs of characters outside `[A-Za-z0-9_-]` become one underscore and nothing
#' is trimmed, so "uncultured Faecalibacterium sp." becomes
#' "uncultured_Faecalibacterium_sp_".
#'
#' @param x Character vector.
#' @export
slugify <- function(x) {
  x <- gsub("[^a-z0-9]+", "-", tolower(x))
  gsub("^-+|-+$", "", x)
}

#' @rdname slugify
#' @export
phipflow_slug <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

#' Escape text for use in Markdown headings and inline text
#'
#' @param x Character vector.
#' @export
md_escape <- function(x) gsub("([\\\\`*_{}\\[\\]#<>|])", "\\\\\\1", x, perl = TRUE)

#' Lower-case roman numerals
#'
#' @param i Integer vector.
#' @export
roman <- function(i) tolower(as.character(utils::as.roman(i)))

#' Display names for group labels
#'
#' Looks labels up in `group_labels` of `report_config.yml`. phipflow's DELTA
#' `direction` strings ("More in <label>") keep their prefix and get the rest
#' relabelled. Labels without an entry are returned unchanged.
#'
#' @param x Character vector of group labels or direction strings.
#' @param book Book loaded by [load_book()].
#' @export
label_group <- function(x, book = current_book()) {
  labels <- unlist(book$group_labels)
  x <- as.character(x)
  prefix <- ifelse(!is.na(x) & startsWith(x, "More in "), "More in ", "")
  core <- substring(x, nchar(prefix) + 1)
  hit <- !is.na(core) & core %in% names(labels)
  core[hit] <- labels[core[hit]]
  ifelse(is.na(x), NA_character_, paste0(prefix, core))
}

#' Comparison titles and headings
#'
#' `cmp_title()` gives "(i) label", numbered by the comparison's position in
#' `report_config.yml`. `cmp_heading()` gives a Markdown heading with the id
#' `<prefix>-<comparison id>`, for `cat()` in a `results='asis'` chunk.
#'
#' @param cmp One element of `book$comparisons`.
#' @param level Heading level; level 1 starts a new page.
#' @param prefix Id prefix, unique per chapter (e.g. "general").
#' @export
cmp_title <- function(cmp) paste0("(", roman(cmp$index), ") ", cmp$label)

#' @rdname cmp_title
#' @export
cmp_heading <- function(cmp, level, prefix) {
  paste0("\n\n", strrep("#", level), " ", md_escape(cmp_title(cmp)),
         " {#", prefix, "-", cmp$id, "}\n\n")
}
