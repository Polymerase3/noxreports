#' General analyses
#'
#' `base` is an analysis directory relative to the results directory: a
#' comparison ([cmp_base()]) or a group's `all` directory.
#'
#' * `permanova_table()`, `dispersion_table()`: phipflow's PERMANOVA and
#'   dispersion test results as report tables.
#' * `permanova_result()`: the first PERMANOVA row as a list (`R2`, `p_value`, ...).
#' * `pop_ranks()`: the ranks with POP plots, peptide level first.
#' * `group_sizes()`: samples and subjects per group of a comparison.
#' * `cmp_intro()`: question, design and group sizes, for the top of a page.
#'
#' @param base Analysis directory relative to the results directory.
#' @param cmp One element of `book$comparisons`.
#' @param book Book loaded by [load_book()].
#' @export
permanova_table <- function(base, book = current_book()) {
  t <- read_rds(file.path(base, "beta_diversity", "permanova_results.rds"), book)
  if (is.null(t)) return(no_plot("PERMANOVA results not available."))
  t <- as.data.frame(t)[, intersect(c("term", "F_stat", "R2", "p_value", "n_perm"), names(t)), drop = FALSE]
  nox_table(signif_cols(t))
}

#' @rdname permanova_table
#' @export
permanova_result <- function(base, book = current_book()) {
  t <- read_rds(file.path(base, "beta_diversity", "permanova_results.rds"), book)
  if (is.null(t) || !nrow(t)) return(NULL)
  as.list(as.data.frame(t)[1, ])
}

#' @rdname permanova_table
#' @export
dispersion_table <- function(base, book = current_book()) {
  t <- read_rds(file.path(base, "beta_diversity", "dispersion_results.rds"), book)$tests
  if (is.null(t)) return(no_plot("Dispersion results not available."))
  t <- as.data.frame(t)[, intersect(c("term", "p_value", "n_perm"), names(t)), drop = FALSE]
  nox_table(signif_cols(t))
}

#' @rdname permanova_table
#' @export
pop_ranks <- function(base, book = current_book()) {
  files <- list.files(res_fs(file.path(base, "POP_framework", "plots"), book), pattern = "_static\\.svg$")
  ranks <- sub("_static\\.svg$", "", files)
  c(intersect("peptide_id", ranks), setdiff(ranks, "peptide_id"))
}

#' @rdname permanova_table
#' @export
group_sizes <- function(cmp, book = current_book()) {
  s <- comparison_samples(cmp, book)
  if (is.null(s)) return("")
  items <- vapply(c(cmp$g1, cmp$g2), function(g) {
    x <- s[s$group == g, , drop = FALSE]
    subj <- if (all(is.na(x$subject))) "" else sprintf(" / %d subjects", length(unique(x$subject)))
    sprintf("<li><strong>%s:</strong> %d samples%s</li>",
            htmltools::htmlEscape(label_group(g, book)), nrow(x), subj)
  }, "")
  paste0("<ul>", paste(items, collapse = ""), "</ul>")
}

#' @rdname permanova_table
#' @export
cmp_intro <- function(cmp, book = current_book()) {
  q <- if (nzchar(cmp$question %||% "")) paste0("*", md_escape(cmp$question), "* &mdash; ") else ""
  paste0(q, "design: **", cmp$design, "**\n\n", group_sizes(cmp, book), "\n\n")
}

read_rds <- function(rel, book) {
  path <- res_fs(rel, book)
  cached(path, function() if (file.exists(path)) readRDS(path) else NULL)
}

signif_cols <- function(df, digits = 4) {
  num <- vapply(df, is.numeric, TRUE)
  df[num] <- lapply(df[num], signif, digits = digits)
  df
}
