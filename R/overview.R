#' First-page summaries
#'
#' * `cohort_numbers()`: samples, subjects, groups and comparisons in the book.
#' * `comparison_table()`: one row per comparison with its design, PERMANOVA,
#'   stability and DELTA results, linking to the comparison's pages.
#'
#' @param book Book loaded by [load_book()].
#' @export
cohort_numbers <- function(book = current_book()) {
  s <- do.call(rbind, lapply(book$comparisons, comparison_samples, book = book))
  design <- vapply(book$comparisons, function(x) x$design %||% NA_character_, "")
  items <- c(
    if (!is.null(s)) sprintf("<strong>%d samples</strong> in the comparisons", length(unique(s$sample))),
    if (!is.null(s) && !all(is.na(s$subject))) sprintf("<strong>%d subjects</strong>", length(unique(s$subject))),
    sprintf("<strong>%d comparison(s)</strong> (%d paired, %d unpaired) in %d phipflow group(s)",
            length(design), sum(design %in% "paired"), sum(design %in% "unpaired"), length(book$groups)),
    sprintf("%d with a repertoire stability analysis",
            sum(vapply(book$comparisons, function(x) isTRUE(x$stability), TRUE)))
  )
  paste0("<ul>", paste0("<li>", items, "</li>", collapse = ""), "</ul>")
}

#' @rdname cohort_numbers
#' @export
comparison_table <- function(book = current_book()) {
  esc <- htmltools::htmlEscape
  rows <- vapply(book$comparisons, function(cmp) {
    pm <- permanova_result(cmp_base(cmp), book)
    sim <- if (isTRUE(cmp$stability)) similarity_summary(cmp, book)
    n <- delta_counts(cmp, book)
    sprintf(paste0("<tr><td>(%s)</td><td><a href=\"#general-%s\">%s</a></td><td>%s</td><td>%s</td>",
                   "<td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>"),
            roman(cmp$index), cmp$id, esc(cmp$label), esc(cmp$question %||% ""), esc(cmp$design),
            if (is.null(pm)) "NA" else sprintf("%s / %s", fmt_num(pm$R2, 3), fmt_p(pm$p_value)),
            if (is.null(sim)) "&ndash;" else sprintf("%.2fx (p = %s)", sim$ratio, fmt_p(sim$p_wilcox)),
            if (is.na(n[["tested"]])) "NA" else sprintf("%d / %d / %d", n[["tested"]], n[["passing"]], n[["shown"]]),
            if (is.na(n[["tested"]]) || n[["shown"]] == 0) "&ndash;" else sprintf('<a href="#delta-summary-%s">table</a>', cmp$id))
  }, "")
  paste0('<table class="summary-table"><thead><tr><th>#</th><th>Comparison</th><th>Question</th>',
         "<th>Design</th><th>PERMANOVA R&sup2; / p</th><th>Within / between similarity</th>",
         "<th>DELTA tested / passing / shown</th><th>Features</th></tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table>")
}
