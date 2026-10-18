#' DELTA results of a comparison
#'
#' * `delta_table()` reads phipflow's `DELTA_framework/delta_table.csv` (every
#'   feature tested); `NULL` when the comparison has no DELTA output.
#' * `delta_features()` gives the features shown in the book: phipflow's own
#'   selection (see [select_top()]) plus the features listed under
#'   `delta: features` in `report_config.yml`. Extra columns: `weighted_score`,
#'   `source` ("top" or "manual"), `in_table` (found in the DELTA table),
#'   `has_plots` (phipflow wrote per-feature plots) and `slug` (for ids).
#' * `delta_counts()` gives the number of features tested, passing the filter
#'   and shown.
#'
#' @param cmp One element of `book$comparisons`.
#' @param book Book loaded by [load_book()].
#' @export
delta_table <- function(cmp, book = current_book()) {
  path <- res_fs(file.path(cmp_base(cmp), "DELTA_framework", "delta_table.csv"), book)
  cached(path, function() {
    if (file.exists(path)) as.data.frame(data.table::fread(path)) else NULL
  })
}

#' phipflow's DELTA feature selection
#'
#' Reproduces `02-run_phiper_analysis.R`: keep features with
#' `m_eff > min_m_eff` and `p_perm < 0.05`; within each rank score them as
#' `weighted_score = rescale01(|T_obs_stand|) * rescale01(max_delta)` (min-max
#' scaling within the rank, so scores compare features of one rank only);
#' take the `top_n` best-scoring features with `T_obs_stand < 0` and the
#' `top_n` with `T_obs_stand > 0`. Protein features without an annotation are
#' dropped, as phipflow does.
#'
#' `score_filtered()` returns every row passing the filter with its score;
#' `select_top()` the selected rows.
#'
#' @param d A DELTA table (see [delta_table()]).
#' @param min_m_eff Minimum effective number of peptides (exclusive).
#' @param top_n Features per rank and direction.
#' @export
select_top <- function(d, min_m_eff, top_n) {
  scored <- score_filtered(d, min_m_eff)
  picked <- lapply(split(scored, factor(scored$rank, levels = unique(scored$rank))), function(x) {
    neg <- x[!is.na(x$T_obs_stand) & x$T_obs_stand < 0, , drop = FALSE]
    pos <- x[!is.na(x$T_obs_stand) & x$T_obs_stand > 0, , drop = FALSE]
    rbind(utils::head(neg[order(-neg$weighted_score), , drop = FALSE], top_n),
          utils::tail(pos[order(pos$weighted_score), , drop = FALSE], top_n))
  })
  out <- do.call(rbind, c(list(scored[0, , drop = FALSE]), picked))
  rownames(out) <- NULL
  out
}

#' @rdname select_top
#' @export
score_filtered <- function(d, min_m_eff) {
  d <- d[!is.na(d$m_eff) & d$m_eff > min_m_eff & !is.na(d$p_perm) & d$p_perm < 0.05, , drop = FALSE]
  if ("protein_annotation" %in% names(d)) {
    blank <- function(v) is.na(v) | trimws(v) == ""
    d <- d[!(d$rank == "protein_seq_id" & (blank(d$protein_annotation) | blank(d$feature))), ,
           drop = FALSE]
  }
  d$weighted_score <- numeric(nrow(d))
  for (r in unique(d$rank)) {
    i <- d$rank == r
    d$weighted_score[i] <- rescale01(abs(d$T_obs_stand[i])) * rescale01(d$max_delta[i])
  }
  d
}

# helper_functions.R safe_rescale01() in phipflow
rescale01 <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(1, length(x)))
  (x - rng[1]) / diff(rng)
}

#' @rdname delta_table
#' @export
delta_features <- function(cmp, book = current_book()) {
  cached(paste0("features:", cmp$id), function() {
    d <- delta_table(cmp, book)
    manual <- Filter(function(f) identical(f$comparison, cmp$id), book$delta$features)
    if (is.null(d)) {
      if (length(manual)) message("Comparison ", cmp$id, " has no DELTA table; its manual features are left out.")
      return(NULL)
    }
    scored <- score_filtered(d, book$delta$min_m_eff)
    top <- select_top(d, book$delta$min_m_eff, book$delta$top_n)
    top <- top[order(top$rank, -top$weighted_score), , drop = FALSE]
    top$source <- rep("top", nrow(top))
    top$in_table <- rep(TRUE, nrow(top))

    rows <- list(top)
    for (f in manual) {
      hit <- d$feature == f$feature
      if ("protein_annotation" %in% names(d)) hit <- hit | (!is.na(d$protein_annotation) & d$protein_annotation == f$feature)
      if (!is.null(f$rank)) hit <- hit & d$rank == f$rank
      if (!any(hit)) {
        message("Manual feature '", f$feature, "' is not in the DELTA table of ", cmp$id, ".")
        row <- d[NA_integer_, , drop = FALSE]
        row$feature <- f$feature
        row$rank <- f$rank %||% NA_character_
        row$weighted_score <- NA_real_
        row$in_table <- FALSE
      } else {
        row <- d[hit, , drop = FALSE]
        row <- row[which.max(abs(row$T_obs_stand)), , drop = FALSE]
        key <- paste(row$rank, row$feature)
        if (key %in% paste(top$rank, top$feature)) next
        s <- scored$weighted_score[paste(scored$rank, scored$feature) == key]
        row$weighted_score <- if (length(s)) s[1] else NA_real_
        row$in_table <- TRUE
      }
      row$source <- "manual"
      rows[[length(rows) + 1]] <- row
    }
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out$has_plots <- file.exists(res_fs(feature_plot(cmp, out$feature, "scatter", FALSE), book))
    out$slug <- make.unique(slugify(paste(out$rank, out$feature)), sep = "-")
    out
  })
}

#' @rdname delta_table
#' @export
delta_counts <- function(cmp, book = current_book()) {
  d <- delta_table(cmp, book)
  f <- delta_features(cmp, book)
  if (is.null(d)) return(c(tested = NA, passing = NA, shown = NA, manual = NA))
  c(tested = nrow(d), passing = nrow(score_filtered(d, book$delta$min_m_eff)),
    shown = nrow(f), manual = sum(f$source == "manual"))
}

#' Number of DELTA permutations per feature
#'
#' Read from phipflow's `DELTA_framework/log.txt` ("B per contrast").
#'
#' @inheritParams delta_table
#' @export
delta_permutations <- function(cmp, book = current_book()) {
  log <- res_fs(file.path(cmp_base(cmp), "DELTA_framework", "log.txt"), book)
  if (!file.exists(log)) return(NA_integer_)
  head <- readLines(log, n = 20, warn = FALSE)
  hit <- regmatches(head, regexpr("B per contrast: [0-9]+", head))
  if (!length(hit)) NA_integer_ else as.integer(sub("\\D+", "", hit[1]))
}

#' Feature pages
#'
#' * `feature_title()`: the feature name; protein features show their
#'   annotation with the protein ID in brackets.
#' * `feature_id()`: the HTML id of the feature's section.
#' * `feature_plot()`: path of one of phipflow's per-feature plots (`type`
#'   "profile", "scatter", "deltaplot" or "ecdf"; only the scatter has an
#'   interactive version, and only protein features have a profile plot).
#' * `feature_card()`: the statistics box.
#' * `feature_tabs()`: the plots in tabs, with a link back to the DELTA
#'   overview.
#' * `feature_table()`: the DELTA overview table of a comparison.
#'
#' @param row One row of [delta_features()].
#' @param cmp One element of `book$comparisons`.
#' @param feature Feature name(s) as in the DELTA table.
#' @param type Plot type.
#' @param interactive Interactive HTML instead of static SVG.
#' @param book Book loaded by [load_book()].
#' @export
feature_title <- function(row) {
  ann <- if ("protein_annotation" %in% names(row)) row$protein_annotation else NA
  ifelse(!is.na(ann) & nzchar(ann) & row$rank == "protein_seq_id",
         paste0(ann, " (", row$feature, ")"), row$feature)
}

#' @rdname feature_title
#' @export
feature_id <- function(cmp, row) paste0("feat-", cmp$id, "-", row$slug)

#' @rdname feature_title
#' @export
feature_plot <- function(cmp, feature, type = c("scatter", "deltaplot", "ecdf", "profile"), interactive = FALSE) {
  type <- match.arg(type)
  suffix <- switch(type,
    scatter = if (interactive) "_scatter_interactive.html" else "_scatter_static.svg",
    deltaplot = "_deltaplot_static.svg",
    ecdf = "_ecdfplot_static.svg",
    profile = "_profileplot_static.svg")
  file.path(cmp_base(cmp), "DELTA_framework", "interesting_features",
            sprintf("%s%s", phipflow_slug(feature), suffix))
}

p_class <- function(p) {
  ifelse(is.na(p), "p-unknown", ifelse(p < 0.01, "p-strong", ifelse(p < 0.05, "p-sig", "p-nonsig")))
}

#' @rdname feature_title
#' @export
feature_card <- function(row, book = current_book()) {
  esc <- htmltools::htmlEscape
  if (!isTRUE(row$in_table)) {
    return(sprintf(paste0('<div class="feature-card feature-card-missing"><strong>%s</strong>: ',
                          'listed in report_config.yml but not found in the DELTA table.</div>'),
                   esc(row$feature)))
  }
  p <- as.numeric(row$p_perm)
  badge <- if (is.na(p)) "badge-unknown" else if (p < 0.05) "badge-sig" else "badge-nonsig"
  ann <- if ("protein_annotation" %in% names(row)) row$protein_annotation else NA
  items <- c(
    sprintf("<li><strong>Feature:</strong> %s (rank <code>%s</code>, %s peptides, m<sub>eff</sub> = %s)</li>",
            esc(row$feature), esc(row$rank), row$n_peptides_used, fmt_num(row$m_eff, 1)),
    if (!is.na(ann) && nzchar(ann)) sprintf("<li><strong>Protein annotation:</strong> %s</li>", esc(ann)),
    sprintf('<li><strong>Permutation p-value:</strong> <span class="badge %s">p_perm = %s</span> (uncorrected)</li>',
            badge, fmt_p(p)),
    sprintf('<li><strong>Standardized Stouffer statistic:</strong> <span class="stat-pill">T_obs_stand = %s</span>: <strong>%s</strong></li>',
            fmt_num(row$T_obs_stand, 3), esc(label_group(row$direction, book))),
    sprintf("<li><strong>Max peptide-level &Delta;:</strong> <code>%s</code>; fraction of peptides with positive &Delta;: <code>%s</code></li>",
            fmt_num(row$max_delta, 3), fmt_num(row$frac_delta_pos, 3)),
    if (!is.na(row$weighted_score)) sprintf("<li><strong>Weighted score</strong> (within rank): <code>%s</code></li>",
                                            fmt_num(row$weighted_score, 3)),
    if (row$source == "manual") "<li><em>Added by hand in report_config.yml.</em></li>"
  )
  paste0('<div class="feature-card"><ul>', paste(items, collapse = ""), "</ul></div>")
}

#' @rdname feature_title
#' @export
feature_tabs <- function(cmp, row, book = current_book()) {
  back <- sprintf('<div class="back-link"><a href="#row-%s">Back to the DELTA overview</a></div>',
                  feature_id(cmp, row))
  if (!isTRUE(row$in_table)) return(back)
  if (!isTRUE(row$has_plots)) {
    return(paste0(callout(sprintf(paste0(
      "phipflow writes per-feature plots only for features with m<sub>eff</sub> &gt; %s and ",
      "p_perm &lt; 0.05, so there are none for this one."), book$delta$min_m_eff)), back))
  }
  name <- paste0("tabs-", feature_id(cmp, row))
  tabs <- c(scatter = "Scatterplot", deltaplot = "&#916;-plot", ecdf = "ECDF plot")
  # phipflow writes a profile plot for protein features only; it leads the tabs
  if (file.exists(res_fs(feature_plot(cmp, row$feature, "profile"), book))) {
    tabs <- c(profile = "Profile plot", tabs)
  }
  plot_of <- function(tp) {
    alt <- paste(tabs[[tp]], row$feature)
    if (tp == "scatter") {
      nox_plot(feature_plot(cmp, row$feature, tp, TRUE), feature_plot(cmp, row$feature, tp), alt, book)
    } else {
      nox_img(feature_plot(cmp, row$feature, tp), alt, book)
    }
  }
  paste0(
    '<div class="ph-tabs">',
    paste0(sprintf('<input type="radio" class="ph-tab-input" data-tab="%s" name="%s" id="%s-%s"%s>',
                   names(tabs), name, name, names(tabs),
                   c(" checked", rep("", length(tabs) - 1L))), collapse = ""),
    '<div class="ph-tabs-nav">',
    paste0(sprintf('<label class="ph-tab-label" for="%s-%s" data-tab="%s">%s</label>',
                   name, names(tabs), names(tabs), tabs), collapse = ""),
    '</div><div class="ph-tabs-panels">',
    paste0(vapply(names(tabs), function(tp) {
      sprintf('<div class="ph-tab-panel" data-tab="%s">%s</div>', tp, plot_of(tp))
    }, ""), collapse = ""),
    "</div></div>", back)
}

#' @rdname feature_title
#' @export
feature_table <- function(cmp, book = current_book()) {
  f <- delta_features(cmp, book)
  if (is.null(f)) return(no_plot("No DELTA results for this comparison."))
  if (!nrow(f)) {
    return(callout(sprintf(paste0("<strong>No features</strong> with m<sub>eff</sub> &gt; %s and ",
                                  "p_perm &lt; 0.05 in this comparison."), book$delta$min_m_eff), "none"))
  }
  esc <- htmltools::htmlEscape
  ids <- feature_id(cmp, f)
  manual <- ifelse(f$source == "manual", ' <span class="badge badge-unknown">manual</span>', "")
  rows <- sprintf(paste0('<tr id="row-%s"><td><a href="#%s">%s</a>%s</td><td>%s</td><td>%s</td>',
                         '<td>%s</td><td class="%s">%s</td><td>%s</td><td>%s</td><td>%s</td></tr>'),
                  ids, ids, esc(feature_title(f)), manual, esc(f$rank),
                  ifelse(is.na(f$n_peptides_used), "NA", f$n_peptides_used),
                  fmt_num(f$T_obs_stand, 3), p_class(f$p_perm), fmt_p(f$p_perm), fmt_num(f$max_delta, 3),
                  fmt_num(f$weighted_score, 3), esc(label_group(f$direction, book)))
  paste0('<table class="summary-table"><thead><tr><th>Feature</th><th>Rank</th><th>Peptides</th>',
         '<th>T_obs_stand</th><th>p_perm</th><th>max_delta</th><th>Score</th><th>Direction</th>',
         '</tr></thead><tbody>', paste(rows, collapse = ""), "</tbody></table>")
}
