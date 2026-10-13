#' Create a report book from phipflow results
#'
#' Copies the book template into `out`, writes `report_config.yml` from what it
#' finds in `results` and `metadata`, and leaves optional chapters disabled
#' (renamed with a leading `_`, which bookdown skips) when their data is
#' missing: `00-demographics.Rmd` without age and sex columns, `02-stability.Rmd`
#' without a stability analysis.
#'
#' @param metadata phipflow metadata CSV (first column = sample IDs).
#' @param results phipflow results directory (`<project>/<results_name>`).
#' @param project Project name; defaults to the directory above `results`.
#' @param out Book directory; defaults to `<project>_book` next to `results`.
#' @param title,author Shown on the first page.
#' @param age_col,sex_col Metadata columns for age and sex; found automatically
#'   (case-insensitive `age`, `sex` or `gender`) when `NULL`.
#' @param stability "auto" enables the stability pages for comparisons with
#'   phipflow similarity output; "no" disables them.
#' @param top_n DELTA features shown per rank and direction.
#' @param min_m_eff phipflow's `delta_min_m_eff`; read from the DELTA file
#'   names when `NULL`.
#' @param features CSV of features to always show, with columns `comparison`
#'   (id or directory name), `feature` and optionally `rank`.
#' @param force Overwrite the template files and config of an existing book.
#'   Other files in the book directory are kept.
#' @export
nox_init <- function(metadata, results = "results", project = NULL, out = NULL,
                     title = NULL, author = "", age_col = NULL, sex_col = NULL,
                     stability = c("auto", "no"), top_n = 15L, min_m_eff = NULL,
                     features = NULL, force = FALSE) {
  stability <- match.arg(stability)
  results <- fs::path_abs(results)
  metadata <- fs::path_abs(metadata)
  if (!dir.exists(results)) stop("Results directory not found: ", results, call. = FALSE)
  if (!file.exists(metadata)) stop("Metadata file not found: ", metadata, call. = FALSE)
  project <- project %||% basename(fs::path_dir(results))
  out <- fs::path_abs(out %||% file.path(fs::path_dir(results), paste0(project, "_book")))
  if (dir.exists(out) && !force) {
    stop(out, " already exists. Edit it and run `noxreports render`, or pass --force ",
         "to overwrite its template files and report_config.yml.", call. = FALSE)
  }

  meta <- data.table::fread(metadata, colClasses = "character")
  cmps <- discover_comparisons(results)
  if (!length(cmps)) {
    stop("No comparison directories (<group>/<g1>_vs._<g2>) found in ", results, call. = FALSE)
  }
  cmp_paths <- vapply(cmps, function(x) file.path(results, x$group, x$dir), character(1))

  columns <- list(
    age = detect_column(meta, age_col, "^age$", "age"),
    sex = detect_column(meta, sex_col, "^(sex|gender)$", "sex"),
    subject = if ("subject_id" %in% names(meta)) "subject_id" else NULL
  )

  ids <- slugify(vapply(cmps, `[[`, "", "dir"))
  dup <- ids %in% ids[duplicated(ids)]
  ids[dup] <- paste(slugify(vapply(cmps[dup], `[[`, "", "group")), ids[dup], sep = "-")

  labels <- unique(unlist(lapply(cmps, function(x) c(x$g1, x$g2))))
  pretty <- function(x) gsub("_", " ", x)
  comparisons <- lapply(seq_along(cmps), function(i) {
    x <- cmps[[i]]
    list(id = ids[i], group = x$group, dir = x$dir, g1 = x$g1, g2 = x$g2,
         label = paste(pretty(x$g1), "vs", pretty(x$g2)), question = "",
         design = detect_design(cmp_paths[i]),
         stability = stability == "auto" && has_stability(cmp_paths[i]))
  })

  groups <- list()
  for (g in unique(vapply(cmps, `[[`, "", "group"))) {
    in_g <- Filter(function(x) x$group == g, cmps)
    g_labels <- unique(unlist(lapply(in_g, function(x) c(x$g1, x$g2))))
    group_col <- detect_group_col(meta, g_labels)
    if (is.null(group_col)) {
      message("No metadata column holds all labels of group '", g, "' (",
              paste(g_labels, collapse = ", "), "); its group sizes and demographics are left out.")
    }
    groups[[g]] <- list(group_col = group_col, all = dir.exists(file.path(results, g, "all")))
  }

  manual <- list()
  if (!is.null(features)) {
    f <- as.data.frame(data.table::fread(features, colClasses = "character"))
    if (!all(c("comparison", "feature") %in% names(f))) {
      stop("The features CSV needs the columns 'comparison' and 'feature'.", call. = FALSE)
    }
    dirs <- vapply(cmps, `[[`, "", "dir")
    for (i in seq_len(nrow(f))) {
      id <- if (f$comparison[i] %in% ids) f$comparison[i] else ids[match(f$comparison[i], dirs)]
      if (is.na(id)) stop("Unknown comparison in the features CSV: ", f$comparison[i], call. = FALSE)
      entry <- list(comparison = id, feature = f$feature[i])
      if ("rank" %in% names(f) && nzchar(f$rank[i])) entry$rank <- f$rank[i]
      manual[[length(manual) + 1]] <- entry
    }
  }

  min_m_eff <- min_m_eff %||% detect_min_m_eff(cmp_paths)
  if (min_m_eff == round(min_m_eff)) min_m_eff <- as.integer(min_m_eff)

  cfg <- list(
    project = project,
    results_dir = as.character(fs::path_rel(results, out)),
    metadata = as.character(fs::path_rel(metadata, out)),
    columns = columns,
    interactive = TRUE,
    group_labels = stats::setNames(as.list(pretty(labels)), labels),
    groups = groups,
    comparisons = comparisons,
    delta = list(min_m_eff = min_m_eff, top_n = as.integer(top_n), features = manual),
    fastqc = list(enabled = TRUE, qc_root = "/lisc/data/work/ccr/mariaDB/fastq_qc/json",
                  project = project, url = "https://ccr-lab.lisc.univie.ac.at")
  )

  disabled <- c(
    "00-demographics.Rmd" = is.null(columns$age) && is.null(columns$sex),
    "02-stability.Rmd" = !any(vapply(comparisons, `[[`, TRUE, "stability"))
  )
  deploy_template(out, disabled, list(
    title = yaml_string(title %||% paste(project, "PhIP-seq report")),
    title_html = htmltools::htmlEscape(title %||% paste(project, "PhIP-seq report")),
    author = yaml_string(author),
    book_filename = slugify(project)
  ))
  write_config(cfg, file.path(out, "report_config.yml"))

  n_paired <- sum(vapply(comparisons, function(x) identical(x$design, "paired"), TRUE))
  message("Created ", out, "\n",
          "  comparisons : ", length(comparisons), " (", n_paired, " paired)\n",
          "  stability   : ", sum(vapply(comparisons, `[[`, TRUE, "stability")), " comparison(s)\n",
          "  age / sex   : ", columns$age %||% "-", " / ", columns$sex %||% "-", "\n",
          "  min_m_eff   : ", cfg$delta$min_m_eff, ", top_n: ", top_n,
          ", manual features: ", length(manual), "\n",
          if (any(disabled)) paste0("  disabled    : _", names(disabled)[disabled], "\n", collapse = ""),
          "Edit report_config.yml if needed, then run: noxreports render ", out)
  invisible(out)
}

yaml_string <- function(x) gsub('(["\\\\])', "\\\\\\1", x)

deploy_template <- function(out, disabled, values) {
  src <- system.file("template", package = "noxreports")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  for (f in list.files(src, all.files = TRUE, no.. = TRUE)) {
    off <- isTRUE(disabled[f])
    target <- if (off) paste0("_", f) else f
    unlink(file.path(out, if (off) f else paste0("_", f)))
    text <- readLines(file.path(src, f), warn = FALSE, encoding = "UTF-8")
    for (k in names(values)) text <- gsub(paste0("{{", k, "}}"), values[[k]], text, fixed = TRUE)
    writeLines(text, file.path(out, target), useBytes = TRUE)
  }
}

# report_config.yml with a comment above each section.
write_config <- function(cfg, path) {
  as_yaml <- function(x) {
    yaml::as.yaml(x, indent.mapping.sequence = TRUE, handlers = list(
      logical = function(v) structure(ifelse(v, "true", "false"), class = "verbatim")
    ))
  }
  section <- function(comment, keys) {
    c(strwrap(comment, width = 78, prefix = "# "),
      sub("\n$", "", gsub(": ~\n", ": null\n", as_yaml(cfg[keys]))), "")
  }
  lines <- c(
    paste0("# Written by noxreports ", utils::packageVersion("noxreports"),
           ". Edit it, then run `noxreports render`."),
    "# Paths are relative to this directory.",
    "",
    section("Project name; phipflow results and metadata (first column = sample IDs)",
            c("project", "results_dir", "metadata")),
    section("Metadata columns for age, sex and the subject pairing (null = not available)",
            "columns"),
    section("false = static SVGs instead of interactive plotly HTML (much smaller bundle)",
            "interactive"),
    section("Display names of the group labels", "group_labels"),
    section(paste("phipflow groups (results subdirectories): the metadata column holding",
                  "their labels, and whether phipflow ran --all TRUE"), "groups"),
    section(paste("Comparisons in book order. label and question are free text;",
                  "stability: true adds a page in the Repertoire stability chapter"), "comparisons"),
    section(paste("DELTA features shown: those passing m_eff > min_m_eff and p_perm < 0.05,",
                  "top_n per rank and direction by phipflow's weighted_score, plus features",
                  "listed here as {comparison: <id>, feature: <name>, rank: <rank>}"), "delta"),
    section(paste("FastQC summary on the first page, read from noxqc's JSON files;",
                  "left out with a message where qc_root is not readable"), "fastqc")
  )
  writeLines(lines, path, useBytes = TRUE)
}
