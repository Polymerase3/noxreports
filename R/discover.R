# What `noxreports init` reads from a phipflow results directory. The layout is
# <results>/<group>/<g1>_vs._<g2>/ (one dir per comparison, named by phipflow
# 02-run_phiper_analysis.R) plus an optional <results>/<group>/all/.

discover_comparisons <- function(results) {
  out <- list()
  for (g in sort(list.dirs(results, full.names = FALSE, recursive = FALSE))) {
    dirs <- sort(list.dirs(file.path(results, g), full.names = FALSE, recursive = FALSE))
    for (d in dirs[grepl("_vs._", dirs, fixed = TRUE)]) {
      parts <- strsplit(d, "_vs._", fixed = TRUE)[[1]]
      out[[length(out) + 1]] <- list(group = g, dir = d, g1 = parts[1], g2 = parts[2])
    }
  }
  out
}

# "paired" / "unpaired" as recorded by DELTA; without a DELTA table, phipflow's
# similarity/ output (written for paired comparisons only) decides.
detect_design <- function(cmp_path) {
  delta <- file.path(cmp_path, "DELTA_framework", "delta_table.csv")
  if (file.exists(delta)) {
    return(data.table::fread(delta, nrows = 1, select = "design")$design[1])
  }
  if (dir.exists(file.path(cmp_path, "similarity"))) "paired" else "unpaired"
}

has_stability <- function(cmp_path) {
  file.exists(file.path(cmp_path, "similarity", "similarity_results.rds"))
}

# phipflow writes the filtered table as delta_table_m_eff_gt<N>_p_perm_lt0.05.csv
detect_min_m_eff <- function(cmp_paths) {
  pattern <- "^delta_table_m_eff_gt(.+)_p_perm_lt0\\.05\\.csv$"
  files <- unlist(lapply(file.path(cmp_paths, "DELTA_framework"), list.files, pattern = pattern))
  values <- unique(sub(pattern, "\\1", files))
  if (length(values) != 1) {
    stop("Could not determine phipflow's delta_min_m_eff from the DELTA tables (found: ",
         if (length(values)) paste(values, collapse = ", ") else "none",
         "). Pass it with --min-m-eff.", call. = FALSE)
  }
  as.numeric(values)
}

# Age and sex columns: the given name (must exist) or a case-insensitive match.
detect_column <- function(meta, given, pattern, what) {
  if (!is.null(given)) {
    if (!given %in% names(meta)) {
      stop("Column '", given, "' (", what, ") is not in the metadata.", call. = FALSE)
    }
    return(given)
  }
  hit <- grep(pattern, names(meta), ignore.case = TRUE, value = TRUE)
  if (!length(hit)) {
    message("No ", what, " column found in the metadata (pass --", what, "-col to name one).")
    return(NULL)
  }
  hit[1]
}

# The metadata column holding a phipflow group's labels: the first one that
# contains every label used in that group's comparisons.
detect_group_col <- function(meta, labels) {
  for (col in names(meta)[-1]) {
    if (all(labels %in% meta[[col]])) return(col)
  }
  NULL
}
