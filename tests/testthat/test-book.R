init_fixture <- function(p, ...) {
  suppressMessages(nox_init(file.path(p$project, "Metadata", "meta.csv"), results = p$results, ...))
}

test_that("init detects comparisons, design, stability, columns and min_m_eff", {
  p <- make_project()
  out <- init_fixture(p)
  expect_equal(basename(out), "proj_book")
  cfg <- yaml::read_yaml(file.path(out, "report_config.yml"))
  expect_equal(cfg$results_dir, "../results")
  expect_equal(vapply(cfg$comparisons, `[[`, "", "dir"), c("A_vs._B", "A_vs._C"))
  expect_equal(vapply(cfg$comparisons, `[[`, "", "design"), c("unpaired", "paired"))
  expect_equal(vapply(cfg$comparisons, `[[`, TRUE, "stability"), c(FALSE, TRUE))
  expect_equal(cfg$columns, list(age = "Age", sex = "Sex", subject = "subject_id"))
  expect_equal(cfg$groups$Cond$group_col, "Cond")
  expect_equal(cfg$delta$min_m_eff, 5)
  expect_true(file.exists(file.path(out, "02-stability.Rmd")))
  expect_error(init_fixture(p), "already exists")
})

test_that("init disables chapters without data and validates options", {
  p <- make_project()
  out <- init_fixture(p, stability = "no")
  expect_true(file.exists(file.path(out, "_02-stability.Rmd")))
  expect_false(file.exists(file.path(out, "02-stability.Rmd")))
  expect_error(init_fixture(p, out = file.path(p$dir, "b2"), sex_col = "Gender"), "not in the metadata")
  file.remove(list.files(p$results, "_p_perm_lt0.05.csv$", recursive = TRUE, full.names = TRUE))
  expect_error(init_fixture(p, out = file.path(p$dir, "b3")), "--min-m-eff")
})

test_that("demographics follow phipflow's paired subject filter", {
  p <- make_project()
  book <- load_book(file.path(init_fixture(p), "report_config.yml"))
  s <- comparison_samples(book$comparisons[[2]], book)
  expect_setequal(s$subject, c("s1", "s2"))
  expect_equal(nrow(comparison_samples(book$comparisons[[1]], book)), 6)
  expect_match(demographics_tests(book$comparisons[[2]], book), "same subjects")
  expect_match(demographics_tests(book$comparisons[[1]], book), "Fisher")
})

test_that("FastQC summary reads noxqc JSONs or is left out", {
  p <- make_project()
  out <- init_fixture(p)
  cfg <- file.path(out, "report_config.yml")
  book <- load_book(cfg)
  book$fastqc$qc_root <- p$qc
  qc <- read_fastqc(book)
  expect_equal(nrow(qc$samples), 2)
  expect_length(qc$missing, 7)
  expect_match(fastqc_tile(book), "2 of 9 samples")
  book$fastqc$qc_root <- file.path(p$dir, "nowhere")
  expect_message(expect_equal(fastqc_tile(book), ""), "not readable")
})

test_that("render and bundle produce a self-consistent zip", {
  skip_if_not(rmarkdown::pandoc_available())
  p <- make_project()
  out <- init_fixture(p, features = {
    f <- file.path(p$dir, "features.csv")
    writeLines(c("comparison,feature", "A_vs._B,Virus three"), f)
    f
  })
  suppressMessages(nox_render(out))
  pages <- list.files(file.path(out, "_book"), "\\.html$")
  expect_true(all(c("index.html", "stability-a-vs-c.html", "delta-a-vs-b.html") %in% pages))

  refs <- readLines(file.path(out, "_book", "noxreports_manifest.txt"))
  feat <- grep("interesting_features", refs, value = TRUE)
  # selected features only; the manual non-significant one has no plots
  expect_false(any(grepl("protseq_2|Virus_three", feat)))
  expect_true(any(grepl("Virus_one_scatter_interactive.html", feat)))

  zip <- suppressMessages(nox_bundle(out))
  unz <- withr::local_tempdir()
  utils::unzip(zip, exdir = unz)
  book_dir <- file.path(unz, "proj", "proj_book", "_book")
  for (page in list.files(book_dir, "\\.html$", full.names = TRUE)) {
    html <- paste(readLines(page, warn = FALSE), collapse = "\n")
    links <- regmatches(html, gregexpr('(src|href)="\\.\\./[^"#]+"', html))[[1]]
    links <- sub('^(src|href)="(.*)"$', "\\2", links)
    expect_true(all(file.exists(file.path(book_dir, utils::URLdecode(links)))), info = basename(page))
  }
})
