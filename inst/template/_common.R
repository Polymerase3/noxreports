# Runs before every chapter (before_chapter_script in _bookdown.yml), so any
# .Rmd you add gets the same setup: the noxreports helpers and `book`, the
# parsed report_config.yml with the metadata (book$meta) and the comparisons
# (book$comparisons). Messages and warnings go to the render log, not the book.
library(noxreports)
knitr::opts_chunk$set(echo = FALSE, message = NA, warning = NA)
book <- load_book("report_config.yml")
