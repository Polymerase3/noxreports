test_that("parse_args handles commands, options, flags and positionals", {
  p <- parse_args(c("init", "--metadata", "m.csv", "--top-n=5", "--force"))
  expect_equal(p$command, "init")
  expect_equal(p$options, list(metadata = "m.csv", `top-n` = "5", force = TRUE))
  p <- parse_args(c("bundle", "book", "--out", "x.zip"))
  expect_equal(p$positional, "book")
  expect_equal(p$options$out, "x.zip")
  expect_error(parse_args(c("init", "--metadata")), "needs a value")
})

test_that("run_cli rejects unknown commands and options", {
  expect_error(run_cli("nope"), "Unknown command")
  expect_error(run_cli(c("render", "--top-n", "3")), "Unknown option")
})
