test_that("phipflow_slug matches phipflow's plot file names", {
  expect_equal(phipflow_slug("uncultured Faecalibacterium sp."), "uncultured_Faecalibacterium_sp_")
  expect_equal(phipflow_slug("a_ b-c"), "a__b-c")
})

test_that("ids, numerals and formatting", {
  expect_equal(slugify("GroupA_Base_vs._GroupB_Week12"), "groupa-base-vs-groupb-week12")
  expect_equal(roman(c(1, 4, 12)), c("i", "iv", "xii"))
  expect_equal(fmt_p(c(0.0123, 1e-6, NA)), c("0.0123", "1.00e-06", "NA"))
  expect_equal(md_escape("a_b [c]"), "a\\_b \\[c\\]")
})

test_that("label_group relabels labels and direction strings", {
  book <- list(group_labels = list(A_x = "Group A", B_y = "Group B"))
  expect_equal(label_group(c("A_x", "More in B_y", "Other", NA), book),
               c("Group A", "More in Group B", "Other", NA))
})
