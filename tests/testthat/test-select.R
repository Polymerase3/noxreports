test_that("select_top reproduces phipflow's weighted_score selection", {
  d <- data.frame(
    rank = c(rep("species", 5), "genus", "protein_seq_id", "protein_seq_id"),
    feature = c("s1", "s2", "s3", "s4", "s5", "g1", "p1", "p2"),
    protein_annotation = c(rep(NA, 6), "ann", " "),
    m_eff = c(10, 10, 10, 10, 3, 10, 10, 10),
    p_perm = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01),
    T_obs_stand = c(4, 2, 3, -1, 5, 2, 2, 2),
    max_delta = c(0.4, 0.2, 0.1, 0.3, 0.9, 0.2, 0.2, 0.2)
  )
  s <- score_filtered(d, 5)
  # s5 fails m_eff, p2 has a blank annotation
  expect_setequal(s$feature, c("s1", "s2", "s3", "s4", "g1", "p1"))
  sp <- s[s$rank == "species", ]
  # |T| 4,2,3,1 -> 1,1/3,2/3,0 ; max_delta .4,.2,.1,.3 -> 1,1/3,0,2/3
  expect_equal(sp$weighted_score, c(1, 1 / 9, 0, 0))
  # a rank with one feature scores 1 (zero range)
  expect_equal(s$weighted_score[s$feature == "g1"], 1)

  top1 <- select_top(d, 5, 1)
  expect_setequal(paste(top1$rank, top1$feature), c("species s1", "species s4", "genus g1", "protein_seq_id p1"))
  expect_equal(nrow(select_top(d, 5, 15)), 6)
  expect_equal(nrow(select_top(d[0, ], 5, 15)), 0)
})
