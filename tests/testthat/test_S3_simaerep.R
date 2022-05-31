source("simaerep_exec.R")

vs <- df_visit

aerep <- simaerep(vs)

test_that("print.simaerep generic must print object description", {
  expect_snapshot(print(aerep))
})

test_that("is_simaerep returns TRUE", {
  expect_true(is_simaerep(aerep))
  expect_false(is_simaerep(LETTERS))
})

test_that("simaerep must retrieve original visit data from parent environment", {
  df_vs_env <- as.data.frame(aerep$visit)
  expect_equal(df_vs_env, df_visit)
})

test_that("plot.simaerep with what='ur'", {
  expect_s3_class(plot(aerep, what = "ur", study = "A"), "ggplot")
})

test_that("plot.simaerep with what='med75'", {
  expect_s3_class(plot(aerep, what = "med75", study = "A", verbose = FALSE), "ggplot")
})


test_that("plot.simaerep throws error when original visit data cannot be retrieved", {
  aerep <- simaerep(vs[vs$site_number != "S0001", ])
  expect_error(plot(aerep, study = "A"))

  vs_filt <- vs[vs$site_number != "S0001", ]

  expect_s3_class(
    plot(aerep, study = "A", what = "ur", df_visit = vs_filt),
    "ggplot"
  )

  expect_s3_class(
    plot(aerep, study = "A", what = "med75", df_visit = vs_filt, verbose = FALSE),
    "ggplot"
  )
})
