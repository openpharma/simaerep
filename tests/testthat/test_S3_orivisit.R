# test data is automatically loaded, check ./data-raw/generate_test_data.R

test_that("orivisit uses less memory than original visit data", {
  expect_true(object.size(visit_test) < object.size(df_visit_test))
})

test_that("orivisit str_call must not be NA", {
  expect_false(is.na(visit_test$str_call))
})


test_that("orivisit must retrieve original visit data from parent environment", {
  df_vs_env <- as.data.frame(visit_test)
  expect_equal(df_vs_env, df_visit_test)
})

test_that("orivisit str_call must be NA when expression is passed instead of variable", {
  visit <- orivisit(df_visit_test[1:10, ])
  expect_true(is.na(visit$str_call))
})

test_that("orivisit must throw error when str_call is NA when retrieving original data", {
  visit <- orivisit(df_visit_test[1:10, ])
  expect_error(
    df_vs_env <- as.data.frame(visit, env = environment()),
    regexp = "Could not find original visit data in parent environment."
  )
})

test_that("print.orivisit generic must print object description", {
  expect_snapshot(print(visit_test))
})

test_that("is_orivisit returns TRUE", {
  expect_true(is_orivisit(visit_test))
  expect_false(is_orivisit(LETTERS))
})


test_that("as.data.frame.orivisit() stops when df_summary doesn't equal x$df_summary", {
  x <- orivisit(df_visit_test)
  x$df_summary$n_sites <- 10000
  message <- paste(
    "Could not find original visit data in parent environment.",
    "Please pass original visit data to function call."
  )
  expect_error(as.data.frame.orivisit(x), regex = message)
  #expect_message did not appear to work, so used expect_error instead
  })


test_that("as.data.frame.orivisit() stops when df dimensions are not equal to x dimensions", {
  x <- orivisit(df_visit_test)
  x$dim <- as.integer(c(300, 300))
  message <- paste(
    "Could not find original visit data in parent environment.",
    "Please pass original visit data to function call."
  )
  expect_error(as.data.frame.orivisit(x), regex = message)
})
