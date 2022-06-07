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
