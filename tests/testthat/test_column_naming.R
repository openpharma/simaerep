test_that("accept non-standard column names via simaerep col_names argument", {

  df_visit <- get_df_visit_test() %>%
    dplyr::rename(
      personX = patient_id,
      studyX = study_id,
      siteX = site_id,
      vsX = visit,
      n_ae = n_event
    )

  # inframe ----------------------------------------------------------------
  evrep <- simaerep(
    df_visit,
    col_names = c(
      study_id = "studyX",
      site_id = "siteX",
      patient_id = "personX",
      visit = "vsX"
    ),
    event_names = "ae"
  )

  expect_true(
    all(
      c("personX", "studyX", "siteX", "vsX", "n_ae") %in%
      colnames(as.data.frame(evrep$visit))
    )
  )

  expect_true(all(c("studyX", "siteX") %in% colnames(evrep$df_eval)))
  expect_true(all(c("studyX", "siteX") %in% colnames(evrep$df_site)))
  expect_true(all(c("studyX", "siteX") %in% colnames(evrep$df_sim_sites)))

  expect_s3_class(plot(evrep, study = "A"), "ggplot")
  expect_s3_class(plot(evrep, what = "med75", verbose = FALSE, study = "A"), "ggplot")

  # classic ----------------------------------------------------------------
  evrep <- simaerep(
    df_visit,
    col_names = c(
      study_id = "studyX",
      site_id = "siteX",
      patient_id = "personX",
      visit = "vsX"
    ),
    event_names = "ae",
    inframe = FALSE
  )

  expect_true(
    all(
      c("personX", "studyX", "siteX", "vsX", "n_ae") %in%
        colnames(as.data.frame(evrep$visit))
    )
  )

  expect_true(all(c("studyX", "siteX") %in% colnames(evrep$df_eval)))
  expect_true(all(c("studyX", "siteX") %in% colnames(evrep$df_site)))
  expect_true(all(c("studyX", "siteX") %in% colnames(evrep$df_sim_sites)))

  expect_s3_class(plot(evrep, study = "A"), "ggplot")
  expect_s3_class(plot(evrep, what = "med75", verbose = FALSE, study = "A"), "ggplot")

})


test_that("simaerep event_names not necessary when there is only one event column", {

  # inframe
  df_visit <- get_df_visit_test() %>%
    rename(
      n_potatoe = n_event
    )

  evrep <- simaerep(df_visit)

  expect_true(all(
    c("potatoe_count", "potatoe_per_visit_site", "potatoe_per_visit_study", "potatoe_prob", "potatoe_delta") %in%
    colnames(evrep$df_eval)
  ))

  # classic
  evrep <- simaerep(df_visit, inframe = FALSE, under_only = FALSE)

  expect_true(all(
    c("mean_potatoe_site_med75", "mean_potatoe_study_med75") %in%
      colnames(evrep$df_eval)
  ))

})
