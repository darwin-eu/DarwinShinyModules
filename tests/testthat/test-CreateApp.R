test_that("CreateApp", {
  skip_if_not_installed(c("usethis"))

  appPath <- file.path(tempdir(), "app")

  base <- Text$new("**base**")
  nested_a <- Text$new("**nested A**")
  nested_b <- Text$new("**nested B**")
  sub_a <- Text$new("**sub A**")
  sub_b <- Text$new("**sub B**")
  comb_a <- Text$new("**comb A**")
  comb_b <- Text$new("**comb B**")
  comb_c <- Text$new("**comb C**")

  appStructure <- list(
    base = base,
    nested = list(nested_a, nested_b),
    nested_sub = list(
      sub_a = sub_a,
      sub_b = sub_b
    ),
    nested_combined = list(
      comb_a_b = list(comb_a, comb_b),
      comb_c = comb_c
    )
  )

  createApp(path = appPath,
            appStructure = appStructure,
            theme = 'shinymodules-darwin',
            additionalFiles = c("test-CreateApp.R"))

  expect_true(dir.exists(appPath))
  expect_true(file.exists(file.path(appPath, "app.R")))
  expect_true(file.exists(file.path(appPath, "appStructure.rds")))
  expect_true(file.exists(file.path(appPath, "R", "test-CreateApp.R")))

  unlink(appPath)
})
