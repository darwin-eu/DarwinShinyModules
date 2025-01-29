test_that("App Creation", {
  skip_if_not(rlang::is_installed(c("shinydashboard", "bslib")))

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

  app <- launchShinydashboardApp(appStructure)
  expect_equal(class(app), "shiny.appobj")

  app <- launchDarwinDashboardApp(appStructure)
  expect_equal(class(app), "shiny.appobj")

  app <- launchBslibApp(appStructure)
  expect_equal(class(app), "shiny.appobj")

  app <- launchDarwinBslibApp(appStructure)
  expect_equal(class(app), "shiny.appobj")
})
