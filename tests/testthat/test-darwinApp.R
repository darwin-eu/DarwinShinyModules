test_that("App Creation", {
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

  app <- darwinApp(appStructure)
  expect_equal(class(app), "shiny.appobj")
})
