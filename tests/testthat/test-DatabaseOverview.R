test_that("DatabaseOverview: creation", {
  databaseOverview <- DatabaseOverview$new("CPRD GOLD", "IPCI", "SIDIAP")

  expect_equal(nrow(databaseOverview$table$args$data), 3)
})
