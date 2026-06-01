test_that("calculate_cash_flows reproduces v0.2.3 golden masters", {
  source(test_path("golden", "golden_fixtures.R"), local = TRUE)
  cases <- golden_cases()

  for (nm in names(cases)) {
    golden_file <- test_path("golden", paste0(nm, ".rds"))
    skip_if_not(file.exists(golden_file), paste("missing golden:", nm))

    golden  <- readRDS(golden_file)
    current <- suppressWarnings(do.call(calculate_cash_flows, cases[[nm]]))

    expect_equal(current, golden, tolerance = 1e-8,
                 info = paste("golden case:", nm))
  }
})
