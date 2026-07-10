test_that("calculate_cash_flows reproduces v0.2.5 golden masters", {
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

test_that("legacy fixed-payment flags reproduce the v0.2.4 golden master", {
  source(test_path("golden", "golden_fixtures.R"), local = TRUE)
  nm   <- names(golden_cases())[1]
  gf   <- test_path("golden", paste0(nm, "_legacy_fixedpay.rds"))
  skip_if_not(file.exists(gf), paste("missing legacy golden:", nm))
  golden <- readRDS(gf)

  args <- golden_cases()[[nm]]
  if (is.null(args$config$interest_on_starting_balance))
    args$config$interest_on_starting_balance <- FALSE
  if (is.null(args$config$credit_loss_reduces_interest))
    args$config$credit_loss_reduces_interest <- TRUE
  args$config$reamortize_survivors <- FALSE

  current <- suppressWarnings(do.call(calculate_cash_flows, args))

  strip_dates <- function(x) {
    drop1 <- function(d) d[setdiff(names(d), c("date", "eff_date"))]
    if (is.data.frame(x)) drop1(x) else lapply(x, drop1)
  }
  # Amounts must match v0.2.4 exactly; dates legitimately shifted to t = 1
  expect_equal(strip_dates(current), strip_dates(golden), tolerance = 1e-8)
})
