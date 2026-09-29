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

test_that("fixed-payment output preserves historical rows before the corrected payoff", {
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

  # Keep the historical fixture unchanged. Each loan matches it until the old
  # post-prepayment cap binds. For this fixture, those rows are full payoffs:
  # independently reconcile the remaining principal instead of freezing the bug.
  for (id in unique(golden$LOAN_ID)) {
    expected <- golden[golden$LOAN_ID == id, ]
    affected <- which(expected$scheduled_principal + expected$prepayment >
                        expected$adjusted_balance + 1e-8)
    if (length(affected) > 0L) {
      k <- affected[1]
      expected <- expected[seq_len(k), ]
      payoff <- expected$starting_balance[k] - expected$credit_loss[k]
      expect_equal(expected$scheduled_principal[k] + expected$prepayment[k],
                   payoff, tolerance = 1e-8)
      delta <- payoff - expected$total_principal[k]
      expected$total_principal[k] <- payoff
      expected$remaining_balance[k] <- 0
      # Fixture uses 100% investor ownership, so principal changes pass through.
      for (col in c("total_payment", "investor_principal", "investor_total")) {
        expected[[col]][k] <- expected[[col]][k] + delta
      }
    }
    # Historical de-minimis principal was included only in total_principal.
    expected$scheduled_principal <- expected$total_principal - expected$prepayment
    actual <- current[current$LOAN_ID == id, ]
    cols <- setdiff(names(expected), c("date", "eff_date"))
    expect_equal(actual[, cols], expected[, cols], tolerance = 1e-8, info = id)
  }
})
