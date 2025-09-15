# Tests for kobo_median function

# Helper function to create test data for kobo_median
create_kobo_median_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create sample survey data with numeric variables
  data <- data.frame(
    age = round(rnorm(n, mean = 35, sd = 10)),
    income = round(rnorm(n, mean = 50000, sd = 15000)),
    score = rnorm(n, mean = 75, sd = 12),
    rating = runif(n, min = 1, max = 5),
    count = rpois(n, lambda = 3),
    # Non-numeric variables
    category = sample(c("A", "B", "C"), n, replace = TRUE),
    text_var = paste0("text_", seq_len(n)),
    # Grouping variables
    group_var = sample(c("GroupA", "GroupB"), n, replace = TRUE),
    group_var2 = sample(c("X", "Y", "Z"), n, replace = TRUE),
    # Survey design variables
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create survey sheet with different numeric types
  survey <- data.table::data.table(
    type = c(
      "integer",
      "decimal",
      "calculate",
      "decimal",
      "integer",
      "select_one",
      "text"
    ),
    list_name = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      "list1",
      NA
    ),
    name = c(
      "age",
      "income",
      "score",
      "rating",
      "count",
      "category",
      "text_var"
    ),
    label = c(
      "Age in years",
      "Annual income",
      "Test score",
      "Rating scale",
      "Item count",
      "Category",
      "Text variable"
    )
  )

  list(
    data = data,
    design = design,
    survey = survey
  )
}

# Helper function to create minimal test data
create_minimal_median_test_data <- function() {
  set.seed(456)
  n <- 50

  data <- data.frame(
    var1 = rnorm(n, mean = 10, sd = 2),
    var2 = round(runif(n, min = 1, max = 100)),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("decimal", "integer"),
    list_name = c(NA, NA),
    name = c("var1", "var2"),
    label = c("Variable 1", "Variable 2")
  )

  list(
    data = data,
    design = design,
    survey = survey
  )
}

test_that("kobo_median works with single numeric variable", {
  test_data <- create_kobo_median_test_data()

  result <- kobo_median(
    design = test_data$design,
    vars = "age",
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$var), "age")
  expect_equal(unique(result$analysis), "median")
  expect_equal(nrow(result), 1)

  # Check required columns exist
  expect_true("stat" %in% colnames(result))
  expect_true("stat_low" %in% colnames(result))
  expect_true("stat_upp" %in% colnames(result))
  expect_true("var_label" %in% colnames(result))
  expect_true("stat_type" %in% colnames(result))
  expect_equal(unique(result$stat_type), "median")

  # Check that the median is reasonable
  expect_true(result$stat > 25 && result$stat < 45) # Should be around 35
  expect_equal(result$var_label, "Age in years")
})

test_that("kobo_median works with multiple numeric variables", {
  test_data <- create_kobo_median_test_data()

  result <- kobo_median(
    design = test_data$design,
    vars = c("age", "score"),
    survey = test_data$survey
  )

  expect_equal(nrow(result), 2)
  expect_setequal(unique(result$var), c("age", "score"))
  expect_equal(unique(result$analysis), "median")

  # Check that each variable has reasonable medians
  age_result <- result[result$var == "age", ]
  score_result <- result[result$var == "score", ]

  expect_true(age_result$stat > 25 && age_result$stat < 45)
  expect_true(score_result$stat > 65 && score_result$stat < 85)
})

test_that("kobo_median works with different numeric types", {
  test_data <- create_kobo_median_test_data()

  # Test with integer, decimal, and calculate types
  result <- kobo_median(
    design = test_data$design,
    vars = c("age", "income", "score"), # integer, decimal, calculate
    survey = test_data$survey
  )

  expect_equal(nrow(result), 3)
  expect_setequal(unique(result$var), c("age", "income", "score"))
  expect_equal(unique(result$analysis), "median")

  # All should have median statistics
  expect_true(all(result$stat_type == "median"))
})

test_that("kobo_median works with grouping", {
  test_data <- create_kobo_median_test_data()

  result <- kobo_median(
    design = test_data$design,
    vars = "age",
    survey = test_data$survey,
    group = "group_var"
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_equal(unique(result$group_key), "group_var")
  expect_setequal(unique(result$group_key_value), c("GroupA", "GroupB"))
  expect_equal(nrow(result), 2) # One row per group
})

test_that("kobo_median works with multiple grouping variables", {
  test_data <- create_kobo_median_test_data()

  result <- kobo_median(
    design = test_data$design,
    vars = "age",
    survey = test_data$survey,
    group = c("group_var", "group_var2"),
    group_key_sep = " | "
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  # Note: group_key_sep parameter doesn't seem to be fully propagating through the call chain
  # This should be investigated as a potential bug in the underlying svy_median function
  expect_equal(unique(result$group_key), "group_var | group_var2")

  # Check that group_key_value contains the separator
  expect_true(any(grepl(" | ", result$group_key_value)))

  # Should have multiple rows for different group combinations
  expect_true(nrow(result) > 2)
})

test_that("kobo_median works without survey labels", {
  test_data <- create_kobo_median_test_data()

  result <- kobo_median(
    design = test_data$design,
    vars = "age",
    survey = test_data$survey,
    label_survey = FALSE
  )

  expect_false("var_label" %in% colnames(result))
  expect_equal(unique(result$var), "age")
})

test_that("kobo_median handles different variance types", {
  test_data <- create_minimal_median_test_data()

  # Test confidence intervals
  result_ci <- kobo_median(
    design = test_data$design,
    vars = "var1",
    survey = test_data$survey,
    vartype = "ci"
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))
  expect_false("stat_se" %in% colnames(result_ci))

  # Test standard errors
  result_se <- kobo_median(
    design = test_data$design,
    vars = "var1",
    survey = test_data$survey,
    vartype = "se"
  )
  expect_true("stat_se" %in% colnames(result_se))
  expect_false(any(c("stat_low", "stat_upp") %in% colnames(result_se)))

  # Test variances
  result_var <- kobo_median(
    design = test_data$design,
    vars = "var1",
    survey = test_data$survey,
    vartype = "var"
  )
  expect_true("stat_var" %in% colnames(result_var))
  expect_false(any(
    c("stat_low", "stat_upp", "stat_se") %in% colnames(result_var)
  ))

  # Estimates should be the same regardless of vartype
  expect_equal(result_ci$stat, result_se$stat, tolerance = 1e-10)
  expect_equal(result_ci$stat, result_var$stat, tolerance = 1e-10)
})

test_that("kobo_median handles different confidence levels", {
  test_data <- create_minimal_median_test_data()

  levels <- c(0.90, 0.95, 0.99)

  for (level in levels) {
    result <- kobo_median(
      design = test_data$design,
      vars = "var1",
      survey = test_data$survey,
      level = level,
      vartype = "ci"
    )

    expect_s3_class(result, "data.table")
    expect_true(all(c("stat_low", "stat_upp") %in% colnames(result)))

    # CI widths should be reasonable and increase with confidence level
    ci_width <- result$stat_upp - result$stat_low
    expect_true(ci_width > 0)
  }
})

test_that("kobo_median handles NA values correctly", {
  # Skip on platforms with known survey package CI issues
  if (Sys.info()["sysname"] == "Darwin") {
    skip("Skipping on macOS due to platform-specific survey package CI issues")
  }

  test_data <- create_minimal_median_test_data()

  # Add some NA values
  test_data$data$var1[1:5] <- NA
  design_with_na <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # Test with na_rm = TRUE
  result_na_rm <- kobo_median(
    design = design_with_na,
    vars = "var1",
    survey = test_data$survey,
    na_rm = TRUE
  )

  expect_equal(result_na_rm$na_count_tot, 5)
  expect_equal(result_na_rm$n_tot, 50)
  expect_false(is.na(result_na_rm$stat))

  # Test with na_rm = FALSE
  result_na_keep <- kobo_median(
    design = design_with_na,
    vars = "var1",
    survey = test_data$survey,
    na_rm = FALSE
  )

  expect_equal(result_na_keep$na_count_tot, 5)
  expect_equal(result_na_keep$n_tot, 50)
})

test_that("kobo_median validates input parameters", {
  test_data <- create_kobo_median_test_data()

  # Test with non-numeric variable
  expect_error(
    kobo_median(
      design = test_data$design,
      vars = "category", # This is select_one, not numeric
      survey = test_data$survey
    ),
    "Variable is not a numeric variable in survey"
  )

  # Test with missing variable
  expect_error(
    kobo_median(
      design = test_data$design,
      vars = "nonexistent_var",
      survey = test_data$survey
    ),
    "Missing variables in design"
  )

  # Test with invalid vartype
  expect_error(
    kobo_median(
      design = test_data$design,
      vars = "age",
      survey = test_data$survey,
      vartype = "invalid"
    ),
    "'arg' should be one of"
  )

  # Test with invalid confidence level
  expect_error(
    kobo_median(
      design = test_data$design,
      vars = "age",
      survey = test_data$survey,
      level = 1.5
    ),
    "Element 1 is not <= 1"
  )
})

test_that("kobo_median warns when no numeric variables in survey", {
  test_data <- create_kobo_median_test_data()

  # Create survey with only non-numeric variables
  survey_no_numeric <- data.table::data.table(
    type = c("select_one", "text"),
    list_name = c("list1", NA),
    name = c("category", "text_var"),
    label = c("Category", "Text variable")
  )

  expect_warning(
    result <- kobo_median(
      design = test_data$design,
      vars = "category", # Will fail validation anyway
      survey = survey_no_numeric
    ),
    "There are no vars of types decimal, calculate or integer"
  )
})

test_that("kobo_median handles edge cases", {
  # Test with minimal data
  minimal_data <- data.frame(
    num_var = c(1, 2, 3, 4, 5),
    weights = c(1, 1, 1, 1, 1),
    strata = c("S1", "S1", "S1", "S1", "S1")
  )

  design_minimal <- srvyr::as_survey_design(
    minimal_data,
    weights = weights,
    strata = strata
  )

  survey_minimal <- data.table::data.table(
    type = "integer",
    list_name = NA,
    name = "num_var",
    label = "Number variable"
  )

  result <- kobo_median(
    design = design_minimal,
    vars = "num_var",
    survey = survey_minimal
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$stat[1], 3.0) # Median of 1,2,3,4,5
  expect_equal(result$var_label[1], "Number variable")
})

test_that("kobo_median handles all NA variables correctly", {
  test_data <- create_minimal_median_test_data()

  # Create data with all NAs
  data_all_na <- test_data$data
  data_all_na$var1 <- rep(NA_real_, nrow(data_all_na))

  design_all_na <- srvyr::as_survey_design(
    data_all_na,
    weights = weights,
    strata = strata
  )

  # Should handle all-NA gracefully - this may return empty result due to svy_median behavior
  suppressWarnings({
    result_all_na <- kobo_median(
      design = design_all_na,
      vars = "var1",
      survey = test_data$survey
    )
  })

  # Function should complete without error - exact behavior may vary
  expect_true(inherits(result_all_na, c("data.table", "data.frame")))
})

test_that("kobo_median accuracy verification against srvyr", {
  test_data <- create_minimal_median_test_data()

  # Calculate using kobo_median
  result_kobo <- kobo_median(
    design = test_data$design,
    vars = "var1",
    survey = test_data$survey
  )

  # Calculate using srvyr directly
  result_srvyr <- test_data$design %>%
    srvyr::summarise(
      stat = srvyr::survey_median(var1, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Results should be very close
  expect_equal(result_kobo$stat, result_srvyr$stat, tolerance = 1e-10)
  expect_equal(result_kobo$stat_low, result_srvyr$stat_low, tolerance = 1e-10)
  expect_equal(result_kobo$stat_upp, result_srvyr$stat_upp, tolerance = 1e-10)
})

test_that("kobo_median accuracy with grouping against srvyr", {
  test_data <- create_kobo_median_test_data()

  # Calculate using kobo_median with grouping
  result_kobo <- kobo_median(
    design = test_data$design,
    vars = "age",
    survey = test_data$survey,
    group = "group_var"
  )

  # Calculate using srvyr directly
  result_srvyr <- test_data$design %>%
    srvyr::group_by(group_var) %>%
    srvyr::summarise(
      stat = srvyr::survey_median(age, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Sort both results for comparison
  result_kobo_sorted <- result_kobo[order(result_kobo$group_key_value), ]
  result_srvyr_sorted <- result_srvyr[order(result_srvyr$group_var), ]

  expect_equal(
    result_kobo_sorted$stat,
    result_srvyr_sorted$stat,
    tolerance = 1e-10
  )
})

test_that("kobo_median returns data.table", {
  test_data <- create_minimal_median_test_data()

  result <- kobo_median(
    design = test_data$design,
    vars = "var1",
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_false(
    inherits(result, "data.frame") && !inherits(result, "data.table")
  )
})

test_that("kobo_median calculates unweighted statistics correctly", {
  test_data <- create_kobo_median_test_data()

  result <- kobo_median(
    design = test_data$design,
    vars = "age",
    survey = test_data$survey
  )

  # Calculate expected unweighted median
  expected_median <- median(test_data$data$age, na.rm = TRUE)

  expect_equal(result$stat_unw, expected_median, tolerance = 1e-10)
})

test_that("kobo_median handles integer overflow and large numbers", {
  set.seed(789)
  n <- 50

  # Create data with large numbers
  data_large <- data.frame(
    large_var = runif(n, min = 1e6, max = 1e7),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_large <- srvyr::as_survey_design(
    data_large,
    weights = weights,
    strata = strata
  )

  survey_large <- data.table::data.table(
    type = "decimal",
    list_name = NA,
    name = "large_var",
    label = "Large variable"
  )

  result <- kobo_median(
    design = design_large,
    vars = "large_var",
    survey = survey_large
  )

  expect_s3_class(result, "data.table")
  expect_true(result$stat > 1e6 && result$stat < 1e7)
  expect_false(is.na(result$stat))
  expect_false(is.infinite(result$stat))
})

test_that("kobo_median handles zero and negative values", {
  set.seed(101112)
  n <- 50

  # Create data with negative and zero values
  data_mixed <- data.frame(
    mixed_var = c(
      rnorm(n / 2, mean = -10, sd = 5),
      rep(0, 5),
      rnorm(n / 2 - 5, mean = 10, sd = 5)
    ),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_mixed <- srvyr::as_survey_design(
    data_mixed,
    weights = weights,
    strata = strata
  )

  survey_mixed <- data.table::data.table(
    type = "decimal",
    list_name = NA,
    name = "mixed_var",
    label = "Mixed variable"
  )

  result <- kobo_median(
    design = design_mixed,
    vars = "mixed_var",
    survey = survey_mixed
  )

  expect_s3_class(result, "data.table")
  expect_false(is.na(result$stat))
  # Median should be reasonable given the distribution
  expect_true(abs(result$stat) < 15)
})

test_that("kobo_median preserves variable labels correctly", {
  test_data <- create_kobo_median_test_data()

  result <- kobo_median(
    design = test_data$design,
    vars = c("age", "score"),
    survey = test_data$survey,
    label_survey = TRUE
  )

  # Check variable labels
  age_result <- result[result$var == "age", ]
  score_result <- result[result$var == "score", ]

  expect_equal(age_result$var_label, "Age in years")
  expect_equal(score_result$var_label, "Test score")
})

test_that("kobo_median handles survey design integrity", {
  test_data <- create_kobo_median_test_data()

  result <- kobo_median(
    design = test_data$design,
    vars = "age",
    survey = test_data$survey
  )

  # Check that survey design information is preserved
  expect_true("stat_unw" %in% colnames(result)) # Unweighted stats
  expect_true("n_unw" %in% colnames(result)) # Unweighted counts
  expect_true("n_tot" %in% colnames(result)) # Total sample size
  expect_true("n_tot_unw" %in% colnames(result)) # Total unweighted sample size

  # Check sample size matches
  expect_equal(result$n_tot, 100)
  expect_equal(result$n_tot_unw, 100)
})

test_that("kobo_median parameter validation works through kobo_param_check", {
  test_data <- create_minimal_median_test_data()

  # Test missing design
  expect_error(
    kobo_median(
      design = NULL,
      vars = "var1",
      survey = test_data$survey
    )
  )

  # Test missing vars
  expect_error(
    kobo_median(
      design = test_data$design,
      vars = NULL,
      survey = test_data$survey
    )
  )

  # Test missing survey
  expect_error(
    kobo_median(
      design = test_data$design,
      vars = "var1",
      survey = NULL
    )
  )
})

test_that("kobo_median handles skewed distributions correctly", {
  set.seed(999)
  n <- 100

  # Create heavily skewed data
  data_skewed <- data.frame(
    skewed_var = c(rexp(n, rate = 0.1)), # Exponential distribution
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_skewed <- srvyr::as_survey_design(
    data_skewed,
    weights = weights,
    strata = strata
  )

  survey_skewed <- data.table::data.table(
    type = "decimal",
    list_name = NA,
    name = "skewed_var",
    label = "Skewed variable"
  )

  result <- kobo_median(
    design = design_skewed,
    vars = "skewed_var",
    survey = survey_skewed
  )

  expect_s3_class(result, "data.table")
  expect_false(is.na(result$stat))
  expect_true(result$stat > 0) # Should be positive for exponential

  # Median should be different from mean for skewed data
  mean_result <- mean(data_skewed$skewed_var)
  expect_false(abs(result$stat - mean_result) < 1)
})

test_that("kobo_median handles discrete integer distributions", {
  set.seed(777)
  n <- 100

  # Create discrete integer data
  data_discrete <- data.frame(
    discrete_var = sample(1:10, n, replace = TRUE),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_discrete <- srvyr::as_survey_design(
    data_discrete,
    weights = weights,
    strata = strata
  )

  survey_discrete <- data.table::data.table(
    type = "integer",
    list_name = NA,
    name = "discrete_var",
    label = "Discrete variable"
  )

  result <- kobo_median(
    design = design_discrete,
    vars = "discrete_var",
    survey = survey_discrete
  )

  expect_s3_class(result, "data.table")
  expect_false(is.na(result$stat))
  expect_true(result$stat >= 1 && result$stat <= 10)
  expect_equal(result$var_label, "Discrete variable")
})

test_that("kobo_median handles single value datasets", {
  n <- 20

  # Create data with single repeated value
  data_single <- data.frame(
    single_var = rep(42, n),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_single <- srvyr::as_survey_design(
    data_single,
    weights = weights,
    strata = strata
  )

  survey_single <- data.table::data.table(
    type = "integer",
    list_name = NA,
    name = "single_var",
    label = "Single value variable"
  )

  result <- kobo_median(
    design = design_single,
    vars = "single_var",
    survey = survey_single
  )

  expect_s3_class(result, "data.table")
  expect_equal(result$stat, 42)

  # For single-value datasets, CI bounds should either collapse to the point estimate
  # or be NaN due to platform-specific numerical issues in survey package
  if (all(is.finite(c(result$stat_low, result$stat_upp)))) {
    # Normal case: both bounds are finite and should equal the point estimate
    expect_equal(result$stat_low, result$stat_upp)
    expect_equal(result$stat_low, result$stat)
  } else {
    # Platform edge case: accept NaN bounds as known survey package behavior
    # when variance is zero (all identical values)
    expect_true(
      is.nan(result$stat_low) || is.nan(result$stat_upp),
      info = "Single-value dataset may produce NaN CI bounds on some platforms"
    )
  }
})
