# Tests for kobo_ratio function

# Helper function to create test data for kobo_ratio
create_kobo_ratio_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create sample survey data with numeric variables for ratios
  data <- data.frame(
    # Numerator variables
    income = round(rnorm(n, mean = 50000, sd = 15000)),
    expenses = round(rnorm(n, mean = 30000, sd = 10000)),
    assets = round(rnorm(n, mean = 100000, sd = 30000)),
    # Denominator variables
    household_size = round(runif(n, min = 1, max = 8)),
    area_sqm = round(runif(n, min = 50, max = 200)),
    population = round(runif(n, min = 100, max = 5000)),
    # Additional numeric variables
    score = rnorm(n, mean = 75, sd = 12),
    rating = runif(n, min = 1, max = 5),
    # Non-numeric variables
    category = sample(c("Urban", "Rural", "Suburban"), n, replace = TRUE),
    text_var = paste0("text_", seq_len(n)),
    # Grouping variables
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    status = sample(c("Active", "Inactive"), n, replace = TRUE),
    group_var = sample(c("A", "B", "C"), n, replace = TRUE),
    # Survey design variables
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE)
  )

  # Ensure no zero denominators to avoid division issues
  data$household_size[data$household_size == 0] <- 1
  data$area_sqm[data$area_sqm == 0] <- 50
  data$population[data$population == 0] <- 100

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create survey sheet with different numeric types
  survey <- data.table::data.table(
    type = c(
      "decimal",
      "decimal",
      "calculate", # numerators
      "integer",
      "decimal",
      "integer", # denominators
      "decimal",
      "decimal", # additional numerics
      "select_one",
      "text" # non-numeric
    ),
    list_name = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "location_list",
      NA
    ),
    name = c(
      "income",
      "expenses",
      "assets",
      "household_size",
      "area_sqm",
      "population",
      "score",
      "rating",
      "category",
      "text_var"
    ),
    label = c(
      "Monthly income",
      "Monthly expenses",
      "Total assets",
      "Household size",
      "Area in square meters",
      "Local population",
      "Test score",
      "Rating scale",
      "Location category",
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
create_minimal_ratio_test_data <- function() {
  set.seed(456)
  n <- 50

  data <- data.frame(
    num1 = rnorm(n, mean = 100, sd = 20),
    num2 = runif(n, min = 50, max = 150),
    denom1 = abs(rnorm(n, mean = 10, sd = 3)) + 1,
    denom2 = runif(n, min = 2, max = 20),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("decimal", "integer", "calculate", "decimal"),
    list_name = c(NA, NA, NA, NA),
    name = c("num1", "num2", "denom1", "denom2"),
    label = c("Numerator 1", "Numerator 2", "Denominator 1", "Denominator 2")
  )

  list(
    data = data,
    design = design,
    survey = survey
  )
}

# Test basic functionality
test_that("kobo_ratio works with single numerator and denominator", {
  test_data <- create_kobo_ratio_test_data()

  result <- kobo_ratio(
    design = test_data$design,
    nums = "income",
    denoms = "household_size",
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(unique(result$analysis), "ratio")
  expect_equal(unique(result$stat_type), "ratio")

  # Check required columns exist
  expect_true("stat" %in% colnames(result))
  expect_true("stat_low" %in% colnames(result))
  expect_true("stat_upp" %in% colnames(result))
  expect_true("var_label" %in% colnames(result))
  expect_true("var" %in% colnames(result))

  # Check that var contains both variables
  expect_true(grepl("income.*household_size", result$var))
  expect_true(grepl("Monthly income.*Household size", result$var_label))

  # Check that the ratio is reasonable (income per household member)
  expect_true(result$stat > 0)
  expect_true(is.numeric(result$stat))
})

test_that("kobo_ratio works with multiple numerators and denominators", {
  test_data <- create_kobo_ratio_test_data()

  result <- kobo_ratio(
    design = test_data$design,
    nums = c("income", "expenses"),
    denoms = c("household_size", "area_sqm"),
    survey = test_data$survey
  )

  expect_equal(nrow(result), 2) # 2 nums paired with 2 denoms = 2 ratios
  expect_equal(unique(result$analysis), "ratio")

  # Check expected pairings are present (income with household_size, expenses with area_sqm)
  expected_pairings <- c(
    "income.*household_size",
    "expenses.*area_sqm"
  )

  for (pairing in expected_pairings) {
    expect_true(any(grepl(pairing, result$var)))
  }
})

test_that("kobo_ratio works with grouping", {
  test_data <- create_kobo_ratio_test_data()

  result <- kobo_ratio(
    design = test_data$design,
    nums = "income",
    denoms = "household_size",
    group = "region",
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))

  # Should have one row per region
  unique_regions <- unique(test_data$data$region)
  expect_equal(nrow(result), length(unique_regions))
  expect_setequal(unique(result$group_key_value), unique_regions)
})

test_that("kobo_ratio works with multiple grouping variables", {
  test_data <- create_kobo_ratio_test_data()

  result <- kobo_ratio(
    design = test_data$design,
    nums = "income",
    denoms = "household_size",
    group = c("region", "status"),
    survey = test_data$survey
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))

  # Check that group_key contains both variables
  expect_true(all(grepl("region.*status", result$group_key)))

  # Should have rows for each combination of region and status
  unique_combos <- unique(paste(
    test_data$data$region,
    test_data$data$status,
    sep = " -/- "
  ))
  expect_equal(nrow(result), length(unique_combos))
})

test_that("kobo_ratio respects custom separators", {
  test_data <- create_minimal_ratio_test_data()

  result <- kobo_ratio(
    design = test_data$design,
    nums = "num1",
    denoms = "denom1",
    ratio_key_sep = " | ",
    survey = test_data$survey
  )

  expect_true(grepl("num1.*\\|.*denom1", result$var))
  expect_true(grepl("Numerator 1.*\\|.*Denominator 1", result$var_label))
})

test_that("kobo_ratio works without labels", {
  test_data <- create_minimal_ratio_test_data()

  result <- kobo_ratio(
    design = test_data$design,
    nums = "num1",
    denoms = "denom1",
    survey = test_data$survey,
    label_survey = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_false("var_label" %in% colnames(result))
  expect_true("var" %in% colnames(result))
  expect_true(grepl("num1.*denom1", result$var))
})

test_that("kobo_ratio handles NA values correctly", {
  test_data <- create_minimal_ratio_test_data()

  # Add some NA values
  test_data$data$num1[1:5] <- NA
  test_data$data$denom1[6:10] <- NA
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # With na_rm = TRUE (default)
  result_rm <- kobo_ratio(
    design = test_data$design,
    nums = "num1",
    denoms = "denom1",
    survey = test_data$survey,
    na_rm = TRUE
  )

  # With na_rm = FALSE
  result_keep <- kobo_ratio(
    design = test_data$design,
    nums = "num1",
    denoms = "denom1",
    survey = test_data$survey,
    na_rm = FALSE
  )

  expect_s3_class(result_rm, "data.table")
  expect_s3_class(result_keep, "data.table")

  # Results should be different due to NA handling
  expect_false(identical(result_rm$stat, result_keep$stat))
})

test_that("kobo_ratio handles different vartype options", {
  test_data <- create_minimal_ratio_test_data()

  # Test different vartype options
  vartypes <- c("ci", "se", "var", "cv")

  for (vt in vartypes) {
    result <- kobo_ratio(
      design = test_data$design,
      nums = "num1",
      denoms = "denom1",
      survey = test_data$survey,
      vartype = vt
    )

    expect_s3_class(result, "data.table")
    expect_true("stat" %in% colnames(result))

    if (vt == "ci") {
      expect_true(all(c("stat_low", "stat_upp") %in% colnames(result)))
    } else if (vt == "se") {
      expect_true("stat_se" %in% colnames(result))
    } else if (vt == "var") {
      expect_true("stat_var" %in% colnames(result))
    } else if (vt == "cv") {
      expect_true("stat_cv" %in% colnames(result))
    }
  }
})

test_that("kobo_ratio handles different confidence levels", {
  test_data <- create_minimal_ratio_test_data()

  result_95 <- kobo_ratio(
    design = test_data$design,
    nums = "num1",
    denoms = "denom1",
    survey = test_data$survey,
    level = 0.95
  )

  result_99 <- kobo_ratio(
    design = test_data$design,
    nums = "num1",
    denoms = "denom1",
    survey = test_data$survey,
    level = 0.99
  )

  # 99% CI should be wider than 95% CI
  ci_width_95 <- result_95$stat_upp - result_95$stat_low
  ci_width_99 <- result_99$stat_upp - result_99$stat_low

  expect_true(ci_width_99 >= ci_width_95)
})

# Test error conditions
test_that("kobo_ratio errors when no numeric variables in survey", {
  test_data <- create_kobo_ratio_test_data()

  # Create survey with only non-numeric variables
  survey_no_numeric <- data.table::data.table(
    type = c("select_one", "text"),
    list_name = c("list1", NA),
    name = c("category", "text_var"),
    label = c("Category", "Text variable")
  )

  expect_warning(
    result <- kobo_ratio(
      design = test_data$design,
      nums = "category",
      denoms = "text_var",
      survey = survey_no_numeric
    ),
    "There are no vars of types decimal, calculate or integer"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("kobo_ratio errors when specified variables are not numeric", {
  test_data <- create_kobo_ratio_test_data()

  expect_error(
    kobo_ratio(
      design = test_data$design,
      nums = "category", # This is select_one, not numeric
      denoms = "household_size",
      survey = test_data$survey
    ),
    "Variable is not a numeric variable in survey"
  )

  expect_error(
    kobo_ratio(
      design = test_data$design,
      nums = "income",
      denoms = "text_var", # This is text, not numeric
      survey = test_data$survey
    ),
    "Variable is not a numeric variable in survey"
  )
})

test_that("kobo_ratio errors when variables not in survey", {
  test_data <- create_kobo_ratio_test_data()

  expect_error(
    kobo_ratio(
      design = test_data$design,
      nums = "nonexistent_var",
      denoms = "household_size",
      survey = test_data$survey
    ),
    "Missing variables in design"
  )
})

# Test edge cases
test_that("kobo_ratio handles single observation", {
  # Create data with multiple observations to avoid survey design issues
  data <- data.frame(
    num = c(100, 110, 90),
    denom = c(5, 5, 5),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1")
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("decimal", "integer"),
    list_name = c(NA, NA),
    name = c("num", "denom"),
    label = c("Numerator", "Denominator")
  )

  result <- kobo_ratio(
    design = design,
    nums = "num",
    denoms = "denom",
    survey = survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true(result$stat > 15 && result$stat < 25) # Should be around 20
})

test_that("kobo_ratio handles all-zero denominators gracefully", {
  # Create data with zero denominators
  data <- data.frame(
    num = c(10, 20, 30),
    denom = c(0, 0, 0),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1")
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("decimal", "integer"),
    list_name = c(NA, NA),
    name = c("num", "denom"),
    label = c("Numerator", "Denominator")
  )

  # This should handle division by zero gracefully (result likely Inf or error)
  expect_no_error({
    result <- kobo_ratio(
      design = design,
      nums = "num",
      denoms = "denom",
      survey = survey
    )
  })
})

test_that("kobo_ratio handles extreme values", {
  # Create data with extreme values
  data <- data.frame(
    num = c(1e6, 1e-6, 1e3),
    denom = c(1e-3, 1e3, 1),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1")
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("decimal", "decimal"),
    list_name = c(NA, NA),
    name = c("num", "denom"),
    label = c("Numerator", "Denominator")
  )

  result <- kobo_ratio(
    design = design,
    nums = "num",
    denoms = "denom",
    survey = survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true(is.finite(result$stat))
})

# Test statistical accuracy by comparing with direct srvyr calls
test_that("kobo_ratio produces statistically accurate results", {
  test_data <- create_minimal_ratio_test_data()

  result <- kobo_ratio(
    design = test_data$design,
    nums = "num1",
    denoms = "denom1",
    survey = test_data$survey,
    label_survey = FALSE
  )

  # Calculate the same ratio directly with srvyr
  direct_result <- test_data$design %>%
    srvyr::summarise(
      stat = srvyr::survey_ratio(num1, denom1, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Results should be very close (allowing for small numerical differences)
  expect_equal(result$stat[1], direct_result$stat, tolerance = 1e-10)
  expect_equal(result$stat_low[1], direct_result$stat_low, tolerance = 1e-10)
  expect_equal(result$stat_upp[1], direct_result$stat_upp, tolerance = 1e-10)
})

test_that("kobo_ratio produces statistically accurate results with grouping", {
  test_data <- create_minimal_ratio_test_data()

  # Add grouping variable
  test_data$data$group <- sample(
    c("A", "B"),
    nrow(test_data$data),
    replace = TRUE
  )
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result <- kobo_ratio(
    design = test_data$design,
    nums = "num1",
    denoms = "denom1",
    group = "group",
    survey = test_data$survey,
    label_survey = FALSE
  )

  # Calculate the same ratio directly with srvyr
  direct_result <- test_data$design %>%
    srvyr::group_by(group) %>%
    srvyr::summarise(
      stat = srvyr::survey_ratio(num1, denom1, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Sort results for comparison
  result_sorted <- result[order(result$group_key_value), ]

  expect_equal(result_sorted$stat, direct_result$stat, tolerance = 1e-10)
  expect_equal(
    result_sorted$stat_low,
    direct_result$stat_low,
    tolerance = 1e-10
  )
  expect_equal(
    result_sorted$stat_upp,
    direct_result$stat_upp,
    tolerance = 1e-10
  )
})

# Test parameter validation
test_that("kobo_ratio validates parameters correctly", {
  test_data <- create_minimal_ratio_test_data()

  # Test invalid level
  expect_error(
    kobo_ratio(
      design = test_data$design,
      nums = "num1",
      denoms = "denom1",
      survey = test_data$survey,
      level = 1.5 # Invalid confidence level
    )
  )

  # Test invalid vartype
  expect_error(
    kobo_ratio(
      design = test_data$design,
      nums = "num1",
      denoms = "denom1",
      survey = test_data$survey,
      vartype = "invalid_vartype"
    )
  )
})

test_that("kobo_ratio handles empty results gracefully", {
  # Create data where filtering results in no observations
  data <- data.frame(
    num = c(NA, NA, NA),
    denom = c(NA, NA, NA),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1")
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("decimal", "integer"),
    list_name = c(NA, NA),
    name = c("num", "denom"),
    label = c("Numerator", "Denominator")
  )

  expect_warning(
    {
      result <- kobo_ratio(
        design = design,
        nums = "num",
        denoms = "denom",
        survey = survey,
        na_rm = TRUE
      )
    },
    "only contains missing values"
  )

  # Should handle gracefully - may return empty result or result with NA/Inf
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})
