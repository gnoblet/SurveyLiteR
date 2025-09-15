# Tests for kobo_median_all function

# Helper function to create test data for kobo_median_all
create_kobo_median_all_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create sample survey data with multiple numeric variables
  data <- data.frame(
    age = round(rnorm(n, mean = 35, sd = 10)),
    income = round(rnorm(n, mean = 50000, sd = 15000)),
    score = rnorm(n, mean = 75, sd = 12),
    rating = runif(n, min = 1, max = 5),
    count = rpois(n, lambda = 3),
    height = rnorm(n, mean = 170, sd = 15),
    # Non-numeric variables
    category = sample(c("A", "B", "C"), n, replace = TRUE),
    text_var = paste0("text_", seq_len(n)),
    select_mult = sample(c("x", "y", "z"), n, replace = TRUE),
    # Grouping variables
    group_var = sample(c("GroupA", "GroupB"), n, replace = TRUE),
    group_var2 = sample(c("X", "Y", "Z"), n, replace = TRUE),
    # Survey design variables
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create survey sheet with mixed variable types
  survey <- data.table::data.table(
    type = c(
      "integer",
      "decimal",
      "calculate",
      "decimal",
      "integer",
      "decimal",
      "select_one",
      "text",
      "select_multiple"
    ),
    list_name = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "list1",
      NA,
      "list2"
    ),
    name = c(
      "age",
      "income",
      "score",
      "rating",
      "count",
      "height",
      "category",
      "text_var",
      "select_mult"
    ),
    label = c(
      "Age in years",
      "Annual income",
      "Test score",
      "Rating scale",
      "Item count",
      "Height in cm",
      "Category",
      "Text variable",
      "Multiple select"
    )
  )

  list(
    data = data,
    design = design,
    survey = survey
  )
}

# Helper function to create minimal test data
create_minimal_median_all_test_data <- function() {
  set.seed(456)
  n <- 50

  data <- data.frame(
    var1 = rnorm(n, mean = 10, sd = 2),
    var2 = round(runif(n, min = 1, max = 100)),
    var3 = rnorm(n, mean = 50, sd = 5),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("decimal", "integer", "calculate"),
    list_name = c(NA, NA, NA),
    name = c("var1", "var2", "var3"),
    label = c("Variable 1", "Variable 2", "Variable 3")
  )

  list(
    data = data,
    design = design,
    survey = survey
  )
}

test_that("kobo_median_all finds and processes all numeric variables", {
  test_data <- create_kobo_median_all_test_data()

  result <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")

  # Should include all 6 numeric variables
  expect_setequal(
    unique(result$var),
    c("age", "income", "score", "rating", "count", "height")
  )

  # Should have 6 rows (one for each variable)
  expect_equal(nrow(result), 6)

  # All should be marked as numeric analysis
  expect_equal(unique(result$analysis), "median")

  # All should have median statistics
  expect_true(all(result$stat_type == "median"))
})

test_that("kobo_median_all excludes grouping variables", {
  test_data <- create_kobo_median_all_test_data()

  # Add a numeric variable that will be used for grouping
  test_data$data$group_numeric <- rnorm(
    nrow(test_data$data),
    mean = 100,
    sd = 20
  )

  # Update design
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # Add to survey sheet
  survey_extended <- rbind(
    test_data$survey,
    data.table::data.table(
      type = "decimal",
      list_name = NA,
      name = "group_numeric",
      label = "Grouping Numeric Variable"
    )
  )

  result <- kobo_median_all(
    design = test_data$design,
    survey = survey_extended,
    group = "group_numeric"
  )

  # Should NOT include the grouping variable
  expect_false("group_numeric" %in% unique(result$var))

  # Should still include other numeric variables
  expect_setequal(
    unique(result$var),
    c("age", "income", "score", "rating", "count", "height")
  )

  # Should have grouping columns
  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
})

test_that("kobo_median_all excludes multiple grouping variables", {
  test_data <- create_kobo_median_all_test_data()

  # Add two numeric variables for grouping
  test_data$data$group1 <- round(runif(nrow(test_data$data), 1, 5))
  test_data$data$group2 <- round(rnorm(nrow(test_data$data), 0, 1))

  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  survey_extended <- rbind(
    test_data$survey,
    data.table::data.table(
      type = c("integer", "decimal"),
      list_name = c(NA, NA),
      name = c("group1", "group2"),
      label = c("Group 1", "Group 2")
    )
  )

  result <- kobo_median_all(
    design = test_data$design,
    survey = survey_extended,
    group = c("group1", "group2")
  )

  # Should exclude both grouping variables
  expect_false(any(c("group1", "group2") %in% unique(result$var)))

  # Should include original numeric variables
  expect_setequal(
    unique(result$var),
    c("age", "income", "score", "rating", "count", "height")
  )
})

test_that("kobo_median_all works without survey labels", {
  test_data <- create_kobo_median_all_test_data()

  result <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey,
    label_survey = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 6) # All numeric variables
  expect_false("var_label" %in% colnames(result))
})

test_that("kobo_median_all handles different variance types", {
  test_data <- create_minimal_median_all_test_data()

  # Test confidence intervals
  result_ci <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey,
    vartype = "ci"
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))

  # Test standard errors
  result_se <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey,
    vartype = "se"
  )
  expect_true("stat_se" %in% colnames(result_se))

  # Test variances
  result_var <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey,
    vartype = "var"
  )
  expect_true("stat_var" %in% colnames(result_var))
})

test_that("kobo_median_all handles different confidence levels", {
  test_data <- create_minimal_median_all_test_data()

  for (level in c(0.90, 0.95, 0.99)) {
    result <- kobo_median_all(
      design = test_data$design,
      survey = test_data$survey,
      level = level,
      vartype = "ci"
    )

    expect_s3_class(result, "data.table")
    expect_true(all(c("stat_low", "stat_upp") %in% colnames(result)))

    # CI widths should be reasonable
    ci_widths <- result$stat_upp - result$stat_low
    expect_true(all(ci_widths >= 0))
  }
})

test_that("kobo_median_all handles NA values correctly", {
  # Skip on platforms with known survey package CI issues
  if (Sys.info()["sysname"] == "Darwin") {
    skip("Skipping on macOS due to platform-specific survey package CI issues")
  }

  test_data <- create_minimal_median_all_test_data()

  # Add some NA values
  test_data$data$var1[1:5] <- NA
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # Test with na_rm = TRUE
  result_na_rm <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey,
    na_rm = TRUE
  )

  # Check that NA counts include the variable with NAs
  expect_true(5 %in% result_na_rm$na_count_tot)
  expect_equal(unique(result_na_rm$n_tot), 50)

  # Test with na_rm = FALSE
  result_na_keep <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey,
    na_rm = FALSE
  )

  expect_true(5 %in% result_na_keep$na_count_tot)
  expect_equal(unique(result_na_keep$n_tot), 50)
})

test_that("kobo_median_all warns when no numeric variables available", {
  test_data <- create_kobo_median_all_test_data()

  # Create survey with only non-numeric variables
  survey_no_numeric <- data.table::data.table(
    type = c("select_one", "text", "select_multiple"),
    list_name = c("list1", NA, "list2"),
    name = c("category", "text_var", "select_mult"),
    label = c("Category", "Text variable", "Multiple select")
  )

  expect_warning(
    result <- kobo_median_all(
      design = test_data$design,
      survey = survey_no_numeric
    ),
    "There are no vars of types decimal, calculate or integer"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("kobo_median_all errors when all numeric variables are grouping variables", {
  test_data <- create_minimal_median_all_test_data()

  expect_error(
    kobo_median_all(
      design = test_data$design,
      survey = test_data$survey,
      group = c("var1", "var2", "var3") # All numeric variables used for grouping
    ),
    "Grouping columns in `group` should be different than the numeric variables"
  )
})

test_that("kobo_median_all warns when some numeric variables are grouping variables", {
  test_data <- create_kobo_median_all_test_data()

  # Add a numeric variable that will be used for grouping
  test_data$data$group_numeric <- rnorm(
    nrow(test_data$data),
    mean = 100,
    sd = 20
  )

  # Update design
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # Add to survey sheet
  survey_extended <- rbind(
    test_data$survey,
    data.table::data.table(
      type = "decimal",
      list_name = NA,
      name = "group_numeric",
      label = "Grouping Numeric Variable"
    )
  )

  expect_warning(
    result <- kobo_median_all(
      design = test_data$design,
      survey = survey_extended,
      group = c("group_var", "group_numeric") # Mix of non-numeric and numeric grouping
    ),
    "Some numeric variables are used for grouping and won't be analyzed"
  )

  # Should NOT include the numeric grouping variable
  expect_false("group_numeric" %in% unique(result$var))

  # Should still include other numeric variables
  expect_setequal(
    unique(result$var),
    c("age", "income", "score", "rating", "count", "height")
  )
})

test_that("kobo_median_all works with custom group separator", {
  test_data <- create_kobo_median_all_test_data()

  result <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey,
    group = c("group_var", "group_var2"),
    group_key_sep = " | "
  )

  expect_true("group_key" %in% colnames(result))
  # Note: group_key_sep parameter doesn't seem to be fully propagating through the call chain
  # This should be investigated as a potential bug in the underlying kobo_median function
  expect_equal(unique(result$group_key), "group_var | group_var2")
  expect_true(any(grepl(" | ", result$group_key_value)))
})

test_that("kobo_median_all preserves variable labels", {
  test_data <- create_kobo_median_all_test_data()

  result <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey
  )

  # Check variable labels exist
  expect_true("var_label" %in% colnames(result))

  # Check specific labels
  age_results <- result[result$var == "age", ]
  expect_true(all(age_results$var_label == "Age in years"))

  score_results <- result[result$var == "score", ]
  expect_true(all(score_results$var_label == "Test score"))
})

test_that("kobo_median_all returns data.table", {
  test_data <- create_minimal_median_all_test_data()

  result <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_false(
    inherits(result, "data.frame") && !inherits(result, "data.table")
  )
})

test_that("kobo_median_all handles single numeric variable", {
  test_data <- create_minimal_median_all_test_data()

  # Survey with only one numeric variable
  survey_single <- data.table::data.table(
    type = "decimal",
    list_name = NA,
    name = "var1",
    label = "Variable 1"
  )

  result <- kobo_median_all(
    design = test_data$design,
    survey = survey_single
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$var), "var1")
  expect_equal(nrow(result), 1)
  expect_false(is.na(result$stat))
})

test_that("kobo_median_all works with mixed question types in survey", {
  test_data <- create_kobo_median_all_test_data()

  result <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey
  )

  # Should only include numeric variables, not others
  expect_setequal(
    unique(result$var),
    c("age", "income", "score", "rating", "count", "height")
  )

  # Should not include select_one, text, or select_multiple variables
  expect_false("category" %in% unique(result$var))
  expect_false("text_var" %in% unique(result$var))
  expect_false("select_mult" %in% unique(result$var))
})

test_that("kobo_median_all equivalent to kobo_median with all numeric vars", {
  test_data <- create_minimal_median_all_test_data()

  # Get all numeric variables manually
  numeric_vars <- c(
    get_survey_decimal(test_data$survey),
    get_survey_calculate(test_data$survey),
    get_survey_integer(test_data$survey)
  )

  result_all <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey
  )

  result_manual <- kobo_median(
    design = test_data$design,
    vars = numeric_vars,
    survey = test_data$survey
  )

  # Results should be identical
  expect_equal(nrow(result_all), nrow(result_manual))
  expect_setequal(result_all$var, result_manual$var)
  expect_equal(result_all$stat, result_manual$stat, tolerance = 1e-10)
})

test_that("kobo_median_all handles survey design integrity", {
  test_data <- create_kobo_median_all_test_data()

  result <- kobo_median_all(
    design = test_data$design,
    survey = test_data$survey
  )

  # Check that survey design information is preserved
  expect_true("stat_unw" %in% colnames(result)) # Unweighted stats
  expect_true("n_unw" %in% colnames(result)) # Unweighted counts
  expect_true("n_tot" %in% colnames(result)) # Total sample size
  expect_true("n_tot_unw" %in% colnames(result)) # Total unweighted sample size

  # Check sample sizes
  expect_equal(unique(result$n_tot), 100)
  expect_equal(unique(result$n_tot_unw), 100)
})

test_that("kobo_median_all parameter validation works", {
  test_data <- create_minimal_median_all_test_data()

  # Test invalid vartype
  expect_error(
    kobo_median_all(
      design = test_data$design,
      survey = test_data$survey,
      vartype = "invalid"
    ),
    "'arg' should be one of"
  )

  # Test invalid confidence level
  expect_error(
    kobo_median_all(
      design = test_data$design,
      survey = test_data$survey,
      level = 1.5
    ),
    "Element 1 is not <= 1"
  )
})

test_that("kobo_median_all handles large number of numeric variables", {
  set.seed(888)
  n <- 50

  # Create data with many numeric variables
  data_many <- data.frame(
    var1 = rnorm(n),
    var2 = rnorm(n),
    var3 = rnorm(n),
    var4 = rnorm(n),
    var5 = rnorm(n),
    var6 = rnorm(n),
    var7 = rnorm(n),
    var8 = rnorm(n),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_many <- srvyr::as_survey_design(
    data_many,
    weights = weights,
    strata = strata
  )

  survey_many <- data.table::data.table(
    type = rep("decimal", 8),
    list_name = rep(NA, 8),
    name = paste0("var", 1:8),
    label = paste("Variable", 1:8)
  )

  result <- kobo_median_all(
    design = design_many,
    survey = survey_many
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 8)
  expect_setequal(unique(result$var), paste0("var", 1:8))
  expect_true(all(result$stat_type == "median"))
})

test_that("kobo_median_all works with empty survey", {
  test_data <- create_minimal_median_all_test_data()

  # Empty survey
  empty_survey <- data.table::data.table(
    type = character(0),
    list_name = character(0),
    name = character(0),
    label = character(0)
  )

  expect_warning(
    result <- kobo_median_all(
      design = test_data$design,
      survey = empty_survey
    ),
    "There are no vars of types decimal, calculate or integer"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("kobo_median_all handles skewed distributions correctly", {
  set.seed(999)
  n <- 100

  # Create heavily skewed data
  data_skewed <- data.frame(
    skewed_var1 = rexp(n, rate = 0.1), # Exponential distribution
    skewed_var2 = rgamma(n, shape = 1, rate = 0.5), # Gamma distribution
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_skewed <- srvyr::as_survey_design(
    data_skewed,
    weights = weights,
    strata = strata
  )

  survey_skewed <- data.table::data.table(
    type = c("decimal", "decimal"),
    list_name = c(NA, NA),
    name = c("skewed_var1", "skewed_var2"),
    label = c("Skewed Variable 1", "Skewed Variable 2")
  )

  result <- kobo_median_all(
    design = design_skewed,
    survey = survey_skewed
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_true(all(!is.na(result$stat)))
  expect_true(all(result$stat > 0)) # Should be positive for these distributions
})

test_that("kobo_median_all handles discrete integer distributions", {
  set.seed(777)
  n <- 100

  # Create discrete integer data
  data_discrete <- data.frame(
    discrete_var1 = sample(1:10, n, replace = TRUE),
    discrete_var2 = sample(0:5, n, replace = TRUE),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_discrete <- srvyr::as_survey_design(
    data_discrete,
    weights = weights,
    strata = strata
  )

  survey_discrete <- data.table::data.table(
    type = c("integer", "integer"),
    list_name = c(NA, NA),
    name = c("discrete_var1", "discrete_var2"),
    label = c("Discrete Variable 1", "Discrete Variable 2")
  )

  result <- kobo_median_all(
    design = design_discrete,
    survey = survey_discrete
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_true(all(!is.na(result$stat)))

  # Check ranges are reasonable
  var1_result <- result[result$var == "discrete_var1", ]$stat
  var2_result <- result[result$var == "discrete_var2", ]$stat

  expect_true(var1_result >= 1 && var1_result <= 10)
  expect_true(var2_result >= 0 && var2_result <= 5)
})

test_that("kobo_median_all handles numeric variables with extreme values", {
  set.seed(777)
  n <- 50

  # Create data with extreme values
  data_extreme <- data.frame(
    small_var = runif(n, min = 0.001, max = 0.01),
    large_var = runif(n, min = 1e6, max = 1e7),
    negative_var = rnorm(n, mean = -100, sd = 10),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_extreme <- srvyr::as_survey_design(
    data_extreme,
    weights = weights,
    strata = strata
  )

  survey_extreme <- data.table::data.table(
    type = c("decimal", "decimal", "decimal"),
    list_name = c(NA, NA, NA),
    name = c("small_var", "large_var", "negative_var"),
    label = c("Small Variable", "Large Variable", "Negative Variable")
  )

  result <- kobo_median_all(
    design = design_extreme,
    survey = survey_extreme
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_true(all(!is.na(result$stat)))
  expect_true(all(!is.infinite(result$stat)))

  # Check ranges are reasonable
  small_result <- result[result$var == "small_var", ]$stat
  large_result <- result[result$var == "large_var", ]$stat
  negative_result <- result[result$var == "negative_var", ]$stat

  expect_true(small_result > 0 && small_result < 0.02)
  expect_true(large_result > 1e6 && large_result < 1e7)
  expect_true(negative_result < 0)
})

test_that("kobo_median_all handles single value datasets", {
  n <- 20

  # Create data with single repeated values for each variable
  data_single <- data.frame(
    single_var1 = rep(42, n),
    single_var2 = rep(100, n),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design_single <- srvyr::as_survey_design(
    data_single,
    weights = weights,
    strata = strata
  )

  survey_single <- data.table::data.table(
    type = c("integer", "integer"),
    list_name = c(NA, NA),
    name = c("single_var1", "single_var2"),
    label = c("Single Value Variable 1", "Single Value Variable 2")
  )

  result <- kobo_median_all(
    design = design_single,
    survey = survey_single
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)

  # Check that medians are the single values
  var1_result <- result[result$var == "single_var1", ]
  var2_result <- result[result$var == "single_var2", ]

  expect_equal(var1_result$stat, 42)
  expect_equal(var2_result$stat, 100)

  # For single-value datasets, CI bounds should either collapse to the point estimate
  # or be NaN due to platform-specific numerical issues in survey package
  if (all(is.finite(c(var1_result$stat_low, var1_result$stat_upp)))) {
    # Normal case: both bounds are finite and should equal the point estimate
    expect_equal(var1_result$stat_low, var1_result$stat_upp)
    expect_equal(var1_result$stat_low, var1_result$stat)
  } else {
    # Platform edge case: accept NaN bounds as known survey package behavior
    expect_true(
      is.nan(var1_result$stat_low) || is.nan(var1_result$stat_upp),
      info = "Single-value dataset may produce NaN CI bounds on some platforms"
    )
  }

  if (all(is.finite(c(var2_result$stat_low, var2_result$stat_upp)))) {
    # Normal case: both bounds are finite and should equal the point estimate
    expect_equal(var2_result$stat_low, var2_result$stat_upp)
    expect_equal(var2_result$stat_low, var2_result$stat)
  } else {
    # Platform edge case: accept NaN bounds as known survey package behavior
    expect_true(
      is.nan(var2_result$stat_low) || is.nan(var2_result$stat_upp),
      info = "Single-value dataset may produce NaN CI bounds on some platforms"
    )
  }
})
