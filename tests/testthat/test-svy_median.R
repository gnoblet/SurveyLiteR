# Test file for svy_median function
# Tests compare SurveyLiteR svy_median function with direct srvyr calls

# Helper function to create test data for svy_median
create_median_test_data <- function() {
  set.seed(123)
  data.frame(
    numeric_var1 = rnorm(100, mean = 50, sd = 10),
    numeric_var2 = runif(100, min = 0, max = 100),
    numeric_var3 = rexp(100, rate = 0.1), # Exponential for skewed data
    group_var1 = sample(c("Group1", "Group2"), 100, replace = TRUE),
    group_var2 = sample(c("A", "B", "C"), 100, replace = TRUE),
    weights = runif(100, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), 100, replace = TRUE)
  )
}

# Helper function to create survey design
create_median_survey_design <- function(data) {
  srvyr::as_survey_design(data, weights = weights, strata = strata)
}

test_that("svy_median produces correct estimates without grouping", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  # Test single variable
  result_svy <- svy_median(design, vars = "numeric_var1", ak = FALSE)

  # Direct srvyr comparison
  direct_result <- design %>%
    srvyr::summarise(
      stat = srvyr::survey_median(numeric_var1, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  expect_equal(result_svy$stat[1], direct_result$stat, tolerance = 1e-10)
  expect_equal(
    result_svy$stat_low[1],
    direct_result$stat_low,
    tolerance = 1e-10
  )
  expect_equal(
    result_svy$stat_upp[1],
    direct_result$stat_upp,
    tolerance = 1e-10
  )

  # Check structure
  expect_s3_class(result_svy, "data.frame")
  expect_equal(nrow(result_svy), 1)
  expect_equal(result_svy$var[1], "numeric_var1")
  expect_equal(result_svy$stat_type[1], "median")
  expect_true(is.na(result_svy$var_value[1]))
})

test_that("svy_median produces correct estimates with single grouping", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  # Test with grouping
  result_grouped <- svy_median(
    design,
    vars = "numeric_var1",
    group = "group_var1",
    ak = FALSE
  )

  # Direct srvyr comparison
  direct_grouped <- design %>%
    srvyr::group_by(group_var1) %>%
    srvyr::summarise(
      stat = srvyr::survey_median(numeric_var1, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Sort both results for comparison
  result_sorted <- result_grouped[order(result_grouped$group_key_value), ]
  direct_sorted <- direct_grouped[order(direct_grouped$group_var1), ]

  expect_equal(result_sorted$stat, direct_sorted$stat, tolerance = 1e-10)
  expect_equal(
    result_sorted$stat_low,
    direct_sorted$stat_low,
    tolerance = 1e-10
  )
  expect_equal(
    result_sorted$stat_upp,
    direct_sorted$stat_upp,
    tolerance = 1e-10
  )

  # Check structure
  expect_equal(nrow(result_grouped), 2) # Two groups
  expect_true("group_key" %in% colnames(result_grouped))
  expect_true("group_key_value" %in% colnames(result_grouped))
  expect_equal(unique(result_grouped$group_key), "group_var1")
  expect_setequal(result_grouped$group_key_value, c("Group1", "Group2"))
})

test_that("svy_median produces correct estimates with multiple grouping", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  # Test with multiple grouping variables
  result_multi_group <- svy_median(
    design,
    vars = "numeric_var1",
    group = c("group_var1", "group_var2"),
    ak = FALSE
  )

  # Direct srvyr comparison
  direct_multi <- design %>%
    srvyr::group_by(group_var1, group_var2) %>%
    srvyr::summarise(
      stat = srvyr::survey_median(numeric_var1, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Check structure
  expect_true(nrow(result_multi_group) >= 4) # At least 2 × 2 combinations
  expect_true(grepl(" -/- ", result_multi_group$group_key_value[1])) # Contains separator
  expect_equal(nrow(result_multi_group), nrow(direct_multi))
})

test_that("svy_median handles multiple variables correctly", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  # Test multiple variables
  result_multi <- svy_median(
    design,
    vars = c("numeric_var1", "numeric_var2", "numeric_var3"),
    ak = FALSE
  )

  expect_equal(nrow(result_multi), 3)
  expect_setequal(
    result_multi$var,
    c("numeric_var1", "numeric_var2", "numeric_var3")
  )
  expect_true(all(result_multi$stat_type == "median"))

  # Check that each variable gets correct median
  for (i in 1:3) {
    var_name <- paste0("numeric_var", i)
    var_result <- result_multi[result_multi$var == var_name, ]

    direct_result <- design %>%
      srvyr::summarise(
        stat = srvyr::survey_median(
          !!rlang::sym(var_name),
          na.rm = TRUE,
          vartype = "ci"
        ),
        .groups = "drop"
      )

    expect_equal(var_result$stat, direct_result$stat, tolerance = 1e-10)
  }
})

test_that("svy_median handles different variance types", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  # Test confidence intervals
  result_ci <- svy_median(
    design,
    vars = "numeric_var1",
    vartype = "ci",
    ak = FALSE
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))
  expect_false("stat_se" %in% colnames(result_ci))

  # Test standard errors
  result_se <- svy_median(
    design,
    vars = "numeric_var1",
    vartype = "se",
    ak = FALSE
  )
  expect_true("stat_se" %in% colnames(result_se))
  expect_false(any(c("stat_low", "stat_upp") %in% colnames(result_se)))

  # Test variances
  result_var <- svy_median(
    design,
    vars = "numeric_var1",
    vartype = "var",
    ak = FALSE
  )
  expect_true("stat_var" %in% colnames(result_var))
  expect_false(any(
    c("stat_low", "stat_upp", "stat_se") %in% colnames(result_var)
  ))

  # Compare estimates (should be the same regardless of vartype)
  expect_equal(result_ci$stat[1], result_se$stat[1], tolerance = 1e-10)
  expect_equal(result_ci$stat[1], result_var$stat[1], tolerance = 1e-10)
})

test_that("svy_median handles NA values correctly", {
  # Create data with NAs
  data_with_na <- create_median_test_data()
  data_with_na$numeric_var1[1:15] <- NA

  design <- create_median_survey_design(data_with_na)

  # Test with na_rm = TRUE - skip on platforms with known survey package issues
  if (Sys.info()["sysname"] == "Darwin") {
    skip("Skipping on macOS due to platform-specific survey package CI issues")
  }

  result_na_rm <- svy_median(
    design,
    vars = "numeric_var1",
    na_rm = TRUE,
    ak = FALSE
  )

  # Test with na_rm = FALSE
  result_na_keep <- svy_median(
    design,
    vars = "numeric_var1",
    na_rm = FALSE,
    ak = FALSE
  )

  # Both should report correct NA count
  expect_equal(result_na_rm$na_count_tot[1], 15)
  expect_equal(result_na_keep$na_count_tot[1], 15)
  expect_equal(result_na_rm$n_tot[1], 100)
  expect_equal(result_na_keep$n_tot[1], 100)

  # Compare with direct srvyr calls
  direct_na_rm <- design %>%
    srvyr::summarise(
      stat = srvyr::survey_median(numeric_var1, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  expect_equal(result_na_rm$stat[1], direct_na_rm$stat, tolerance = 1e-10)
})

test_that("svy_median respects confidence levels", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  # Test different confidence levels
  levels <- c(0.90, 0.95, 0.99)
  ci_widths <- numeric(length(levels))

  for (i in seq_along(levels)) {
    result <- svy_median(
      design,
      vars = "numeric_var1",
      level = levels[i],
      ak = FALSE
    )
    ci_widths[i] <- result$stat_upp[1] - result$stat_low[1]

    # Compare with direct srvyr call
    direct_result <- design %>%
      srvyr::summarise(
        stat = srvyr::survey_median(
          numeric_var1,
          na.rm = TRUE,
          level = levels[i],
          vartype = "ci"
        ),
        .groups = "drop"
      )

    expect_equal(result$stat[1], direct_result$stat, tolerance = 1e-10)
    expect_equal(result$stat_low[1], direct_result$stat_low, tolerance = 1e-10)
    expect_equal(result$stat_upp[1], direct_result$stat_upp, tolerance = 1e-10)
  }

  # Check that confidence intervals get wider with higher confidence levels
  expect_true(ci_widths[3] > ci_widths[2]) # 99% > 95%
  expect_true(ci_widths[2] > ci_widths[1]) # 95% > 90%
})

test_that("svy_median analysis key functionality works", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  # Test with analysis key
  result_ak <- svy_median(design, vars = "numeric_var1", ak = TRUE)
  result_no_ak <- svy_median(design, vars = "numeric_var1", ak = FALSE)

  expect_true("analysis_key" %in% colnames(result_ak))
  expect_false("analysis_key" %in% colnames(result_no_ak))
  expect_true(grepl("median", result_ak$analysis_key[1]))
  expect_true(grepl("numeric_var1", result_ak$analysis_key[1]))

  # Test with grouping
  result_grouped_ak <- svy_median(
    design,
    vars = "numeric_var1",
    group = "group_var1",
    ak = TRUE
  )

  expect_true("analysis_key" %in% colnames(result_grouped_ak))
  expect_true(all(grepl("median", result_grouped_ak$analysis_key)))
  expect_true(all(grepl("numeric_var1", result_grouped_ak$analysis_key)))
  expect_true(all(grepl("Group", result_grouped_ak$analysis_key)))
})

test_that("svy_median calculates unweighted statistics correctly", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  result <- svy_median(design, vars = "numeric_var1", ak = FALSE)
  expected_unweighted <- median(data$numeric_var1, na.rm = TRUE)

  expect_equal(result$stat_unw[1], expected_unweighted, tolerance = 1e-10)
})

test_that("svy_median handles edge cases", {
  # Test with minimal data
  minimal_data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    weights = c(1, 1, 1, 1, 1),
    strata = c("S1", "S1", "S1", "S1", "S1")
  )

  design <- create_median_survey_design(minimal_data)
  result <- svy_median(design, vars = "x", ak = FALSE)

  expect_equal(nrow(result), 1)
  expect_true(is.numeric(result$stat[1]))
  expect_equal(result$stat[1], 3) # Median of 1, 2, 3, 4, 5

  # Test with even number of observations
  even_data <- data.frame(
    x = c(1, 2, 3, 4),
    weights = c(1, 1, 1, 1),
    strata = c("S1", "S1", "S1", "S1")
  )

  design_even <- create_median_survey_design(even_data)
  result_even <- svy_median(design_even, vars = "x", ak = FALSE)

  # Compare with direct srvyr call
  direct_even <- design_even %>%
    srvyr::summarise(
      stat = srvyr::survey_median(x, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  expect_equal(nrow(result_even), 1)
  expect_true(is.numeric(result_even$stat[1]))
  expect_equal(result_even$stat[1], direct_even$stat, tolerance = 1e-10)
})

test_that("svy_median handles all NA variables correctly", {
  # Create data with all NAs
  data_all_na <- create_median_test_data()
  data_all_na$numeric_var1 <- rep(NA_real_, nrow(data_all_na))

  design <- create_median_survey_design(data_all_na)

  # Should return empty data frame with warning
  expect_warning(
    result_all_na <- svy_median(design, vars = "numeric_var1", ak = FALSE),
    "only contains missing values"
  )
  expect_equal(nrow(result_all_na), 0)
})

test_that("svy_median works with skewed data", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  # Test with exponential (highly skewed) data
  result_exp <- svy_median(design, vars = "numeric_var3", ak = FALSE)

  direct_result <- design %>%
    srvyr::summarise(
      stat = srvyr::survey_median(numeric_var3, na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  expect_equal(result_exp$stat[1], direct_result$stat, tolerance = 1e-10)
  expect_equal(
    result_exp$stat_low[1],
    direct_result$stat_low,
    tolerance = 1e-10
  )
  expect_equal(
    result_exp$stat_upp[1],
    direct_result$stat_upp,
    tolerance = 1e-10
  )

  # Median should be robust to outliers (unlike mean)
  expect_true(is.finite(result_exp$stat[1]))
  expect_true(result_exp$stat[1] > 0) # Exponential data is positive
})

test_that("svy_median parameter validation works", {
  data <- create_median_test_data()
  design <- create_median_survey_design(data)

  # Test invalid vartype (error comes from srvyr, not our validation)
  expect_error(
    svy_median(design, vars = "numeric_var1", vartype = "invalid"),
    "'arg' should be one of"
  )

  # Test invalid confidence level
  expect_error(
    svy_median(design, vars = "numeric_var1", level = 1.5),
    "Element 1 is not <= 1"
  )

  # Test missing variables
  expect_error(
    svy_median(design, vars = "nonexistent_var"),
    "Missing variables"
  )
})
