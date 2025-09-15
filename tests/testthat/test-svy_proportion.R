# Test file for svy_proportion function
# Tests compare SurveyLiteR svy_proportion function with direct srvyr calls

# Helper function to create test data for svy_proportion
create_proportion_test_data <- function() {
  set.seed(123)
  data.frame(
    categorical_var1 = sample(
      c("A", "B", "C"),
      100,
      replace = TRUE,
      prob = c(0.4, 0.35, 0.25)
    ),
    binary_var = sample(
      c("Yes", "No"),
      100,
      replace = TRUE,
      prob = c(0.6, 0.4)
    ),
    ordered_var = sample(
      c("Low", "Medium", "High"),
      100,
      replace = TRUE,
      prob = c(0.3, 0.4, 0.3)
    ),
    group_var1 = sample(c("Group1", "Group2"), 100, replace = TRUE),
    group_var2 = sample(c("X", "Y", "Z"), 100, replace = TRUE),
    weights = runif(100, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), 100, replace = TRUE)
  )
}

# Helper function to create survey design
create_proportion_survey_design <- function(data) {
  srvyr::as_survey_design(data, weights = weights, strata = strata)
}

test_that("svy_proportion produces correct estimates without grouping", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  # Test single categorical variable
  result_svy <- svy_proportion(design, vars = "categorical_var1", ak = FALSE)

  # Direct srvyr comparison
  direct_result <- design %>%
    srvyr::group_by(categorical_var1) %>%
    srvyr::summarise(
      stat = srvyr::survey_prop(na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Sort both results for comparison
  result_sorted <- result_svy[order(result_svy$var_value), ]
  direct_sorted <- direct_result[order(direct_result$categorical_var1), ]

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
  expect_s3_class(result_svy, "data.frame")
  expect_equal(nrow(result_svy), 3) # A, B, C categories
  expect_equal(unique(result_svy$var), "categorical_var1")
  expect_equal(unique(result_svy$stat_type), "proportion")
  expect_setequal(result_svy$var_value, c("A", "B", "C"))

  # Check proportions sum to 1
  expect_equal(sum(result_svy$stat), 1.0, tolerance = 1e-10)
})

test_that("svy_proportion produces correct estimates with external grouping", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  # Test with external grouping
  result_grouped <- svy_proportion(
    design,
    vars = "categorical_var1",
    group = "group_var1",
    ak = FALSE
  )

  # Direct srvyr comparison
  direct_grouped <- design %>%
    srvyr::group_by(group_var1, categorical_var1) %>%
    srvyr::summarise(
      stat = srvyr::survey_prop(na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Check that we have the right number of combinations
  expect_equal(nrow(result_grouped), nrow(direct_grouped))

  # Check structure
  expect_true("group_key" %in% colnames(result_grouped))
  expect_true("group_key_value" %in% colnames(result_grouped))
  expect_equal(unique(result_grouped$group_key), "group_var1")
  expect_setequal(unique(result_grouped$group_key_value), c("Group1", "Group2"))

  # Check that proportions sum to 1 within each group
  for (grp in c("Group1", "Group2")) {
    grp_data <- result_grouped[result_grouped$group_key_value == grp, ]
    expect_equal(sum(grp_data$stat), 1.0, tolerance = 1e-8)
  }
})

test_that("svy_proportion produces correct estimates with multiple external grouping", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  # Test with multiple external grouping variables
  result_multi_group <- svy_proportion(
    design,
    vars = "categorical_var1",
    group = c("group_var1", "group_var2"),
    ak = FALSE
  )

  # Direct srvyr comparison
  direct_multi <- design %>%
    srvyr::group_by(group_var1, group_var2, categorical_var1) %>%
    srvyr::summarise(
      stat = srvyr::survey_prop(na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Check structure
  expect_equal(nrow(result_multi_group), nrow(direct_multi))
  expect_true(grepl(" -/- ", result_multi_group$group_key_value[1])) # Contains separator

  # Check that proportions sum to 1 within each group combination
  unique_groups <- unique(result_multi_group$group_key_value)
  for (grp in unique_groups) {
    grp_data <- result_multi_group[result_multi_group$group_key_value == grp, ]
    expect_equal(sum(grp_data$stat), 1.0, tolerance = 1e-8)
  }
})

test_that("svy_proportion handles binary variables correctly", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  # Test binary variable
  result_binary <- svy_proportion(design, vars = "binary_var", ak = FALSE)

  # Direct srvyr comparison
  direct_binary <- design %>%
    srvyr::group_by(binary_var) %>%
    srvyr::summarise(
      stat = srvyr::survey_prop(na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Sort both results
  result_sorted <- result_binary[order(result_binary$var_value), ]
  direct_sorted <- direct_binary[order(direct_binary$binary_var), ]

  expect_equal(result_sorted$stat, direct_sorted$stat, tolerance = 1e-10)
  expect_equal(nrow(result_binary), 2) # Yes/No
  expect_setequal(result_binary$var_value, c("Yes", "No"))
  expect_equal(sum(result_binary$stat), 1.0, tolerance = 1e-10)
})

test_that("svy_proportion handles multiple variables correctly", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  # Test multiple variables
  result_multi <- svy_proportion(
    design,
    vars = c("categorical_var1", "binary_var"),
    ak = FALSE
  )

  expect_equal(nrow(result_multi), 5) # 3 categories + 2 binary
  expect_setequal(unique(result_multi$var), c("categorical_var1", "binary_var"))
  expect_true(all(result_multi$stat_type == "proportion"))

  # Check each variable separately
  cat_results <- result_multi[result_multi$var == "categorical_var1", ]
  binary_results <- result_multi[result_multi$var == "binary_var", ]

  expect_equal(sum(cat_results$stat), 1.0, tolerance = 1e-10)
  expect_equal(sum(binary_results$stat), 1.0, tolerance = 1e-10)
})

test_that("svy_proportion handles different variance types", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  # Test confidence intervals
  result_ci <- svy_proportion(
    design,
    vars = "categorical_var1",
    vartype = "ci",
    ak = FALSE
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))
  expect_false("stat_se" %in% colnames(result_ci))

  # Test standard errors
  result_se <- svy_proportion(
    design,
    vars = "categorical_var1",
    vartype = "se",
    ak = FALSE
  )
  expect_true("stat_se" %in% colnames(result_se))
  expect_false(any(c("stat_low", "stat_upp") %in% colnames(result_se)))

  # Test variances
  result_var <- svy_proportion(
    design,
    vars = "categorical_var1",
    vartype = "var",
    ak = FALSE
  )
  expect_true("stat_var" %in% colnames(result_var))
  expect_false(any(
    c("stat_low", "stat_upp", "stat_se") %in% colnames(result_var)
  ))

  # Compare estimates (should be the same regardless of vartype)
  expect_equal(result_ci$stat, result_se$stat, tolerance = 1e-10)
  expect_equal(result_ci$stat, result_var$stat, tolerance = 1e-10)
})

test_that("svy_proportion handles NA values correctly", {
  # Create data with NAs
  data_with_na <- create_proportion_test_data()
  data_with_na$categorical_var1[1:10] <- NA

  design <- create_proportion_survey_design(data_with_na)

  # Test with na_rm = TRUE
  result_na_rm <- svy_proportion(
    design,
    vars = "categorical_var1",
    na_rm = TRUE,
    ak = FALSE
  )

  # Test with na_rm = FALSE
  result_na_keep <- svy_proportion(
    design,
    vars = "categorical_var1",
    na_rm = FALSE,
    ak = FALSE
  )

  # Both should report correct NA count
  expect_equal(unique(result_na_rm$na_count_tot), 10)
  expect_equal(unique(result_na_keep$na_count_tot), 10)
  expect_equal(unique(result_na_rm$n_tot), 100)
  expect_equal(unique(result_na_keep$n_tot), 100)

  # With na_rm = TRUE, should only have non-NA categories
  expect_setequal(result_na_rm$var_value, c("A", "B", "C"))
  expect_equal(sum(result_na_rm$stat), 1.0, tolerance = 1e-10)

  # With na_rm = FALSE, might include NA as a category (depending on implementation)
  expect_true(all(result_na_keep$var_value %in% c("A", "B", "C", NA, "NA")))
})

test_that("svy_proportion respects confidence levels", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  # Test different confidence levels
  levels <- c(0.90, 0.95, 0.99)

  for (level in levels) {
    result <- svy_proportion(
      design,
      vars = "categorical_var1",
      level = level,
      ak = FALSE
    )

    # Compare with direct srvyr call
    direct_result <- design %>%
      srvyr::group_by(categorical_var1) %>%
      srvyr::summarise(
        stat = srvyr::survey_prop(
          na.rm = TRUE,
          level = level,
          vartype = "ci"
        ),
        .groups = "drop"
      )

    # Sort both for comparison
    result_sorted <- result[order(result$var_value), ]
    direct_sorted <- direct_result[order(direct_result$categorical_var1), ]

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

    # Check that CI widths are reasonable
    ci_widths <- result$stat_upp - result$stat_low
    expect_true(all(ci_widths >= 0))
    expect_true(all(ci_widths < 1)) # CI width should be less than 1 for proportions
  }
})

test_that("svy_proportion analysis key functionality works", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  # Test with analysis key
  result_ak <- svy_proportion(design, vars = "categorical_var1", ak = TRUE)
  result_no_ak <- svy_proportion(design, vars = "categorical_var1", ak = FALSE)

  expect_true("analysis_key" %in% colnames(result_ak))
  expect_false("analysis_key" %in% colnames(result_no_ak))
  expect_true(all(grepl("proportion", result_ak$analysis_key)))
  expect_true(all(grepl("categorical_var1", result_ak$analysis_key)))

  # Test with grouping
  result_grouped_ak <- svy_proportion(
    design,
    vars = "categorical_var1",
    group = "group_var1",
    ak = TRUE
  )

  expect_true("analysis_key" %in% colnames(result_grouped_ak))
  expect_true(all(grepl("proportion", result_grouped_ak$analysis_key)))
  expect_true(all(grepl("categorical_var1", result_grouped_ak$analysis_key)))
  expect_true(all(grepl("Group", result_grouped_ak$analysis_key)))
})

test_that("svy_proportion calculates unweighted statistics correctly", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  result <- svy_proportion(design, vars = "categorical_var1", ak = FALSE)

  # Calculate expected unweighted proportions
  expected_props <- table(data$categorical_var1) / nrow(data)

  # Sort both for comparison
  result_sorted <- result[order(result$var_value), ]
  expected_sorted <- expected_props[order(names(expected_props))]

  expect_equal(
    result_sorted$stat_unw,
    as.numeric(expected_sorted),
    tolerance = 1e-10
  )
})

test_that("svy_proportion handles edge cases", {
  # Test with minimal data
  minimal_data <- data.frame(
    cat = c("A", "B", "A"),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1")
  )

  design <- create_proportion_survey_design(minimal_data)
  result <- svy_proportion(design, vars = "cat", ak = FALSE)

  expect_equal(nrow(result), 2) # A and B categories
  expect_setequal(result$var_value, c("A", "B"))
  expect_equal(sum(result$stat), 1.0, tolerance = 1e-10)

  # Test with single category (all same value)
  single_cat_data <- data.frame(
    cat = c("A", "A", "A", "A"),
    weights = c(1, 1, 1, 1),
    strata = c("S1", "S1", "S1", "S1")
  )

  design_single <- create_proportion_survey_design(single_cat_data)
  result_single <- svy_proportion(design_single, vars = "cat", ak = FALSE)

  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$var_value[1], "A")
  expect_equal(result_single$stat[1], 1.0)
  expect_equal(result_single$stat_low[1], result_single$stat_upp[1]) # CI should collapse
})

test_that("svy_proportion handles all NA variables correctly", {
  # Create data with all NAs
  data_all_na <- create_proportion_test_data()
  data_all_na$categorical_var1 <- rep(NA_character_, nrow(data_all_na))

  design <- create_proportion_survey_design(data_all_na)

  # Should return empty data frame with warning
  expect_warning(
    result_all_na <- svy_proportion(
      design,
      vars = "categorical_var1",
      ak = FALSE
    ),
    "only contains missing values"
  )
  expect_equal(nrow(result_all_na), 0)
})

test_that("svy_proportion works with factor variables", {
  data <- create_proportion_test_data()

  # Convert to factor
  data$categorical_var1 <- factor(
    data$categorical_var1,
    levels = c("C", "B", "A")
  )

  design <- create_proportion_survey_design(data)
  result <- svy_proportion(design, vars = "categorical_var1", ak = FALSE)

  # Should still work with factors
  expect_equal(nrow(result), 3)
  expect_setequal(result$var_value, c("A", "B", "C"))
  expect_equal(sum(result$stat), 1.0, tolerance = 1e-10)
})

test_that("svy_proportion handles ordered factors", {
  data <- create_proportion_test_data()

  # Convert to ordered factor
  data$ordered_var <- factor(
    data$ordered_var,
    levels = c("Low", "Medium", "High"),
    ordered = TRUE
  )

  design <- create_proportion_survey_design(data)
  result <- svy_proportion(design, vars = "ordered_var", ak = FALSE)

  expect_equal(nrow(result), 3)
  expect_setequal(result$var_value, c("Low", "Medium", "High"))
  expect_equal(sum(result$stat), 1.0, tolerance = 1e-8)
})

test_that("svy_proportion parameter validation works", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  # Test invalid vartype (error comes from srvyr, not our validation)
  expect_error(
    svy_proportion(design, vars = "categorical_var1", vartype = "invalid"),
    "'arg' should be one of"
  )

  # Test invalid confidence level
  expect_error(
    svy_proportion(design, vars = "categorical_var1", level = 1.5),
    "Element 1 is not <= 1"
  )

  # Test missing variables
  expect_error(
    svy_proportion(design, vars = "nonexistent_var"),
    "Missing variables"
  )
})

test_that("svy_proportion var_value column is character", {
  data <- create_proportion_test_data()
  design <- create_proportion_survey_design(data)

  result <- svy_proportion(design, vars = "categorical_var1", ak = FALSE)

  expect_true(is.character(result$var_value))
  expect_false(is.factor(result$var_value))
})

test_that("svy_proportion handles numeric categorical variables", {
  data <- create_proportion_test_data()

  # Add a numeric categorical variable
  data$numeric_cat <- sample(c(1, 2, 3), 100, replace = TRUE)

  design <- create_proportion_survey_design(data)
  result <- svy_proportion(design, vars = "numeric_cat", ak = FALSE)

  expect_equal(nrow(result), 3)
  expect_setequal(result$var_value, c("1", "2", "3")) # Should be converted to character
  expect_equal(sum(result$stat), 1.0, tolerance = 1e-10)
})
