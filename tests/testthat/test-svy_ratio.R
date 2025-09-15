# Test file for svy_ratio function
# Tests compare SurveyLiteR svy_ratio function with direct srvyr calls

# Helper function to create test data for svy_ratio
create_ratio_test_data <- function() {
  set.seed(123)
  data.frame(
    numerator1 = rnorm(100, mean = 50, sd = 10),
    numerator2 = runif(100, min = 10, max = 100),
    denominator1 = rnorm(100, mean = 25, sd = 5),
    denominator2 = runif(100, min = 5, max = 50),
    # Ensure denominators are positive for meaningful ratios
    pos_denom = abs(rnorm(100, mean = 20, sd = 3)) + 1,
    group_var1 = sample(c("Group1", "Group2"), 100, replace = TRUE),
    group_var2 = sample(c("A", "B", "C"), 100, replace = TRUE),
    weights = runif(100, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), 100, replace = TRUE)
  )
}

# Helper function to create survey design
create_ratio_survey_design <- function(data) {
  srvyr::as_survey_design(data, weights = weights, strata = strata)
}

test_that("svy_ratio produces correct estimates without grouping", {
  data <- create_ratio_test_data()
  design <- create_ratio_survey_design(data)

  # Test single ratio
  result_svy <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    ak = FALSE
  )

  # Direct srvyr comparison
  direct_result <- design %>%
    srvyr::summarise(
      stat = srvyr::survey_ratio(
        numerator1,
        denominator1,
        na.rm = TRUE,
        vartype = "ci"
      ),
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
  expect_true(grepl("numerator1.*denominator1", result_svy$var[1]))
  expect_equal(result_svy$stat_type[1], "ratio")
  expect_true(is.na(result_svy$var_value[1]))
})

test_that("svy_ratio produces correct estimates with single grouping", {
  data <- create_ratio_test_data()
  design <- create_ratio_survey_design(data)

  # Test with grouping
  result_grouped <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    group = "group_var1",
    ak = FALSE
  )

  # Direct srvyr comparison
  direct_grouped <- design %>%
    srvyr::group_by(group_var1) %>%
    srvyr::summarise(
      stat = srvyr::survey_ratio(
        numerator1,
        denominator1,
        na.rm = TRUE,
        vartype = "ci"
      ),
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

test_that("svy_ratio produces correct estimates with multiple grouping", {
  data <- create_ratio_test_data()
  design <- create_ratio_survey_design(data)

  # Test with multiple grouping variables
  result_multi_group <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    group = c("group_var1", "group_var2"),
    ak = FALSE
  )

  # Direct srvyr comparison
  direct_multi <- design %>%
    srvyr::group_by(group_var1, group_var2) %>%
    srvyr::summarise(
      stat = srvyr::survey_ratio(
        numerator1,
        denominator1,
        na.rm = TRUE,
        vartype = "ci"
      ),
      .groups = "drop"
    )

  # Check structure
  expect_true(nrow(result_multi_group) >= 4) # At least 2 × 2 combinations
  expect_true(grepl(" -/- ", result_multi_group$group_key_value[1])) # Contains separator
  expect_equal(nrow(result_multi_group), nrow(direct_multi))
})

test_that("svy_ratio handles multiple ratios correctly", {
  data <- create_ratio_test_data()
  design <- create_ratio_survey_design(data)

  # Test multiple ratios with same denominator
  result_multi_same_denom <- svy_ratio(
    design,
    nums = c("numerator1", "numerator2"),
    denoms = c("denominator1", "denominator1"),
    ak = FALSE
  )

  expect_equal(nrow(result_multi_same_denom), 2)
  expect_true(all(grepl("denominator1", result_multi_same_denom$var)))
  expect_true(all(result_multi_same_denom$stat_type == "ratio"))

  # Test multiple ratios with different denominators
  result_multi_diff_denom <- svy_ratio(
    design,
    nums = c("numerator1", "numerator2"),
    denoms = c("denominator1", "denominator2"),
    ak = FALSE
  )

  expect_equal(nrow(result_multi_diff_denom), 2)
  expect_true(any(grepl("denominator1", result_multi_diff_denom$var)))
  expect_true(any(grepl("denominator2", result_multi_diff_denom$var)))

  # Check that each ratio is correct individually
  for (i in 1:2) {
    num_var <- paste0("numerator", i)
    denom_var <- paste0("denominator", i)

    single_result <- svy_ratio(
      design,
      nums = num_var,
      denoms = denom_var,
      ak = FALSE
    )

    multi_row <- result_multi_diff_denom[
      grepl(paste(num_var, denom_var, sep = ".*"), result_multi_diff_denom$var),
    ]

    expect_equal(single_result$stat, multi_row$stat, tolerance = 1e-10)
  }
})

test_that("svy_ratio handles different variance types", {
  data <- create_ratio_test_data()
  design <- create_ratio_survey_design(data)

  # Test confidence intervals
  result_ci <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    vartype = "ci",
    ak = FALSE
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))
  expect_false("stat_se" %in% colnames(result_ci))

  # Test standard errors
  result_se <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    vartype = "se",
    ak = FALSE
  )
  expect_true("stat_se" %in% colnames(result_se))
  expect_false(any(c("stat_low", "stat_upp") %in% colnames(result_se)))

  # Test variances
  result_var <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
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

test_that("svy_ratio handles NA values correctly", {
  # Create data with NAs
  data_with_na <- create_ratio_test_data()
  data_with_na$numerator1[1:10] <- NA
  data_with_na$denominator1[11:15] <- NA

  design <- create_ratio_survey_design(data_with_na)

  # Test with na_rm = TRUE
  result_na_rm <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    na_rm = TRUE,
    ak = FALSE
  )

  # Test with na_rm = FALSE
  result_na_keep <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    na_rm = FALSE,
    ak = FALSE
  )

  # Both should report correct NA count (union of NAs in num and denom)
  expect_equal(result_na_rm$na_count_tot[1], 15) # 10 + 5 unique NAs
  expect_equal(result_na_keep$na_count_tot[1], 15)
  expect_equal(result_na_rm$n_tot[1], 100)
  expect_equal(result_na_keep$n_tot[1], 100)

  # Compare with direct srvyr calls
  direct_na_rm <- design %>%
    srvyr::summarise(
      stat = srvyr::survey_ratio(
        numerator1,
        denominator1,
        na.rm = TRUE,
        vartype = "ci"
      ),
      .groups = "drop"
    )

  expect_equal(result_na_rm$stat[1], direct_na_rm$stat, tolerance = 1e-10)
})

test_that("svy_ratio analysis key functionality works", {
  data <- create_ratio_test_data()
  design <- create_ratio_survey_design(data)

  # Test with analysis key
  result_ak <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    ak = TRUE
  )
  result_no_ak <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    ak = FALSE
  )

  expect_true("analysis_key" %in% colnames(result_ak))
  expect_false("analysis_key" %in% colnames(result_no_ak))
  expect_true(grepl("ratio", result_ak$analysis_key[1]))
  expect_true(grepl("numerator1", result_ak$analysis_key[1]))
  expect_true(grepl("denominator1", result_ak$analysis_key[1]))

  # Test with grouping
  result_grouped_ak <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    group = "group_var1",
    ak = TRUE
  )

  expect_true("analysis_key" %in% colnames(result_grouped_ak))
  expect_true(all(grepl("ratio", result_grouped_ak$analysis_key)))
  expect_true(all(grepl("numerator1", result_grouped_ak$analysis_key)))
  expect_true(all(grepl("denominator1", result_grouped_ak$analysis_key)))
  expect_true(all(grepl("Group", result_grouped_ak$analysis_key)))
})

test_that("svy_ratio calculates unweighted statistics correctly", {
  data <- create_ratio_test_data()
  design <- create_ratio_survey_design(data)

  result <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    ak = FALSE
  )

  # Calculate expected unweighted ratio
  expected_unweighted <- mean(data$numerator1, na.rm = TRUE) /
    mean(data$denominator1, na.rm = TRUE)

  expect_equal(result$stat_unw[1], expected_unweighted, tolerance = 1e-10)
})

test_that("svy_ratio handles edge cases", {
  # Test with minimal data
  minimal_data <- data.frame(
    num = c(10, 20, 30),
    denom = c(2, 4, 6),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1")
  )

  design <- create_ratio_survey_design(minimal_data)
  result <- svy_ratio(design, nums = "num", denoms = "denom", ak = FALSE)

  expect_equal(nrow(result), 1)
  expect_true(is.numeric(result$stat[1]))
  expect_equal(result$stat[1], 5) # (10+20+30)/(2+4+6) = 60/12 = 5

  # Test with zero denominators (should handle gracefully)
  zero_denom_data <- data.frame(
    num = c(1, 2, 3),
    denom = c(0, 1, 2),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1")
  )

  design_zero <- create_ratio_survey_design(zero_denom_data)

  # This might produce infinite values or errors - check it doesn't crash
  expect_no_error({
    result_zero <- svy_ratio(
      design_zero,
      nums = "num",
      denoms = "denom",
      ak = FALSE
    )
  })
})

test_that("svy_ratio handles all NA variables correctly", {
  # Create data with all NAs in numerator
  data_all_na_num <- create_ratio_test_data()
  data_all_na_num$numerator1 <- rep(NA_real_, nrow(data_all_na_num))

  design <- create_ratio_survey_design(data_all_na_num)

  # Should return empty data frame with warning
  expect_warning(
    result_all_na_num <- svy_ratio(
      design,
      nums = "numerator1",
      denoms = "denominator1",
      ak = FALSE
    ),
    "only contains missing values"
  )
  expect_equal(nrow(result_all_na_num), 0)

  # Create data with all NAs in denominator
  data_all_na_denom <- create_ratio_test_data()
  data_all_na_denom$denominator1 <- rep(NA_real_, nrow(data_all_na_denom))

  design_denom <- create_ratio_survey_design(data_all_na_denom)

  # Should return empty data frame with warning
  expect_warning(
    result_all_na_denom <- svy_ratio(
      design_denom,
      nums = "numerator1",
      denoms = "denominator1",
      ak = FALSE
    ),
    "only contains missing values"
  )
  expect_equal(nrow(result_all_na_denom), 0)
})

test_that("svy_ratio custom separators work correctly", {
  data <- create_ratio_test_data()
  design <- create_ratio_survey_design(data)

  # Test custom ratio key separator
  result_custom_sep <- svy_ratio(
    design,
    nums = "numerator1",
    denoms = "denominator1",
    ratio_key_sep = " / ",
    ak = FALSE
  )

  expect_true(grepl(" / ", result_custom_sep$var[1]))
  expect_true(grepl("numerator1 / denominator1", result_custom_sep$var[1]))
})

test_that("svy_ratio parameter validation works", {
  data <- create_ratio_test_data()
  design <- create_ratio_survey_design(data)

  # Test invalid vartype (error comes from srvyr, not our validation)
  expect_error(
    svy_ratio(
      design,
      nums = "numerator1",
      denoms = "denominator1",
      vartype = "invalid"
    ),
    "'arg' should be one of"
  )

  # Test invalid confidence level
  expect_error(
    svy_ratio(
      design,
      nums = "numerator1",
      denoms = "denominator1",
      level = 1.5
    ),
    "Element 1 is not <= 1"
  )

  # Test missing variables
  expect_error(
    svy_ratio(design, nums = "nonexistent_num", denoms = "denominator1"),
    "Missing variables"
  )

  expect_error(
    svy_ratio(design, nums = "numerator1", denoms = "nonexistent_denom"),
    "Missing variables"
  )

  # Test mismatched lengths
  expect_error(
    svy_ratio(
      design,
      nums = c("numerator1", "numerator2"),
      denoms = "denominator1"
    ),
    "same number of numerators and denominators"
  )
})

test_that("svy_ratio handles very small denominators", {
  # Create data with very small denominators
  small_denom_data <- create_ratio_test_data()
  small_denom_data$denominator1 <- runif(100, min = 1e-10, max = 1e-8)

  design <- create_ratio_survey_design(small_denom_data)

  # Should still work but might produce large ratios
  expect_no_error({
    result <- svy_ratio(
      design,
      nums = "numerator1",
      denoms = "denominator1",
      ak = FALSE
    )
  })

  expect_true(is.finite(result$stat[1]) || is.infinite(result$stat[1]))
})

test_that("svy_ratio produces sensible results with real-world ratios", {
  # Create realistic ratio data (e.g., income to expenditure)
  realistic_data <- data.frame(
    income = rnorm(100, mean = 50000, sd = 15000),
    expenditure = rnorm(100, mean = 40000, sd = 10000),
    group = sample(c("Urban", "Rural"), 100, replace = TRUE),
    weights = runif(100, min = 0.8, max = 1.2),
    strata = sample(c("Region1", "Region2", "Region3"), 100, replace = TRUE)
  )

  # Ensure positive values
  realistic_data$income <- pmax(realistic_data$income, 10000)
  realistic_data$expenditure <- pmax(realistic_data$expenditure, 5000)

  design <- create_ratio_survey_design(realistic_data)

  result <- svy_ratio(
    design,
    nums = "income",
    denoms = "expenditure",
    group = "group",
    ak = TRUE
  )

  # Check that ratios are sensible (income/expenditure should be > 1 on average)
  expect_true(all(result$stat > 0))
  expect_true(mean(result$stat) > 1) # Income typically > expenditure
  expect_equal(nrow(result), 2) # Urban and Rural groups
  expect_true("analysis_key" %in% colnames(result))
})
