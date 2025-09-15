# Tests for auto_svy_analysis function

# Helper function to create test data
create_auto_svy_test_data <- function() {
  set.seed(123)
  data.frame(
    # Numeric variables
    numeric_var1 = rnorm(100, mean = 50, sd = 10),
    numeric_var2 = runif(100, min = 0, max = 100),
    numeric_var3 = rgamma(100, shape = 2, rate = 0.1),

    # Character variables
    char_var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    char_var2 = sample(c("X", "Y"), 100, replace = TRUE),
    char_var3 = sample(c("Good", "Fair", "Poor"), 100, replace = TRUE),

    # Grouping variables (character for grouping)
    group_var1 = sample(c("Group1", "Group2"), 100, replace = TRUE),
    group_var2 = sample(c("Rural", "Urban"), 100, replace = TRUE),

    # Survey design variables
    survey_weights = runif(100, min = 0.5, max = 2.0),
    survey_strata = sample(
      c("Stratum1", "Stratum2", "Stratum3"),
      100,
      replace = TRUE
    )
  )
}

# Helper function to create survey design
create_auto_svy_survey_design <- function(data) {
  srvyr::as_survey_design(
    data,
    weights = survey_weights,
    strata = survey_strata
  )
}

# Test basic functionality
test_that("auto_svy_analysis works with basic inputs", {
  data <- create_auto_svy_test_data()
  design <- create_auto_svy_survey_design(data)

  result <- auto_svy_analysis(design)

  # Should return a data.table
  expect_s3_class(result, "data.table")

  # Should have required columns
  expect_true(all(c("stat_type", "var", "stat") %in% colnames(result)))

  # Should include all numeric variables
  numeric_vars <- c("numeric_var1", "numeric_var2", "numeric_var3")
  expect_true(all(numeric_vars %in% result$var))

  # Should include character variables (excluding grouping variables)
  character_vars <- c("char_var1", "char_var2", "char_var3")
  expect_true(all(character_vars %in% result$var))

  # Should have mean and median for numeric variables
  for (var in numeric_vars) {
    expect_true(any(result$stat_type == "mean" & result$var == var))
    expect_true(any(result$stat_type == "median" & result$var == var))
  }

  # Should have proportion for character variables
  for (var in character_vars) {
    expect_true(any(result$stat_type == "proportion" & result$var == var))
  }
})

test_that("auto_svy_analysis works with grouping", {
  data <- create_auto_svy_test_data()
  design <- create_auto_svy_survey_design(data)

  result <- auto_svy_analysis(design, group = "group_var1")

  # Should return a data.table
  expect_s3_class(result, "data.table")

  # Should have group columns
  expect_true("group_var1" %in% colnames(result))
  expect_true("group_key" %in% colnames(result))

  # Should have both group levels
  expect_true(all(c("Group1", "Group2") %in% result$group_var1))

  # Check that we have results for both groups (group_key contains group variable name)
  expect_true(all(result$group_key == "group_var1"))
})

test_that("auto_svy_analysis works with multiple grouping variables", {
  data <- create_auto_svy_test_data()
  design <- create_auto_svy_survey_design(data)

  result <- auto_svy_analysis(design, group = c("group_var1", "group_var2"))

  # Should return a data.table
  expect_s3_class(result, "data.table")

  # Should have both group columns
  expect_true(all(
    c("group_var1", "group_var2", "group_key") %in% colnames(result)
  ))

  # Group key should contain the separator for multiple groups
  expect_true(all(grepl(" -/- ", result$group_key)))
})

test_that("auto_svy_analysis works with custom group_key_sep", {
  data <- create_auto_svy_test_data()
  design <- create_auto_svy_survey_design(data)

  result <- auto_svy_analysis(
    design,
    group = c("group_var1", "group_var2"),
    group_key_sep = " | "
  )

  # Should use custom separator
  expect_true(all(grepl(" \\| ", result$group_key)))
  expect_false(any(grepl(" -/- ", result$group_key)))
})

test_that("auto_svy_analysis handles na_rm parameter", {
  # Skip on platforms with known survey package CI issues
  if (Sys.info()["sysname"] == "Darwin") {
    skip("Skipping on macOS due to platform-specific survey package CI issues")
  }

  data <- create_auto_svy_test_data()
  # Add some NAs
  data$numeric_var1[1:5] <- NA
  data$char_var1[1:3] <- NA
  design <- create_auto_svy_survey_design(data)

  # Test with na_rm = TRUE (default)
  result_rm <- auto_svy_analysis(design, na_rm = TRUE)

  # Test with na_rm = FALSE
  result_keep <- auto_svy_analysis(design, na_rm = FALSE)

  # Both should return data.tables
  expect_s3_class(result_rm, "data.table")
  expect_s3_class(result_keep, "data.table")

  # Results should be different (though we can't easily test specific values)
  # At minimum, check that both return reasonable results
  expect_true(nrow(result_rm) > 0)
  expect_true(nrow(result_keep) > 0)
})

test_that("auto_svy_analysis works with different confidence levels", {
  data <- create_auto_svy_test_data()
  design <- create_auto_svy_survey_design(data)

  result_95 <- auto_svy_analysis(design, level = 0.95)
  result_90 <- auto_svy_analysis(design, level = 0.90)

  # Should have confidence interval columns
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_95)))
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_90)))

  # 90% CI should be narrower than 95% CI (for the same variable)
  numeric_mean_95 <- result_95[stat_type == "mean" & var == "numeric_var1"][1]
  numeric_mean_90 <- result_90[stat_type == "mean" & var == "numeric_var1"][1]

  ci_width_95 <- numeric_mean_95$stat_upp - numeric_mean_95$stat_low
  ci_width_90 <- numeric_mean_90$stat_upp - numeric_mean_90$stat_low

  expect_true(ci_width_90 < ci_width_95)
})

test_that("auto_svy_analysis works with different vartypes", {
  data <- create_auto_svy_test_data()
  design <- create_auto_svy_survey_design(data)

  # Test with confidence intervals (default)
  result_ci <- auto_svy_analysis(design, vartype = "ci")
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))

  # Test with standard errors
  result_se <- auto_svy_analysis(design, vartype = "se")
  expect_true("stat_se" %in% colnames(result_se))
  expect_false(all(c("stat_low", "stat_upp") %in% colnames(result_se)))
})

test_that("auto_svy_analysis bind parameter works correctly", {
  data <- create_auto_svy_test_data()
  design <- create_auto_svy_survey_design(data)

  # Test with bind = TRUE (default)
  result_bound <- auto_svy_analysis(design, bind = TRUE)
  expect_s3_class(result_bound, "data.table")

  # Test with bind = FALSE
  result_list <- auto_svy_analysis(design, bind = FALSE)
  expect_type(result_list, "list")
  expect_true(all(c("mean", "median", "prop") %in% names(result_list)))

  # Each element should be a data.table
  expect_s3_class(result_list$mean, "data.table")
  expect_s3_class(result_list$median, "data.table")
  expect_s3_class(result_list$prop, "data.table")
})

test_that("auto_svy_analysis handles edge cases", {
  # Test with only numeric variables
  data_numeric <- data.frame(
    numeric_var1 = rnorm(50),
    numeric_var2 = runif(50),
    design_weight = rep(1, 50)
  )
  design_numeric <- srvyr::as_survey_design(
    data_numeric,
    weights = design_weight
  )

  result_numeric <- auto_svy_analysis(design_numeric)
  expect_s3_class(result_numeric, "data.table")
  expect_true(all(result_numeric$stat_type %in% c("mean", "median")))

  # Test with only character variables
  data_char <- data.frame(
    char_var1 = sample(c("A", "B"), 50, replace = TRUE),
    char_var2 = sample(c("X", "Y", "Z"), 50, replace = TRUE),
    weight = rep(1, 50)
  )
  design_char <- srvyr::as_survey_design(data_char, weights = weight)

  result_char <- auto_svy_analysis(design_char, weight_var = "weight")
  expect_s3_class(result_char, "data.table")
  expect_true(all(result_char$stat_type == "proportion"))
})

test_that("auto_svy_analysis validates inputs correctly", {
  data <- create_auto_svy_test_data()
  design <- create_auto_svy_survey_design(data)

  # Test invalid design
  expect_error(
    auto_svy_analysis("not_a_design"),
    "Must inherit from class"
  )

  # Test invalid group variable
  expect_error(
    auto_svy_analysis(design, group = "nonexistent_var"),
    "Must be a subset"
  )

  # Test invalid level
  expect_error(
    auto_svy_analysis(design, level = 1.5),
    "Element 1 is not <= 1"
  )

  expect_error(
    auto_svy_analysis(design, level = -0.1),
    "Element 1 is not >= 0"
  )

  # Test invalid vartype
  expect_error(
    auto_svy_analysis(design, vartype = "invalid"),
    "should be one of"
  )
})

test_that("auto_svy_analysis produces consistent results", {
  data <- create_auto_svy_test_data()
  design <- create_auto_svy_survey_design(data)

  # Run the same analysis twice
  result1 <- auto_svy_analysis(design)
  result2 <- auto_svy_analysis(design)

  # Results should be identical
  expect_equal(result1, result2)

  # Check that means are reasonable for our test data
  numeric_var1_mean <- result1[stat_type == "mean" & var == "numeric_var1"]$stat
  expect_true(numeric_var1_mean > 40 && numeric_var1_mean < 60) # Should be around 50

  numeric_var2_mean <- result1[stat_type == "mean" & var == "numeric_var2"]$stat
  expect_true(numeric_var2_mean > 30 && numeric_var2_mean < 70) # Should be around 50
})

test_that("auto_svy_analysis excludes survey design variables", {
  # Create test data with survey design variables
  data <- data.frame(
    numeric_var = rnorm(50),
    char_var = sample(c("A", "B"), 50, replace = TRUE),
    my_weights = rep(1, 50),
    my_strata = sample(c("S1", "S2"), 50, replace = TRUE)
  )
  design <- srvyr::as_survey_design(
    data,
    weights = my_weights,
    strata = my_strata
  )

  result <- auto_svy_analysis(
    design,
    weight_var = "my_weights",
    stratum_var = "my_strata"
  )

  # Survey design variables should not be analyzed (they contain "weight" or "strat")
  expect_false("my_weights" %in% result$var)
  expect_false("my_strata" %in% result$var)
})

test_that("auto_svy_analysis handles mixed data types correctly", {
  # Create data with various data types
  data <- data.frame(
    numeric_var = rnorm(50),
    integer_var = sample(1:10, 50, replace = TRUE),
    character_var = sample(c("A", "B", "C"), 50, replace = TRUE),
    factor_var = factor(sample(c("Low", "Medium", "High"), 50, replace = TRUE)),
    logical_var = sample(c(TRUE, FALSE), 50, replace = TRUE),
    design_weight = rep(1, 50)
  )
  design <- srvyr::as_survey_design(data, weights = design_weight)

  result <- auto_svy_analysis(design)

  # Numeric and integer should get mean/median
  expect_true(any(result$stat_type == "mean" & result$var == "numeric_var"))
  expect_true(any(result$stat_type == "mean" & result$var == "integer_var"))

  # Character should get proportion
  expect_true(any(
    result$stat_type == "proportion" & result$var == "character_var"
  ))

  # Factor and logical variables depend on how srvyr treats them
  # We'll just check that the function doesn't error
  expect_s3_class(result, "data.table")
})

test_that("auto_svy_analysis survey design variable parameters work correctly", {
  # Test default parameter behavior
  data1 <- data.frame(
    numeric_var = rnorm(50),
    char_var = sample(c("A", "B"), 50, replace = TRUE),
    weights = rep(1, 50),
    strata = sample(c("S1", "S2"), 50, replace = TRUE)
  )
  design1 <- srvyr::as_survey_design(data1, weights = weights, strata = strata)

  result1 <- auto_svy_analysis(
    design1,
    weight_var = "weights",
    stratum_var = "strata"
  )

  # Should exclude default survey design variables
  expect_false("weights" %in% result1$var)
  expect_false("strata" %in% result1$var)

  # Test custom parameter behavior
  data2 <- data.frame(
    numeric_var = rnorm(50),
    char_var = sample(c("A", "B"), 50, replace = TRUE),
    my_custom_weight = rep(1, 50),
    my_custom_strata = sample(c("S1", "S2"), 50, replace = TRUE),
    weights = rnorm(50) # This should NOT be excluded with custom params
  )
  design2 <- srvyr::as_survey_design(
    data2,
    weights = my_custom_weight,
    strata = my_custom_strata
  )

  result2 <- auto_svy_analysis(
    design2,
    weight_var = "my_custom_weight",
    stratum_var = "my_custom_strata"
  )

  # Should exclude custom survey design variables but not default ones
  expect_false("my_custom_weight" %in% result2$var)
  expect_false("my_custom_strata" %in% result2$var)
  expect_true("weights" %in% result2$var) # This should be analyzed since it's not excluded
})

test_that("auto_svy_analysis handles multiple values for survey design parameters", {
  data <- data.frame(
    numeric_var = rnorm(50),
    char_var = sample(c("A", "B"), 50, replace = TRUE),
    weight1 = rep(1, 50),
    weight2 = rep(2, 50),
    cluster1 = sample(1:5, 50, replace = TRUE),
    cluster2 = sample(1:3, 50, replace = TRUE),
    normal_var = rnorm(50)
  )
  design <- srvyr::as_survey_design(data, weights = weight1)

  result <- auto_svy_analysis(
    design,
    weight_var = c("weight1", "weight2"),
    cluster_var = c("cluster1", "cluster2")
  )

  # Should exclude all specified survey design variables
  expect_false("weight1" %in% result$var)
  expect_false("weight2" %in% result$var)
  expect_false("cluster1" %in% result$var)
  expect_false("cluster2" %in% result$var)

  # Should include normal variables
  expect_true("numeric_var" %in% result$var)
  expect_true("char_var" %in% result$var)
  expect_true("normal_var" %in% result$var)
})

test_that("auto_svy_analysis survey design parameters can be empty", {
  data <- data.frame(
    numeric_var = rnorm(50),
    weights = rep(1, 50),
    strata = sample(c("S1", "S2"), 50, replace = TRUE)
  )
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Set empty parameters to include everything except srvyr internals
  result <- auto_svy_analysis(
    design,
    weight_var = character(0),
    stratum_var = character(0),
    cluster_var = character(0),
    id_var = character(0),
    fpc_var = character(0)
  )

  # Should include the survey design variables since we disabled filtering
  expect_true("weights" %in% result$var)
  expect_true("strata" %in% result$var)
})
