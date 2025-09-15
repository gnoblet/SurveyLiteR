test_that("svy_analysis validates inputs correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    strata = rep(1:4, 25),
    weights = runif(100, 0.5, 2),
    numeric_var = rnorm(100),
    numeric_var2 = rnorm(100),
    char_var = sample(c("A", "B", "C"), 100, replace = TRUE),
    char_var2 = sample(c("X", "Y"), 100, replace = TRUE),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test invalid analysis type
  expect_error(
    svy_analysis(design, "invalid", "numeric_var"),
    "Must be a subset"
  )

  # Test missing vars
  expect_error(
    svy_analysis(design, "mean", character(0)),
    "length.*1"
  )

  # Test invalid design
  expect_error(
    svy_analysis("not_design", "mean", "numeric_var"),
    "class.*tbl_svy"
  )
})

test_that("svy_analysis mean analysis works correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    numeric_var = rnorm(100, 10, 2),
    numeric_var2 = rnorm(100, 20, 3),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test single variable mean
  result <- svy_analysis(design, "mean", "numeric_var")
  expect_s3_class(result, "data.table")
  expect_true("stat" %in% colnames(result))
  expect_equal(result$stat_type[1], "mean")
  expect_equal(result$var[1], "numeric_var")

  # Test multiple variables
  result_multi <- svy_analysis(design, "mean", c("numeric_var", "numeric_var2"))
  expect_equal(nrow(result_multi), 2)
  expect_equal(sort(result_multi$var), c("numeric_var", "numeric_var2"))

  # Test with grouping
  result_grouped <- svy_analysis(
    design,
    "mean",
    "numeric_var",
    group = "group_var"
  )
  expect_true(nrow(result_grouped) >= 2) # At least one row per group
  expect_true("group_key" %in% colnames(result_grouped))

  # Compare with direct svy_mean call
  direct_result <- svy_mean(design, "numeric_var")
  expect_equal(result$stat, direct_result$stat, tolerance = 1e-10)
})

test_that("svy_analysis median analysis works correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    numeric_var = rnorm(100, 15, 3),
    group_var = sample(c("A", "B"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test median analysis
  result <- svy_analysis(design, "median", "numeric_var")
  expect_s3_class(result, "data.table")
  expect_equal(result$stat_type[1], "median")
  expect_equal(result$var[1], "numeric_var")

  # Test with grouping
  result_grouped <- svy_analysis(
    design,
    "median",
    "numeric_var",
    group = "group_var"
  )
  expect_true(nrow(result_grouped) >= 2)

  # Compare with direct svy_median call
  direct_result <- svy_median(design, "numeric_var")
  expect_equal(result$stat, direct_result$stat, tolerance = 1e-10)
})

test_that("svy_analysis proportion analysis works correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    char_var = sample(c("Yes", "No", "Maybe"), 100, replace = TRUE),
    char_var2 = sample(c("X", "Y"), 100, replace = TRUE),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test proportion analysis
  result <- svy_analysis(design, "proportion", "char_var", ak = FALSE)
  expect_s3_class(result, "data.table")
  expect_equal(unique(result$stat_type), "proportion")
  expect_equal(unique(result$var), "char_var")
  expect_true("var_value" %in% colnames(result))

  # Test with grouping
  result_grouped <- svy_analysis(
    design,
    "proportion",
    "char_var",
    group = "group_var",
    ak = FALSE
  )
  expect_true(nrow(result_grouped) > nrow(result))

  # Compare with direct svy_proportion call
  direct_result <- svy_proportion(design, "char_var", ak = FALSE)
  expect_equal(
    result[order(var_value)]$stat,
    direct_result[order(var_value)]$stat,
    tolerance = 1e-10
  )
})

test_that("svy_analysis ratio analysis works correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    numerator = rpois(100, 20),
    denominator = rpois(100, 50),
    num2 = rpois(100, 15),
    denom2 = rpois(100, 30),
    group_var = sample(c("A", "B"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test single ratio
  result <- svy_analysis(
    design,
    "ratio",
    c(numerator = "denominator"),
    ak = FALSE
  )
  expect_s3_class(result, "data.table")
  expect_equal(result$stat_type[1], "ratio")
  expect_true(grepl("numerator.*denominator", result$var[1]))

  # Test multiple ratios
  result_multi <- svy_analysis(
    design,
    "ratio",
    c(numerator = "denominator", num2 = "denom2"),
    ak = FALSE
  )
  expect_equal(nrow(result_multi), 2)

  # Test with grouping
  result_grouped <- svy_analysis(
    design,
    "ratio",
    c(numerator = "denominator"),
    group = "group_var",
    ak = FALSE
  )
  expect_true(nrow(result_grouped) >= 2)

  # Compare with direct svy_ratio call
  direct_result <- svy_ratio(design, "numerator", "denominator", ak = FALSE)
  expect_equal(result$stat, direct_result$stat, tolerance = 1e-10)
})

test_that("svy_analysis interact analysis works correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    var1 = sample(c("A", "B"), 100, replace = TRUE),
    var2 = sample(c("X", "Y"), 100, replace = TRUE),
    var3 = sample(c("1", "2"), 100, replace = TRUE),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test interact analysis
  result <- svy_analysis(design, "interact", c("var1", "var2"))
  expect_s3_class(result, "data.table")
  expect_equal(unique(result$stat_type), "proportion")
  expect_true("interact_key" %in% colnames(result))
  expect_true("interact_key_value" %in% colnames(result))

  # Test with grouping
  result_grouped <- svy_analysis(
    design,
    "interact",
    c("var1", "var2"),
    group = "group_var"
  )
  expect_true(nrow(result_grouped) > nrow(result))

  # Compare with direct svy_interact call
  direct_result <- svy_interact(design, c("var1", "var2"))
  expect_equal(nrow(result), nrow(direct_result))
})

test_that("svy_analysis quantile analysis works correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    numeric_var = rnorm(100, 25, 5),
    group_var = sample(c("A", "B"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)
  result <- svy_analysis(design, "quantile", "numeric_var")

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$stat_type), "quantile")
  expect_equal(result$var[1], "numeric_var")
  expect_equal(nrow(result), 1)
  expect_true(unique(
    c("stat_q25", "stat_q50", "stat_q75") %in% colnames(result)
  ))
})

test_that("svy_analysis handles parameters correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    numeric_var = c(rnorm(90, 10, 2), rep(NA, 10)),
    char_var = c(sample(c("A", "B"), 90, replace = TRUE), rep(NA, 10)),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test na_rm parameter
  result_na_rm <- svy_analysis(design, "mean", "numeric_var", na_rm = TRUE)
  result_na_keep <- svy_analysis(design, "mean", "numeric_var", na_rm = FALSE)
  expect_true(result_na_rm$n_unw[1] < result_na_keep$n_unw[1])

  # Test vartype parameter
  result_ci <- svy_analysis(design, "mean", "numeric_var", vartype = "ci")
  result_se <- svy_analysis(design, "mean", "numeric_var", vartype = "se")
  expect_true("stat_low" %in% colnames(result_ci))
  expect_false("stat_low" %in% colnames(result_se))
  expect_true("stat_se" %in% colnames(result_se))

  # Test level parameter
  result_95 <- svy_analysis(design, "mean", "numeric_var", level = 0.95)
  result_99 <- svy_analysis(design, "mean", "numeric_var", level = 0.99)
  ci_width_95 <- result_95$stat_upp[1] - result_95$stat_low[1]
  ci_width_99 <- result_99$stat_upp[1] - result_99$stat_low[1]
  expect_true(ci_width_99 > ci_width_95)

  # Test group_key_sep parameter - just verify it runs and has grouping columns
  result_default_sep <- svy_analysis(
    design,
    "mean",
    "numeric_var",
    group = "group_var"
  )
  result_custom_sep <- svy_analysis(
    design,
    "mean",
    "numeric_var",
    group = "group_var",
    group_key_sep = " | "
  )
  expect_true("group_key" %in% colnames(result_default_sep))
  expect_true("group_key" %in% colnames(result_custom_sep))
  expect_true(nrow(result_default_sep) >= 2) # Should have multiple groups
  expect_true(nrow(result_custom_sep) >= 2)
})

test_that("svy_analysis handles analysis key parameters", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    numeric_var = rnorm(100, 10, 2),
    char_var = sample(c("A", "B"), 100, replace = TRUE),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test analysis key enabled
  result_ak <- svy_analysis(
    design,
    "mean",
    "numeric_var",
    group = "group_var",
    ak = TRUE
  )
  expect_true("analysis_key" %in% colnames(result_ak))

  # Test analysis key disabled
  result_no_ak <- svy_analysis(
    design,
    "mean",
    "numeric_var",
    group = "group_var",
    ak = FALSE
  )
  expect_false("analysis_key" %in% colnames(result_no_ak))

  # Test custom analysis key separators
  result_custom_ak <- svy_analysis(
    design,
    "proportion",
    "char_var",
    group = "group_var",
    ak = TRUE,
    ak_overall_sep = " @@ ",
    ak_main_sep = " -- ",
    ak_var_to_value_sep = " %% "
  )
  if ("analysis_key" %in% colnames(result_custom_ak)) {
    expect_true(any(grepl(" @@ ", result_custom_ak$analysis_key, fixed = TRUE)))
  }
})

test_that("svy_analysis handles edge cases", {
  # Create test data with edge cases
  df <- data.frame(
    id = 1:50,
    weights = runif(50, 0.5, 2),
    all_na = rep(NA_real_, 50),
    single_value = rep(5, 50),
    char_single = rep("A", 50),
    group_var = sample(c("Group1", "Group2"), 50, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test with all NA values - should warn and return empty
  expect_warning(
    result_all_na <- svy_analysis(design, "mean", "all_na"),
    "only contains missing values"
  )
  expect_equal(nrow(result_all_na), 0)

  # Test with single unique value
  result_single <- svy_analysis(design, "mean", "single_value")
  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$stat[1], 5)

  # Test proportion with single category
  result_char_single <- svy_analysis(design, "proportion", "char_single")
  expect_equal(nrow(result_char_single), 1)
  expect_equal(result_char_single$stat[1], 1, tolerance = 1e-10)
})

test_that("svy_analysis passes additional arguments correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    numeric_var = rnorm(100, 10, 2),
    group_var = sample(c("A", "B"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test that additional arguments are passed through
  # This is harder to test directly, but we can verify the function runs
  # without error when extra arguments are provided
  expect_no_error(
    svy_analysis(
      design,
      "mean",
      "numeric_var",
      deff = TRUE # This should be passed to svy_mean
    )
  )
})

test_that("svy_analysis handles different variance types", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    numeric_var = rnorm(100, 15, 3),
    char_var = sample(c("X", "Y", "Z"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  variance_types <- c("se", "ci", "var", "cv")

  for (vtype in variance_types) {
    # Test mean
    result_mean <- svy_analysis(
      design,
      "mean",
      "numeric_var",
      vartype = vtype
    )
    expect_s3_class(result_mean, "data.table")

    # Test proportion
    result_prop <- svy_analysis(
      design,
      "proportion",
      "char_var",
      vartype = vtype
    )
    expect_s3_class(result_prop, "data.table")
  }
})

test_that("svy_analysis maintains consistency with individual functions", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    numeric_var = rnorm(100, 20, 4),
    char_var = sample(c("A", "B", "C"), 100, replace = TRUE),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Compare svy_analysis results with individual function calls

  # Mean comparison
  analysis_mean <- svy_analysis(
    design,
    "mean",
    "numeric_var",
    group = "group_var"
  )
  direct_mean <- svy_mean(design, "numeric_var", group = "group_var")
  expect_equal(analysis_mean$stat, direct_mean$stat, tolerance = 1e-10)

  # Median comparison
  analysis_median <- svy_analysis(
    design,
    "median",
    "numeric_var",
    group = "group_var"
  )
  direct_median <- svy_median(design, "numeric_var", group = "group_var")
  expect_equal(analysis_median$stat, direct_median$stat, tolerance = 1e-10)

  # Proportion comparison
  analysis_prop <- svy_analysis(
    design,
    "proportion",
    "char_var",
    group = "group_var"
  )
  direct_prop <- svy_proportion(design, "char_var", group = "group_var")

  # Sort both results for comparison
  analysis_prop_sorted <- analysis_prop[order(group_key, var_value)]
  direct_prop_sorted <- direct_prop[order(group_key, var_value)]
  expect_equal(
    analysis_prop_sorted$stat,
    direct_prop_sorted$stat,
    tolerance = 1e-10
  )
})
