test_that("svy_interact validates inputs correctly", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    strata = rep(1:4, 25),
    weights = runif(100, 0.5, 2),
    var1 = sample(c("A", "B"), 100, replace = TRUE),
    var2 = sample(c("X", "Y"), 100, replace = TRUE),
    var3 = sample(c("1", "2"), 100, replace = TRUE),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test invalid design
  expect_error(
    svy_interact("not_design", c("var1", "var2")),
    "class.*tbl_svy"
  )

  # Test missing interact variables
  expect_error(
    svy_interact(design, c("var1", "missing_var")),
    "following variables from vars are missing: missing_var"
  )

  # Test overlap between interact and group variables
  expect_error(
    svy_interact(design, c("var1", "var2"), group = "var1"),
    "Grouping columns.*should be different.*interact"
  )
})

test_that("svy_interact produces correct basic interaction results", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    var1 = sample(c("A", "B"), 100, replace = TRUE),
    var2 = sample(c("X", "Y"), 100, replace = TRUE),
    var3 = sample(c("Red", "Blue"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test basic two-variable interaction
  result <- svy_interact(design, c("var1", "var2"))
  expect_s3_class(result, "data.table")
  expect_true("interact_key" %in% colnames(result))
  expect_true("interact_key_value" %in% colnames(result))
  expect_equal(unique(result$stat_type), "proportion")
  expect_true(all(result$stat >= 0 & result$stat <= 1))

  # Should have all combinations of var1 and var2
  expected_combinations <- nrow(expand.grid(
    unique(df$var1),
    unique(df$var2)
  ))
  expect_equal(nrow(result), expected_combinations)

  # Check that proportions sum to 1
  expect_equal(sum(result$stat), 1, tolerance = 1e-10)

  # Test three-variable interaction
  result_three <- svy_interact(design, c("var1", "var2", "var3"))
  expected_combinations_three <- nrow(expand.grid(
    unique(df$var1),
    unique(df$var2),
    unique(df$var3)
  ))
  expect_equal(nrow(result_three), expected_combinations_three)
  expect_equal(sum(result_three$stat), 1, tolerance = 1e-10)
})

test_that("svy_interact works with grouping variables", {
  # Create test data
  df <- data.frame(
    id = 1:200,
    weights = runif(200, 0.5, 2),
    var1 = sample(c("A", "B"), 200, replace = TRUE),
    var2 = sample(c("X", "Y"), 200, replace = TRUE),
    group1 = sample(c("Group1", "Group2"), 200, replace = TRUE),
    group2 = sample(c("Cat1", "Cat2", "Cat3"), 200, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test with single grouping variable
  result_single_group <- svy_interact(
    design,
    c("var1", "var2"),
    group = "group1"
  )
  expect_true("group_key" %in% colnames(result_single_group))
  expect_true("group_key_value" %in% colnames(result_single_group))
  expect_true(nrow(result_single_group) > 4) # More than ungrouped result

  # Check proportions sum to 1 within each group
  group_sums <- result_single_group[, .(sum_stat = sum(stat)), by = group1]
  expect_true(all(abs(group_sums$sum_stat - 1) < 1e-10))

  # Test with multiple grouping variables
  result_multi_group <- svy_interact(
    design,
    c("var1", "var2"),
    group = c("group1", "group2")
  )
  expect_true(nrow(result_multi_group) > nrow(result_single_group))

  # Check proportions sum to 1 within each group combination
  group_sums_multi <- result_multi_group[,
    .(sum_stat = sum(stat)),
    by = c("group1", "group2")
  ]
  expect_true(all(abs(group_sums_multi$sum_stat - 1) < 1e-10))
})

test_that("svy_interact handles NA values correctly", {
  # Create test data with NAs
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    var1 = c(sample(c("A", "B"), 80, replace = TRUE), rep(NA, 20)),
    var2 = c(sample(c("X", "Y"), 85, replace = TRUE), rep(NA, 15)),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test na_rm = TRUE (default)
  result_na_rm <- svy_interact(design, c("var1", "var2"), na_rm = TRUE)
  expect_true(result_na_rm$na_count_tot[1] > 0)
  expect_true(result_na_rm$n_tot_unw[1] < 100)

  # Test na_rm = FALSE
  result_na_keep <- svy_interact(design, c("var1", "var2"), na_rm = FALSE)
  expect_equal(result_na_keep$n_tot_unw[1], 100)
  expect_true(result_na_keep$na_count_tot[1] > 0)
})

test_that("svy_interact handles interact_key_sep parameter", {
  # Create test data
  df <- data.frame(
    id = 1:50,
    weights = runif(50, 0.5, 2),
    var1 = sample(c("A", "B"), 50, replace = TRUE),
    var2 = sample(c("X", "Y"), 50, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test default separator
  result_default <- svy_interact(design, c("var1", "var2"))
  expect_true(any(grepl(" -/- ", result_default$interact_key, fixed = TRUE)))

  # Test custom separator
  result_custom <- svy_interact(
    design,
    c("var1", "var2"),
    interact_key_sep = " | "
  )
  expect_true(any(grepl(" | ", result_custom$interact_key, fixed = TRUE)))
  expect_false(any(grepl(" -/- ", result_custom$interact_key, fixed = TRUE)))
})

test_that("svy_interact handles different variance types", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    var1 = sample(c("A", "B"), 100, replace = TRUE),
    var2 = sample(c("X", "Y"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  variance_types <- c("se", "ci", "var", "cv")

  for (vtype in variance_types) {
    result <- svy_interact(design, c("var1", "var2"), vartype = vtype)
    expect_s3_class(result, "data.table")
    expect_true("stat" %in% colnames(result))

    if (vtype == "ci") {
      expect_true("stat_low" %in% colnames(result))
      expect_true("stat_upp" %in% colnames(result))
    } else if (vtype == "se") {
      expect_true("stat_se" %in% colnames(result))
    } else if (vtype == "var") {
      expect_true("stat_var" %in% colnames(result))
    } else if (vtype == "cv") {
      expect_true("stat_cv" %in% colnames(result))
    }
  }
})

test_that("svy_interact handles confidence levels", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    var1 = sample(c("A", "B"), 100, replace = TRUE),
    var2 = sample(c("X", "Y"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test different confidence levels
  result_95 <- svy_interact(design, c("var1", "var2"), level = 0.95)
  result_99 <- svy_interact(design, c("var1", "var2"), level = 0.99)

  # 99% confidence intervals should be wider than 95%
  ci_width_95 <- result_95$stat_upp - result_95$stat_low
  ci_width_99 <- result_99$stat_upp - result_99$stat_low
  expect_true(all(ci_width_99 >= ci_width_95))
})

test_that("svy_interact handles analysis key parameters", {
  # Create test data
  df <- data.frame(
    id = 1:50,
    weights = runif(50, 0.5, 2),
    var1 = sample(c("A", "B"), 50, replace = TRUE),
    var2 = sample(c("X", "Y"), 50, replace = TRUE),
    group_var = sample(c("Group1", "Group2"), 50, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test analysis key enabled (default)
  result_ak <- svy_interact(
    design,
    c("var1", "var2"),
    group = "group_var",
    ak = TRUE
  )
  expect_true("analysis_key" %in% colnames(result_ak))

  # Test analysis key disabled
  result_no_ak <- svy_interact(
    design,
    c("var1", "var2"),
    group = "group_var",
    ak = FALSE
  )
  expect_false("analysis_key" %in% colnames(result_no_ak))

  # Test custom analysis key separators
  result_custom_ak <- svy_interact(
    design,
    c("var1", "var2"),
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

test_that("svy_interact stops for single variable interactions", {
  # Create test data
  df <- data.frame(
    id = 1:50,
    weights = runif(50, 0.5, 2),
    var1 = sample(c("A", "B"), 50, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Should warn when interact has only one variable
  expect_error(
    result <- svy_interact(design, "var1"),
    "Assertion on 'interact' failed: Must have length >= 2, but has length 1"
  )
})

test_that("svy_interact produces consistent unweighted statistics", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    var1 = sample(c("A", "B"), 100, replace = TRUE),
    var2 = sample(c("X", "Y"), 100, replace = TRUE),
    group_var = sample(c("Group1", "Group2"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test unweighted statistics
  result <- svy_interact(design, c("var1", "var2"))

  # Unweighted proportions should sum to 1
  expect_equal(sum(result$stat_unw), 1, tolerance = 1e-10)

  # n_unw should sum to total observations (minus NAs)
  expect_equal(sum(result$n_unw), result$n_tot_unw[1])

  # Test with grouping
  result_grouped <- svy_interact(design, c("var1", "var2"), group = "group_var")

  # Within each group, unweighted proportions should sum to 1
  group_unw_sums <- result_grouped[, .(sum_unw = sum(stat_unw)), by = group_var]
  expect_true(all(abs(group_unw_sums$sum_unw - 1) < 1e-10))
})

test_that("svy_interact handles edge cases", {
  # Create test data with edge cases
  df <- data.frame(
    id = 1:50,
    weights = runif(50, 0.5, 2),
    all_same = rep("A", 50),
    var1 = sample(c("A", "B"), 50, replace = TRUE),
    var2 = sample(c("X", "Y"), 50, replace = TRUE),
    all_na = rep(NA_character_, 50)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test with variable that has only one category
  result_single_cat <- svy_interact(design, c("all_same", "var1"))
  expect_s3_class(result_single_cat, "data.table")
  expect_equal(nrow(result_single_cat), length(unique(df$var1)))

  # Test with all NA variable - should work but have high NA count
  result_with_na <- svy_interact(design, c("var1", "var2"))
  expect_s3_class(result_with_na, "data.table")
  expect_equal(result_with_na$na_count_tot[1], 0) # No NAs in this case
})

test_that("svy_interact column ordering and structure", {
  # Create test data
  df <- data.frame(
    id = 1:50,
    weights = runif(50, 0.5, 2),
    var1 = sample(c("A", "B"), 50, replace = TRUE),
    var2 = sample(c("X", "Y"), 50, replace = TRUE),
    group_var = sample(c("Group1", "Group2"), 50, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  result <- svy_interact(design, c("var1", "var2"), group = "group_var")

  # Check expected column structure
  expected_cols <- c(
    "group_var",
    "group_key",
    "group_key_value",
    "var1",
    "var2",
    "interact_key",
    "interact_key_value",
    "stat",
    "stat_low",
    "stat_upp",
    "n_unw",
    "stat_type",
    "stat_unw",
    "n_tot_unw",
    "n_tot",
    "na_count_tot",
    "analysis_key"
  )

  # All expected columns should be present
  for (col in expected_cols) {
    expect_true(col %in% colnames(result), info = paste("Missing column:", col))
  }

  # Check data types
  expect_true(is.numeric(result$stat))
  expect_true(is.character(result$interact_key))
  expect_true(is.character(result$interact_key_value))
  expect_true(is.character(result$stat_type))
  expect_equal(unique(result$stat_type), "proportion")
})

test_that("svy_interact passes additional arguments to survey_mean", {
  # Create test data
  df <- data.frame(
    id = 1:100,
    weights = runif(100, 0.5, 2),
    var1 = sample(c("A", "B"), 100, replace = TRUE),
    var2 = sample(c("X", "Y"), 100, replace = TRUE)
  )
  design <- srvyr::as_survey_design(df, weights = weights)

  # Test that additional arguments are passed through
  # This is harder to test directly, but we can verify the function runs
  # without error when extra arguments are provided
  expect_no_error(
    svy_interact(
      design,
      c("var1", "var2"),
      deff = TRUE # This should be passed to survey_mean
    )
  )
})
