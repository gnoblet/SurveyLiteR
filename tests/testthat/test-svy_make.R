test_that("svy_make works correctly for means", {
  # Create sample data
  data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(10, 20, 30, 40, 50),
    group = c("A", "A", "B", "B", "B"),
    weights = c(1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make directly for means
  result_mean <- svy_make(
    design = design,
    vars = "x",
    stat_type = "mean",
    survey_fn = srvyr::survey_mean,
    unweighted_fn = function(x) mean(x),
    group_by_var = FALSE,
    add_var_value = TRUE
  )

  expect_s3_class(result_mean, "data.frame")
  expect_true("stat" %in% colnames(result_mean))
  expect_true("stat_unw" %in% colnames(result_mean))
  expect_true("stat_type" %in% colnames(result_mean))
  expect_equal(result_mean$stat_type[1], "mean")
  expect_equal(result_mean$var[1], "x")
  expect_true(is.na(result_mean$var_value[1]))
})

test_that("svy_make works correctly for proportions", {
  # Create sample data with categorical variable
  data <- data.frame(
    cat_var = factor(c("A", "B", "A", "B", "A")),
    group = c("G1", "G1", "G2", "G2", "G1"),
    weights = c(1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make directly for proportions
  result_prop <- svy_make(
    design = design,
    vars = "cat_var",
    stat_type = "proportion",
    survey_fn = function(var, ...) srvyr::survey_prop(...),
    unweighted_fn = function(x) 1,
    group_by_var = TRUE,
    add_var_value = FALSE
  )

  expect_s3_class(result_prop, "data.frame")
  expect_true("stat" %in% colnames(result_prop))
  expect_true("stat_unw" %in% colnames(result_prop))
  expect_true("stat_type" %in% colnames(result_prop))
  expect_equal(unique(result_prop$stat_type), "proportion")
  expect_equal(unique(result_prop$var), "cat_var")
  expect_true(all(!is.na(result_prop$var_value)))
  expect_true(all(result_prop$var_value %in% c("A", "B")))
})

test_that("svy_make works correctly for medians", {
  # Create sample data
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    weights = c(1, 1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make directly for medians
  result_median <- svy_make(
    design = design,
    vars = "x",
    stat_type = "median",
    survey_fn = srvyr::survey_median,
    unweighted_fn = function(x) median(x),
    group_by_var = FALSE,
    add_var_value = TRUE
  )

  expect_s3_class(result_median, "data.frame")
  expect_true("stat" %in% colnames(result_median))
  expect_true("stat_unw" %in% colnames(result_median))
  expect_true("stat_type" %in% colnames(result_median))
  expect_equal(result_median$stat_type[1], "median")
  expect_equal(result_median$var[1], "x")
  expect_true(is.na(result_median$var_value[1]))
})

test_that("svy_make_ratio function works correctly", {
  # Create sample data
  data <- data.frame(
    num = c(10, 20, 30, 40, 50),
    denom = c(2, 4, 6, 8, 10),
    weights = c(1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make_ratio directly
  result_ratio <- svy_make_ratio(
    design = design,
    nums = "num",
    denoms = "denom"
  )

  expect_s3_class(result_ratio, "data.frame")
  expect_true("stat" %in% colnames(result_ratio))
  expect_true("stat_unw" %in% colnames(result_ratio))
  expect_true("stat_type" %in% colnames(result_ratio))
  expect_equal(result_ratio$stat_type[1], "ratio")
  expect_true(grepl("num.*denom", result_ratio$var[1]))
})

test_that("svy_make handles NA values correctly", {
  # Create sample data with NAs
  data <- data.frame(
    x = c(1, 2, NA, 4, 5),
    weights = c(1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make with na_rm = TRUE
  result_na_rm <- svy_make(
    design = design,
    vars = "x",
    na_rm = TRUE,
    stat_type = "mean",
    survey_fn = srvyr::survey_mean,
    unweighted_fn = function(x) mean(x),
    group_by_var = FALSE
  )

  expect_equal(result_na_rm$na_count_tot[1], 1)
  expect_equal(result_na_rm$n_tot[1], 5)

  # Test svy_make with na_rm = FALSE
  result_na_keep <- svy_make(
    design = design,
    vars = "x",
    na_rm = FALSE,
    stat_type = "mean",
    survey_fn = srvyr::survey_mean,
    unweighted_fn = function(x) mean(x),
    group_by_var = FALSE
  )

  expect_equal(result_na_keep$na_count_tot[1], 1)
  expect_equal(result_na_keep$n_tot[1], 5)
})

test_that("svy_make handles all NA variables correctly", {
  # Create sample data with all NAs
  data <- data.frame(
    x = c(NA, NA, NA, NA, NA),
    weights = c(1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make with all NAs - should return empty data frame with warning
  expect_warning(
    result_all_na <- svy_make(
      design = design,
      vars = "x",
      stat_type = "mean",
      survey_fn = srvyr::survey_mean,
      unweighted_fn = function(x) mean(x),
      group_by_var = FALSE
    ),
    "only contains missing values"
  )
  expect_equal(nrow(result_all_na), 0)
})

test_that("svy_make works with grouping", {
  # Create sample data with groups
  data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    group = c("A", "A", "B", "B", "C", "C"),
    weights = c(1, 1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make with grouping
  result_grouped <- svy_make(
    design = design,
    vars = "x",
    group = "group",
    stat_type = "mean",
    survey_fn = srvyr::survey_mean,
    unweighted_fn = function(x) mean(x),
    group_by_var = FALSE
  )

  expect_s3_class(result_grouped, "data.frame")
  expect_equal(nrow(result_grouped), 3) # Should have 3 rows for groups A, B, C
  expect_true("group_key" %in% colnames(result_grouped))
  expect_true("group_key_value" %in% colnames(result_grouped))
  expect_equal(unique(result_grouped$group_key), "group")
  expect_setequal(result_grouped$group_key_value, c("A", "B", "C"))
})

test_that("svy_make handles multiple variables", {
  # Create sample data
  data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(10, 20, 30, 40, 50),
    weights = c(1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make with multiple variables
  result_multiple <- svy_make(
    design = design,
    vars = c("x", "y"),
    stat_type = "mean",
    survey_fn = srvyr::survey_mean,
    unweighted_fn = function(x) mean(x),
    group_by_var = FALSE
  )

  expect_s3_class(result_multiple, "data.frame")
  expect_equal(nrow(result_multiple), 2) # Should have 2 rows for x and y
  expect_setequal(result_multiple$var, c("x", "y"))
  expect_true(all(result_multiple$stat_type == "mean"))
})

test_that("svy_make handles custom column names", {
  # Create sample data
  data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    weights = c(1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make with custom var_column_name
  result_custom <- svy_make(
    design = design,
    vars = "x",
    stat_type = "mean",
    survey_fn = srvyr::survey_mean,
    unweighted_fn = function(x) mean(x),
    group_by_var = FALSE,
    var_column_name = "variable"
  )

  expect_s3_class(result_custom, "data.frame")
  expect_true("variable" %in% colnames(result_custom))
  expect_equal(result_custom$variable[1], "x")
})

test_that("svy_make handles different variance types", {
  # Create sample data
  data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    weights = c(1, 1, 1, 1, 1)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights)

  # Test svy_make with different vartype values
  for (vartype in c("ci", "se", "var")) {
    result <- svy_make(
      design = design,
      vars = "x",
      vartype = vartype,
      stat_type = "mean",
      survey_fn = srvyr::survey_mean,
      unweighted_fn = function(x) mean(x),
      group_by_var = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true("stat" %in% colnames(result))

    # Check that appropriate columns exist based on vartype
    if (vartype == "ci") {
      expect_true("stat_low" %in% colnames(result))
      expect_true("stat_upp" %in% colnames(result))
    } else if (vartype == "se") {
      expect_true("stat_se" %in% colnames(result))
    } else if (vartype == "var") {
      expect_true("stat_var" %in% colnames(result))
    }
  }
})
