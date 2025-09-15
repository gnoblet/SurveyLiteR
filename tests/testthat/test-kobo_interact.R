# Tests for kobo_interact function

# Helper function to create test data for kobo_interact
create_kobo_interact_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create sample survey data with select_one variables for interaction
  data <- data.frame(
    var1 = sample(c("A", "B", "C"), n, replace = TRUE),
    var2 = sample(c("X", "Y"), n, replace = TRUE),
    var3 = sample(c("High", "Medium", "Low"), n, replace = TRUE),
    var4 = sample(c("Yes", "No"), n, replace = TRUE),
    # Grouping variables
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    status = sample(c("Active", "Inactive"), n, replace = TRUE),
    # Survey design variables
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create survey sheet
  survey <- data.table::data.table(
    type = c("select_one", "select_one", "select_one", "select_one"),
    list_name = c("list1", "list2", "list3", "list4"),
    name = c("var1", "var2", "var3", "var4"),
    label = c("Variable 1", "Variable 2", "Variable 3", "Variable 4")
  )

  # Create choices sheet
  choices <- data.table::data.table(
    list_name = c(
      "list1",
      "list1",
      "list1",
      "list2",
      "list2",
      "list3",
      "list3",
      "list3",
      "list4",
      "list4"
    ),
    name = c(
      "A",
      "B",
      "C",
      "X",
      "Y",
      "High",
      "Medium",
      "Low",
      "Yes",
      "No"
    ),
    label = c(
      "Option A",
      "Option B",
      "Option C",
      "Choice X",
      "Choice Y",
      "High Level",
      "Medium Level",
      "Low Level",
      "Yes Response",
      "No Response"
    )
  )

  list(
    data = data,
    design = design,
    survey = survey,
    choices = choices
  )
}

# Helper function to create minimal test data
create_minimal_interact_test_data <- function() {
  set.seed(456)
  n <- 50

  data <- data.frame(
    var_a = sample(c("Type1", "Type2"), n, replace = TRUE),
    var_b = sample(c("Cat1", "Cat2"), n, replace = TRUE),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("select_one", "select_one"),
    list_name = c("types", "categories"),
    name = c("var_a", "var_b"),
    label = c("Variable A", "Variable B")
  )

  choices <- data.table::data.table(
    list_name = c("types", "types", "categories", "categories"),
    name = c("Type1", "Type2", "Cat1", "Cat2"),
    label = c("First Type", "Second Type", "First Category", "Second Category")
  )

  list(
    data = data,
    design = design,
    survey = survey,
    choices = choices
  )
}

# Test basic functionality
test_that("kobo_interact works with two variables", {
  test_data <- create_kobo_interact_test_data()

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var1", "var2"),
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "interact")
  expect_equal(unique(result$stat_type), "proportion")

  # Check required columns exist
  expect_true("interact_key" %in% colnames(result))
  expect_true("interact_key_value" %in% colnames(result))
  expect_true("stat" %in% colnames(result))
  expect_true("stat_low" %in% colnames(result))
  expect_true("stat_upp" %in% colnames(result))
  expect_true("var_label" %in% colnames(result))

  # Check that interact_key contains both variables
  expect_true(all(grepl("var1.*var2", result$interact_key)))
  expect_true(all(grepl("Variable 1.*Variable 2", result$var_label)))

  # Check that we have the expected number of combinations
  # var1 has 3 levels (A, B, C) and var2 has 2 levels (X, Y) = 6 combinations max
  expect_true(nrow(result) <= 6)
  expect_true(nrow(result) > 0)

  # Check that proportions sum to approximately 1
  expect_equal(sum(result$stat), 1, tolerance = 1e-10)
})

test_that("kobo_interact works with three variables", {
  test_data <- create_kobo_interact_test_data()

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var1", "var2", "var4"),
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "interact")

  # Check that interact_key contains all three variables
  expect_true(all(grepl("var1.*var2.*var4", result$interact_key)))

  # var1: 3 levels, var2: 2 levels, var4: 2 levels = max 12 combinations
  expect_true(nrow(result) <= 12)
  expect_true(nrow(result) > 0)

  # Proportions should sum to 1
  expect_equal(sum(result$stat), 1, tolerance = 1e-10)
})

test_that("kobo_interact works with grouping", {
  test_data <- create_kobo_interact_test_data()

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var1", "var2"),
    group = "region",
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))

  # Should have results for each region
  unique_regions <- unique(test_data$data$region)
  expect_setequal(unique(result$group_key_value), unique_regions)

  # Proportions should sum to 1 within each group
  group_sums <- result[, .(total = sum(stat)), by = group_key_value]
  expect_true(all(abs(group_sums$total - 1) < 1e-10))
})

test_that("kobo_interact works with multiple grouping variables", {
  test_data <- create_kobo_interact_test_data()

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var1", "var2"),
    group = c("region", "status"),
    survey = test_data$survey
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))

  # Check that group_key contains both variables
  expect_true(all(grepl("region.*status", result$group_key)))

  # Should have results for each combination of region and status
  unique_combos <- unique(paste(
    test_data$data$region,
    test_data$data$status,
    sep = " -/- "
  ))
  result_combos <- unique(result$group_key_value)
  expect_true(length(intersect(unique_combos, result_combos)) > 0)
})

test_that("kobo_interact works with custom separators", {
  test_data <- create_minimal_interact_test_data()

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    interact_key_sep = " | ",
    survey = test_data$survey
  )

  expect_true(all(grepl("var_a.*\\|.*var_b", result$interact_key)))
  expect_true(all(grepl("Variable A.*\\|.*Variable B", result$var_label)))
  expect_true(all(grepl(".*\\|.*", result$interact_key_value)))
})

test_that("kobo_interact works without variable labels", {
  test_data <- create_minimal_interact_test_data()

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    survey = test_data$survey,
    label_survey = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_false("var_label" %in% colnames(result))
  expect_true("interact_key" %in% colnames(result))
  expect_true("interact_key_value" %in% colnames(result))
})

test_that("kobo_interact works with choice labels", {
  test_data <- create_minimal_interact_test_data()

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = TRUE
  )

  expect_s3_class(result, "data.table")
  expect_true("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result))

  # Check that choice labels are correctly applied
  expect_true(any(grepl("First Type", result$var_value_label)))
  expect_true(any(grepl("Second Type", result$var_value_label)))
  expect_true(any(grepl("First Category", result$var_value_label)))
  expect_true(any(grepl("Second Category", result$var_value_label)))
})

test_that("kobo_interact works with only choice labels (no variable labels)", {
  test_data <- create_minimal_interact_test_data()

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_false("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result))

  # Check that choice labels are present
  expect_true(any(grepl("First Type", result$var_value_label)))
  expect_true(any(grepl("First Category", result$var_value_label)))
})

test_that("kobo_interact handles NA values correctly", {
  test_data <- create_minimal_interact_test_data()

  # Add some NA values
  test_data$data$var_a[1:5] <- NA
  test_data$data$var_b[6:10] <- NA
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # With na_rm = TRUE (default)
  result_rm <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    survey = test_data$survey,
    na_rm = TRUE
  )

  # With na_rm = FALSE
  result_keep <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    survey = test_data$survey,
    na_rm = FALSE
  )

  expect_s3_class(result_rm, "data.table")
  expect_s3_class(result_keep, "data.table")

  # Results should be different due to NA handling
  expect_false(identical(result_rm$stat, result_keep$stat))
})

test_that("kobo_interact handles different vartype options", {
  test_data <- create_minimal_interact_test_data()

  # Test different vartype options
  vartypes <- c("ci", "se", "var", "cv")

  for (vt in vartypes) {
    result <- kobo_interact(
      design = test_data$design,
      vars = c("var_a", "var_b"),
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

test_that("kobo_interact handles different confidence levels", {
  test_data <- create_minimal_interact_test_data()

  result_95 <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    survey = test_data$survey,
    level = 0.95
  )

  result_99 <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    survey = test_data$survey,
    level = 0.99
  )

  # 99% CI should be wider than 95% CI for at least some rows
  ci_width_95 <- result_95$stat_upp - result_95$stat_low
  ci_width_99 <- result_99$stat_upp - result_99$stat_low

  expect_true(any(ci_width_99 >= ci_width_95))
})

# Test statistical accuracy by comparing with direct srvyr calls
test_that("kobo_interact produces statistically accurate results", {
  test_data <- create_minimal_interact_test_data()

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    survey = test_data$survey,
    label_survey = FALSE
  )

  # Calculate the same interaction directly with srvyr
  direct_result <- test_data$design %>%
    srvyr::group_by(srvyr::interact(var_a, var_b)) %>%
    srvyr::summarise(
      stat = srvyr::survey_mean(na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Results should be very close (allowing for small numerical differences)
  expect_equal(sum(result$stat), sum(direct_result$stat), tolerance = 1e-10)
  expect_equal(nrow(result), nrow(direct_result))
})

test_that("kobo_interact produces statistically accurate results with grouping", {
  test_data <- create_minimal_interact_test_data()

  # Add a simple grouping variable
  test_data$data$group <- sample(
    c("G1", "G2"),
    nrow(test_data$data),
    replace = TRUE
  )
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result <- kobo_interact(
    design = test_data$design,
    vars = c("var_a", "var_b"),
    group = "group",
    survey = test_data$survey,
    label_survey = FALSE
  )

  # Calculate the same interaction directly with srvyr
  direct_result <- test_data$design %>%
    srvyr::group_by(group, srvyr::interact(var_a, var_b)) %>%
    srvyr::summarise(
      stat = srvyr::survey_mean(na.rm = TRUE, vartype = "ci"),
      .groups = "drop"
    )

  # Check that the total proportions are consistent
  result_group_totals <- result[, .(total = sum(stat)), by = group_key_value]
  direct_group_totals <- data.table::as.data.table(direct_result)[,
    .(total = sum(stat)),
    by = group
  ]

  expect_equal(nrow(result_group_totals), nrow(direct_group_totals))
  expect_true(all(abs(result_group_totals$total - 1) < 1e-10))
})

# Test parameter validation
test_that("kobo_interact validates parameters correctly", {
  test_data <- create_minimal_interact_test_data()

  # Test invalid level
  expect_error(
    kobo_interact(
      design = test_data$design,
      vars = c("var_a", "var_b"),
      survey = test_data$survey,
      level = 1.5 # Invalid confidence level
    )
  )

  # Test invalid vartype
  expect_error(
    kobo_interact(
      design = test_data$design,
      vars = c("var_a", "var_b"),
      survey = test_data$survey,
      vartype = "invalid_vartype"
    )
  )

  # Test single variable (should produce an error)
  expect_error(
    kobo_interact(
      design = test_data$design,
      vars = "var_a", # Only one variable
      survey = test_data$survey
    ),
    "Must have length >= 2"
  )
})

test_that("kobo_interact handles empty results gracefully", {
  # Create data where all combinations are filtered out
  data <- data.frame(
    var1 = rep(NA, 10),
    var2 = rep(NA, 10),
    weights = rep(1, 10),
    strata = rep("S1", 10)
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("select_one", "select_one"),
    list_name = c("list1", "list2"),
    name = c("var1", "var2"),
    label = c("Variable 1", "Variable 2")
  )

  # This should handle the error gracefully
  expect_error(
    kobo_interact(
      design = design,
      vars = c("var1", "var2"),
      survey = survey,
      na_rm = TRUE
    )
  )
})

test_that("kobo_interact works with single observation per combination", {
  # Create data with exactly one observation per combination
  data <- data.frame(
    var1 = c("A", "A", "B", "B"),
    var2 = c("X", "Y", "X", "Y"),
    weights = c(1, 1, 1, 1),
    strata = c("S1", "S1", "S1", "S1")
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("select_one", "select_one"),
    list_name = c("list1", "list2"),
    name = c("var1", "var2"),
    label = c("Variable 1", "Variable 2")
  )

  result <- kobo_interact(
    design = design,
    vars = c("var1", "var2"),
    survey = survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 4) # All 4 combinations should be present
  expect_equal(sum(result$stat), 1, tolerance = 1e-10) # Should sum to 1

  # Each combination should have proportion 0.25
  expect_true(all(abs(result$stat - 0.25) < 1e-10))
})

test_that("kobo_interact handles special characters in data", {
  # Create data with special characters and spaces
  data <- data.frame(
    var1 = c("Option A", "Option B", "Option A", "Option B"),
    var2 = c("Type-1", "Type-2", "Type-2", "Type-1"),
    weights = c(1, 1, 1, 1),
    strata = c("S1", "S1", "S1", "S1")
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("select_one", "select_one"),
    list_name = c("list1", "list2"),
    name = c("var1", "var2"),
    label = c("Variable 1", "Variable 2")
  )

  choices <- data.table::data.table(
    list_name = c("list1", "list1", "list2", "list2"),
    name = c("Option A", "Option B", "Type-1", "Type-2"),
    label = c("First Option", "Second Option", "First Type", "Second Type")
  )

  result <- kobo_interact(
    design = design,
    vars = c("var1", "var2"),
    survey = survey,
    choices = choices,
    label_survey = TRUE
  )

  expect_s3_class(result, "data.table")
  expect_true("var_value_label" %in% colnames(result))

  # Check that special characters are handled correctly
  expect_true(any(grepl("First Option", result$var_value_label)))
  expect_true(any(grepl("First Type", result$var_value_label)))
})
