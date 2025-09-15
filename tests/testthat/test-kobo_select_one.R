# Tests for kobo_select_one function

# Helper function to create test data for kobo_select_one
create_kobo_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create sample survey data
  data <- data.frame(
    q1_select_one = sample(
      c("option1", "option2", "option3"),
      n,
      replace = TRUE,
      prob = c(0.4, 0.35, 0.25)
    ),
    q2_select_one = sample(
      c("yes", "no"),
      n,
      replace = TRUE,
      prob = c(0.6, 0.4)
    ),
    q3_select_one = sample(
      c("low", "medium", "high"),
      n,
      replace = TRUE,
      prob = c(0.3, 0.4, 0.3)
    ),
    q4_numeric = rnorm(n),
    q5_text = paste0("text_", seq_len(n)),
    group_var = sample(c("GroupA", "GroupB"), n, replace = TRUE),
    group_var2 = sample(c("X", "Y", "Z"), n, replace = TRUE),
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create survey sheet (split format)
  survey <- data.table::data.table(
    type = c("select_one", "select_one", "select_one", "integer", "text"),
    list_name = c("list1", "list2", "list3", NA, NA),
    name = c(
      "q1_select_one",
      "q2_select_one",
      "q3_select_one",
      "q4_numeric",
      "q5_text"
    ),
    label = c(
      "Question 1",
      "Question 2",
      "Question 3",
      "Question 4",
      "Question 5"
    )
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
      "list3"
    ),
    name = c(
      "option1",
      "option2",
      "option3",
      "yes",
      "no",
      "low",
      "medium",
      "high"
    ),
    label = c(
      "Option 1",
      "Option 2",
      "Option 3",
      "Yes",
      "No",
      "Low",
      "Medium",
      "High"
    )
  )

  list(
    data = data,
    design = design,
    survey = survey,
    choices = choices
  )
}

# Test basic functionality
test_that("kobo_select_one works with single variable", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$var), "q1_select_one")
  expect_equal(unique(result$analysis), "select_one")
  expect_setequal(result$var_value, c("option1", "option2", "option3"))
  expect_equal(nrow(result), 3)

  # Check that proportions sum to 1
  expect_equal(sum(result$stat), 1.0, tolerance = 1e-10)

  # Check required columns exist
  expect_true("stat" %in% colnames(result))
  expect_true("stat_low" %in% colnames(result))
  expect_true("stat_upp" %in% colnames(result))
  expect_true("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result))
})

test_that("kobo_select_one works with multiple variables", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = c("q1_select_one", "q2_select_one"),
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_equal(nrow(result), 5) # 3 options for q1 + 2 options for q2
  expect_setequal(unique(result$var), c("q1_select_one", "q2_select_one"))
  expect_equal(unique(result$analysis), "select_one")

  # Check proportions sum to 1 within each variable
  q1_results <- result[result$var == "q1_select_one", ]
  q2_results <- result[result$var == "q2_select_one", ]
  expect_equal(sum(q1_results$stat), 1.0, tolerance = 1e-10)
  expect_equal(sum(q2_results$stat), 1.0, tolerance = 1e-10)
})

test_that("kobo_select_one works with grouping", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey,
    choices = test_data$choices,
    group = "group_var"
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_equal(unique(result$group_key), "group_var")
  expect_setequal(unique(result$group_key_value), c("GroupA", "GroupB"))

  # Check proportions sum to 1 within each group
  for (grp in c("GroupA", "GroupB")) {
    grp_data <- result[result$group_key_value == grp, ]
    expect_equal(sum(grp_data$stat), 1.0, tolerance = 1e-8)
  }
})

test_that("kobo_select_one works with multiple grouping variables", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey,
    choices = test_data$choices,
    group = c("group_var", "group_var2"),
    group_key_sep = " | "
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_equal(unique(result$group_key), "group_var | group_var2")

  # Check that group_key_value contains the separator
  expect_true(any(grepl(" | ", result$group_key_value)))

  # Check proportions sum to 1 within each group combination
  unique_groups <- unique(result$group_key_value)
  for (grp in unique_groups) {
    grp_data <- result[result$group_key_value == grp, ]
    expect_equal(sum(grp_data$stat), 1.0, tolerance = 1e-8)
  }
})

test_that("kobo_select_one works without survey labels", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = FALSE
  )

  expect_false("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result)) # Should still have choice labels
})

test_that("kobo_select_one works without choices", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey,
    choices = NULL
  )

  expect_true("var_label" %in% colnames(result))
  expect_false("var_value_label" %in% colnames(result))
  expect_setequal(result$var_value, c("option1", "option2", "option3"))
})

test_that("kobo_select_one handles different variance types", {
  test_data <- create_kobo_test_data()

  # Test confidence intervals
  result_ci <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey,
    vartype = "ci"
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))
  expect_false("stat_se" %in% colnames(result_ci))

  # Test standard errors
  result_se <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey,
    vartype = "se"
  )
  expect_true("stat_se" %in% colnames(result_se))
  expect_false(any(c("stat_low", "stat_upp") %in% colnames(result_se)))

  # Test variances
  result_var <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey,
    vartype = "var"
  )
  expect_true("stat_var" %in% colnames(result_var))
  expect_false(any(
    c("stat_low", "stat_upp", "stat_se") %in% colnames(result_var)
  ))

  # Estimates should be the same regardless of vartype
  expect_equal(result_ci$stat, result_se$stat, tolerance = 1e-10)
  expect_equal(result_ci$stat, result_var$stat, tolerance = 1e-10)
})

test_that("kobo_select_one handles different confidence levels", {
  test_data <- create_kobo_test_data()

  levels <- c(0.90, 0.95, 0.99)

  for (level in levels) {
    result <- kobo_select_one(
      design = test_data$design,
      vars = "q1_select_one",
      survey = test_data$survey,
      level = level,
      vartype = "ci"
    )

    # Check that CI widths are reasonable and increase with confidence level
    ci_widths <- result$stat_upp - result$stat_low
    expect_true(all(ci_widths >= 0))
    expect_true(all(ci_widths < 1))
  }
})

test_that("kobo_select_one handles NA values correctly", {
  test_data <- create_kobo_test_data()

  # Add some NA values
  test_data$data$q1_select_one[1:10] <- NA
  design_with_na <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # Test with na_rm = TRUE
  result_na_rm <- kobo_select_one(
    design = design_with_na,
    vars = "q1_select_one",
    survey = test_data$survey,
    na_rm = TRUE
  )

  expect_equal(unique(result_na_rm$na_count_tot), 10)
  expect_equal(unique(result_na_rm$n_tot), 100)
  expect_setequal(result_na_rm$var_value, c("option1", "option2", "option3"))
  expect_equal(sum(result_na_rm$stat), 1.0, tolerance = 1e-10)

  # Test with na_rm = FALSE
  result_na_keep <- kobo_select_one(
    design = design_with_na,
    vars = "q1_select_one",
    survey = test_data$survey,
    na_rm = FALSE
  )

  expect_equal(unique(result_na_keep$na_count_tot), 10)
  expect_equal(unique(result_na_keep$n_tot), 100)
})

test_that("kobo_select_one validates input parameters", {
  test_data <- create_kobo_test_data()

  # Test with non-select_one variable
  expect_error(
    kobo_select_one(
      design = test_data$design,
      vars = "q4_numeric", # This is not a select_one
      survey = test_data$survey
    ),
    "Variable is not a select_one in survey"
  )

  # Test with missing variable
  expect_error(
    kobo_select_one(
      design = test_data$design,
      vars = "nonexistent_var",
      survey = test_data$survey
    ),
    "Missing variables in design"
  )

  # Test with invalid vartype (error comes from srvyr)
  expect_error(
    kobo_select_one(
      design = test_data$design,
      vars = "q1_select_one",
      survey = test_data$survey,
      vartype = "invalid"
    ),
    "'arg' should be one of"
  )

  # Test with invalid confidence level
  expect_error(
    kobo_select_one(
      design = test_data$design,
      vars = "q1_select_one",
      survey = test_data$survey,
      level = 1.5
    ),
    "Element 1 is not <= 1"
  )
})

test_that("kobo_select_one handles edge cases", {
  test_data <- create_kobo_test_data()

  # Test with single category (all same value)
  single_cat_data <- test_data$data
  single_cat_data$q1_select_one <- rep("option1", nrow(single_cat_data))

  design_single <- srvyr::as_survey_design(
    single_cat_data,
    weights = weights,
    strata = strata
  )

  result_single <- kobo_select_one(
    design = design_single,
    vars = "q1_select_one",
    survey = test_data$survey
  )

  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$var_value[1], "option1")
  expect_equal(result_single$stat[1], 1.0)

  # Test with minimal data
  minimal_data <- data.frame(
    q1_select_one = c("option1", "option2", "option1"),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1")
  )

  design_minimal <- srvyr::as_survey_design(
    minimal_data,
    weights = weights,
    strata = strata
  )

  survey_minimal <- data.table::data.table(
    type = "select_one",
    list_name = "list1",
    name = "q1_select_one",
    label = "Question 1"
  )

  result_minimal <- kobo_select_one(
    design = design_minimal,
    vars = "q1_select_one",
    survey = survey_minimal
  )

  expect_equal(nrow(result_minimal), 2)
  expect_setequal(result_minimal$var_value, c("option1", "option2"))
  expect_equal(sum(result_minimal$stat), 1.0, tolerance = 1e-10)
})

test_that("kobo_select_one handles all NA variables correctly", {
  test_data <- create_kobo_test_data()

  # Create data with all NAs
  data_all_na <- test_data$data
  data_all_na$q1_select_one <- rep(NA_character_, nrow(data_all_na))

  design_all_na <- srvyr::as_survey_design(
    data_all_na,
    weights = weights,
    strata = strata
  )

  # Should return empty data frame with warning
  expect_warning(
    result_all_na <- kobo_select_one(
      design = design_all_na,
      vars = "q1_select_one",
      survey = test_data$survey
    ),
    "only contains missing values"
  )
  expect_equal(nrow(result_all_na), 0)
})

test_that("kobo_select_one works with factor variables", {
  test_data <- create_kobo_test_data()

  # Convert to factor with specific level order
  test_data$data$q1_select_one <- factor(
    test_data$data$q1_select_one,
    levels = c("option3", "option1", "option2")
  )

  design_factor <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result <- kobo_select_one(
    design = design_factor,
    vars = "q1_select_one",
    survey = test_data$survey
  )

  expect_equal(nrow(result), 3)
  expect_setequal(result$var_value, c("option1", "option2", "option3"))
  expect_equal(sum(result$stat), 1.0, tolerance = 1e-10)
})

test_that("kobo_select_one preserves choice labels correctly", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Check that choice labels are correctly assigned
  for (i in seq_len(nrow(result))) {
    var_value <- result$var_value[i]
    expected_label <- test_data$choices[
      test_data$choices$list_name == "list1" &
        test_data$choices$name == var_value,
      "label"
    ][[1]]
    expect_equal(result$var_value_label[i], expected_label)
  }
})

test_that("kobo_select_one preserves variable labels correctly", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = c("q1_select_one", "q2_select_one"),
    survey = test_data$survey,
    label_survey = TRUE
  )

  # Check variable labels
  q1_results <- result[result$var == "q1_select_one", ]
  q2_results <- result[result$var == "q2_select_one", ]

  expect_true(all(q1_results$var_label == "Question 1"))
  expect_true(all(q2_results$var_label == "Question 2"))
})

test_that("kobo_select_one returns data.table", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
})

test_that("kobo_select_one var_value column is character", {
  test_data <- create_kobo_test_data()

  result <- kobo_select_one(
    design = test_data$design,
    vars = "q1_select_one",
    survey = test_data$survey
  )

  expect_true(is.character(result$var_value))
  expect_false(is.factor(result$var_value))
})

test_that("kobo_select_one handles numeric categorical variables", {
  test_data <- create_kobo_test_data()

  # Add a numeric select_one variable
  test_data$data$q6_numeric_select <- sample(c(1, 2, 3), 100, replace = TRUE)

  # Update survey to include this variable
  survey_extended <- rbind(
    test_data$survey,
    data.table::data.table(
      type = "select_one",
      list_name = "list4",
      name = "q6_numeric_select",
      label = "Numeric Question"
    )
  )

  # Update choices
  choices_extended <- rbind(
    test_data$choices,
    data.table::data.table(
      list_name = rep("list4", 3),
      name = c("1", "2", "3"),
      label = c("One", "Two", "Three")
    )
  )

  design_extended <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result <- kobo_select_one(
    design = design_extended,
    vars = "q6_numeric_select",
    survey = survey_extended,
    choices = choices_extended
  )

  expect_equal(nrow(result), 3)
  expect_setequal(result$var_value, c("1", "2", "3")) # Should be character
  expect_setequal(result$var_value_label, c("One", "Two", "Three"))
  expect_equal(sum(result$stat), 1.0, tolerance = 1e-10)
})
