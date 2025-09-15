# Tests for kobo_select_one_all function

# Helper function to create test data for kobo_select_one_all
create_kobo_all_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create sample survey data with multiple select_one variables
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
    q4_select_one = sample(
      c("agree", "disagree", "neutral"),
      n,
      replace = TRUE,
      prob = c(0.45, 0.35, 0.2)
    ),
    q5_numeric = rnorm(n),
    q6_text = paste0("text_", seq_len(n)),
    q7_select_multiple = sample(
      c("a", "b", "c"),
      n,
      replace = TRUE
    ),
    group_var = sample(c("GroupA", "GroupB"), n, replace = TRUE),
    group_var2 = sample(c("X", "Y", "Z"), n, replace = TRUE),
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE)
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create survey sheet (split format) with mixed question types
  survey <- data.table::data.table(
    type = c(
      "select_one",
      "select_one",
      "select_one",
      "select_one",
      "integer",
      "text",
      "select_multiple"
    ),
    list_name = c(
      "list1",
      "list2",
      "list3",
      "list4",
      NA,
      NA,
      "list5"
    ),
    name = c(
      "q1_select_one",
      "q2_select_one",
      "q3_select_one",
      "q4_select_one",
      "q5_numeric",
      "q6_text",
      "q7_select_multiple"
    ),
    label = c(
      "Question 1",
      "Question 2",
      "Question 3",
      "Question 4",
      "Question 5",
      "Question 6",
      "Question 7"
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
      "list3",
      "list4",
      "list4",
      "list4",
      "list5",
      "list5",
      "list5"
    ),
    name = c(
      "option1",
      "option2",
      "option3",
      "yes",
      "no",
      "low",
      "medium",
      "high",
      "agree",
      "disagree",
      "neutral",
      "a",
      "b",
      "c"
    ),
    label = c(
      "Option 1",
      "Option 2",
      "Option 3",
      "Yes",
      "No",
      "Low",
      "Medium",
      "High",
      "Agree",
      "Disagree",
      "Neutral",
      "Choice A",
      "Choice B",
      "Choice C"
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
create_minimal_kobo_test_data <- function() {
  set.seed(456)
  n <- 50

  data <- data.frame(
    q1 = sample(c("a", "b"), n, replace = TRUE),
    q2 = sample(c("x", "y", "z"), n, replace = TRUE),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("select_one", "select_one"),
    list_name = c("list1", "list2"),
    name = c("q1", "q2"),
    label = c("Question 1", "Question 2")
  )

  choices <- data.table::data.table(
    list_name = c("list1", "list1", "list2", "list2", "list2"),
    name = c("a", "b", "x", "y", "z"),
    label = c("A", "B", "X", "Y", "Z")
  )

  list(
    data = data,
    design = design,
    survey = survey,
    choices = choices
  )
}

test_that("kobo_select_one_all finds and processes all select_one variables", {
  test_data <- create_kobo_all_test_data()

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")

  # Should include all 4 select_one variables
  expect_setequal(
    unique(result$var),
    c("q1_select_one", "q2_select_one", "q3_select_one", "q4_select_one")
  )

  # Should have correct number of rows (3+2+3+3 = 11 options total)
  expect_equal(nrow(result), 11)

  # All should be marked as select_one analysis
  expect_equal(unique(result$analysis), "select_one")

  # Check that proportions sum to 1 within each variable
  for (var in unique(result$var)) {
    var_data <- result[result$var == var, ]
    expect_equal(sum(var_data$stat), 4.0, tolerance = 1e-6)
  }
})

test_that("kobo_select_one_all excludes grouping variables", {
  test_data <- create_kobo_all_test_data()

  # Add a select_one variable that will be used for grouping
  test_data$data$group_select_one <- sample(
    c("type1", "type2"),
    nrow(test_data$data),
    replace = TRUE
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
      type = "select_one",
      list_name = "list_group",
      name = "group_select_one",
      label = "Grouping Question"
    )
  )

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = survey_extended,
    choices = test_data$choices,
    group = "group_select_one"
  )

  # Should NOT include the grouping variable
  expect_false("group_select_one" %in% unique(result$var))

  # Should still include other select_one variables
  expect_setequal(
    unique(result$var),
    c("q1_select_one", "q2_select_one", "q3_select_one", "q4_select_one")
  )

  # Should have grouping columns
  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
})

test_that("kobo_select_one_all excludes multiple grouping variables", {
  test_data <- create_kobo_all_test_data()

  # Add two select_one variables for grouping
  test_data$data$group1 <- sample(
    c("g1a", "g1b"),
    nrow(test_data$data),
    replace = TRUE
  )
  test_data$data$group2 <- sample(
    c("g2x", "g2y"),
    nrow(test_data$data),
    replace = TRUE
  )

  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  survey_extended <- rbind(
    test_data$survey,
    data.table::data.table(
      type = c("select_one", "select_one"),
      list_name = c("list_g1", "list_g2"),
      name = c("group1", "group2"),
      label = c("Group 1", "Group 2")
    )
  )

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = survey_extended,
    group = c("group1", "group2")
  )

  # Should exclude both grouping variables
  expect_false(any(c("group1", "group2") %in% unique(result$var)))

  # Should include original select_one variables
  expect_setequal(
    unique(result$var),
    c("q1_select_one", "q2_select_one", "q3_select_one", "q4_select_one")
  )
})

test_that("kobo_select_one_all works without choices", {
  test_data <- create_kobo_all_test_data()

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = NULL
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 11) # All options from 4 variables
  expect_false("var_value_label" %in% colnames(result))
  expect_true("var_label" %in% colnames(result)) # Should still have survey labels
})

test_that("kobo_select_one_all works without survey labels", {
  test_data <- create_kobo_all_test_data()

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_false("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result)) # Should still have choice labels
})

test_that("kobo_select_one_all handles different variance types", {
  test_data <- create_minimal_kobo_test_data()

  # Test confidence intervals
  result_ci <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    vartype = "ci"
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))

  # Test standard errors
  result_se <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    vartype = "se"
  )
  expect_true("stat_se" %in% colnames(result_se))

  # Test variances
  result_var <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    vartype = "var"
  )
  expect_true("stat_var" %in% colnames(result_var))
})

test_that("kobo_select_one_all handles different confidence levels", {
  test_data <- create_minimal_kobo_test_data()

  for (level in c(0.90, 0.95, 0.99)) {
    result <- kobo_select_one_all(
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
    expect_true(all(ci_widths < 1))
  }
})

test_that("kobo_select_one_all handles NA values correctly", {
  test_data <- create_minimal_kobo_test_data()

  # Add some NA values
  test_data$data$q1[1:5] <- NA
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # Test with na_rm = TRUE
  result_na_rm <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    na_rm = TRUE
  )

  # Check that NA counts include the variable with NAs
  expect_true(5 %in% result_na_rm$na_count_tot)
  expect_equal(unique(result_na_rm$n_tot), 50)

  # Test with na_rm = FALSE
  result_na_keep <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    na_rm = FALSE
  )

  expect_true(5 %in% result_na_keep$na_count_tot)
  expect_equal(unique(result_na_keep$n_tot), 50)
})

test_that("kobo_select_one_all warns when no select_one variables available", {
  test_data <- create_kobo_all_test_data()

  # Create survey with only non-select_one variables
  survey_no_select_one <- data.table::data.table(
    type = c("integer", "text", "select_multiple"),
    list_name = c(NA, NA, "list1"),
    name = c("q5_numeric", "q6_text", "q7_select_multiple"),
    label = c("Question 5", "Question 6", "Question 7")
  )

  expect_warning(
    result <- kobo_select_one_all(
      design = test_data$design,
      survey = survey_no_select_one
    ),
    "There are no vars of type select_one"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("kobo_select_one_all warns when all select_one variables are grouping variables", {
  test_data <- create_minimal_kobo_test_data()

  expect_warning(
    result <- kobo_select_one_all(
      design = test_data$design,
      survey = test_data$survey,
      group = c("q1", "q2") # Both select_one variables used for grouping
    ),
    "There are no vars of type select_one which are not grouping columns"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("kobo_select_one_all works with custom group separator", {
  test_data <- create_kobo_all_test_data()

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    group = c("group_var", "group_var2"),
    group_key_sep = " | "
  )

  expect_true("group_key" %in% colnames(result))
  expect_equal(unique(result$group_key), "group_var | group_var2")
  expect_true(any(grepl(" | ", result$group_key_value)))
})

test_that("kobo_select_one_all preserves choice and variable labels", {
  test_data <- create_kobo_all_test_data()

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Check variable labels exist
  expect_true("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result))

  # Check specific labels
  q1_results <- result[result$var == "q1_select_one", ]
  expect_true(all(q1_results$var_label == "Question 1"))

  # Check choice labels
  option1_result <- result[
    result$var == "q1_select_one" & result$var_value == "option1",
  ]
  expect_equal(option1_result$var_value_label[1], "Option 1")
})

test_that("kobo_select_one_all returns data.table", {
  test_data <- create_minimal_kobo_test_data()

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_false(
    inherits(result, "data.frame") && !inherits(result, "data.table")
  )
})

test_that("kobo_select_one_all handles single select_one variable", {
  test_data <- create_minimal_kobo_test_data()

  # Survey with only one select_one variable
  survey_single <- data.table::data.table(
    type = "select_one",
    list_name = "list1",
    name = "q1",
    label = "Question 1"
  )

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = survey_single
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$var), "q1")
  expect_equal(nrow(result), 2) # Two options: a, b
  expect_equal(sum(result$stat), 1.0, tolerance = 1e-10)
})

test_that("kobo_select_one_all works with mixed question types in survey", {
  test_data <- create_kobo_all_test_data()

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey
  )

  # Should only include select_one variables, not others
  expect_setequal(
    unique(result$var),
    c("q1_select_one", "q2_select_one", "q3_select_one", "q4_select_one")
  )

  # Should not include integer, text, or select_multiple variables
  expect_false("q5_numeric" %in% unique(result$var))
  expect_false("q6_text" %in% unique(result$var))
  expect_false("q7_select_multiple" %in% unique(result$var))
})

test_that("kobo_select_one_all equivalent to kobo_select_one with all vars", {
  test_data <- create_minimal_kobo_test_data()

  # Get all select_one variables manually
  select_one_vars <- get_survey_select_one(test_data$survey)

  result_all <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  result_manual <- kobo_select_one(
    design = test_data$design,
    vars = select_one_vars,
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Results should be identical
  expect_equal(nrow(result_all), nrow(result_manual))
  expect_setequal(result_all$var, result_manual$var)
  expect_equal(result_all$stat, result_manual$stat, tolerance = 1e-10)
})

test_that("kobo_select_one_all handles factor variables", {
  test_data <- create_minimal_kobo_test_data()

  # Convert to factors
  test_data$data$q1 <- factor(test_data$data$q1, levels = c("b", "a"))
  test_data$data$q2 <- factor(test_data$data$q2, levels = c("z", "y", "x"))

  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 5) # 2 + 3 options

  # Should work with factor levels
  q1_values <- result[result$var == "q1", ]$var_value
  q2_values <- result[result$var == "q2", ]$var_value

  expect_setequal(q1_values, c("a", "b"))
  expect_setequal(q2_values, c("x", "y", "z"))
})

test_that("kobo_select_one_all handles all NA variables correctly", {
  test_data <- create_minimal_kobo_test_data()

  # Make one variable all NA
  test_data$data$q1 <- rep(NA_character_, nrow(test_data$data))
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # Expect warning about all-NA variable
  expect_warning(
    result <- kobo_select_one_all(
      design = test_data$design,
      survey = test_data$survey
    ),
    "Variable 'q1' only contains missing values. Returning an empty data frame."
  )
})

test_that("kobo_select_one_all parameter validation works", {
  test_data <- create_minimal_kobo_test_data()

  # Test invalid vartype
  expect_error(
    kobo_select_one_all(
      design = test_data$design,
      survey = test_data$survey,
      vartype = "invalid"
    ),
    "'arg' should be one of"
  )

  # Test invalid confidence level
  expect_error(
    kobo_select_one_all(
      design = test_data$design,
      survey = test_data$survey,
      level = 1.5
    ),
    "Element 1 is not <= 1"
  )
})

test_that("kobo_select_one_all maintains survey design integrity", {
  test_data <- create_kobo_all_test_data()

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = test_data$survey
  )

  # Check that survey design information is preserved
  expect_true("stat_unw" %in% colnames(result)) # Unweighted stats
  expect_true("n_unw" %in% colnames(result)) # Unweighted counts
  expect_true("n_tot" %in% colnames(result)) # Total sample size
  expect_true("n_tot_unw" %in% colnames(result)) # Total unweighted sample size
})

test_that("kobo_select_one_all handles numeric categorical variables", {
  test_data <- create_minimal_kobo_test_data()

  # Add numeric select_one variable
  test_data$data$q3_numeric <- sample(
    c(1, 2, 3),
    nrow(test_data$data),
    replace = TRUE
  )

  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  survey_extended <- rbind(
    test_data$survey,
    data.table::data.table(
      type = "select_one",
      list_name = "list_numeric",
      name = "q3_numeric",
      label = "Numeric Question"
    )
  )

  result <- kobo_select_one_all(
    design = test_data$design,
    survey = survey_extended
  )

  # Should include the numeric variable
  expect_true("q3_numeric" %in% unique(result$var))

  # Values should be converted to character
  q3_results <- result[result$var == "q3_numeric", ]
  expect_true(is.character(q3_results$var_value))
  expect_setequal(q3_results$var_value, c("1", "2", "3"))
})

test_that("kobo_select_one_all works with empty survey", {
  test_data <- create_minimal_kobo_test_data()

  # Empty survey
  empty_survey <- data.table::data.table(
    type = character(0),
    list_name = character(0),
    name = character(0),
    label = character(0)
  )

  expect_warning(
    result <- kobo_select_one_all(
      design = test_data$design,
      survey = empty_survey
    ),
    "There are no vars of type select_one"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})
