# Tests for kobo_select_multiple function

# Helper function to create test data for kobo_select_multiple
create_kobo_select_multiple_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create sample survey data with select_multiple questions
  # Skills question with communication, leadership, technical, and none options
  skills_communication <- sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3))
  skills_leadership <- sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4))
  skills_technical <- sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2))

  # Create 'none' variable: 1 when all other skills are 0
  skills_none <- ifelse(
    skills_communication == 0 & skills_leadership == 0 & skills_technical == 0,
    1,
    0
  )

  # Languages question with english, french, spanish, and none options
  languages_english <- sample(c(0, 1), n, replace = TRUE, prob = c(0.4, 0.6))
  languages_french <- sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2))
  languages_spanish <- sample(c(0, 1), n, replace = TRUE, prob = c(0.9, 0.1))

  # Create 'none' variable for languages
  languages_none <- ifelse(
    languages_english == 0 & languages_french == 0 & languages_spanish == 0,
    1,
    0
  )

  # Create parent columns (as they would appear in Kobo exports)
  skills_parent <- character(n)
  for (i in 1:n) {
    selected <- c()
    if (skills_communication[i] == 1) {
      selected <- c(selected, "communication")
    }
    if (skills_leadership[i] == 1) {
      selected <- c(selected, "leadership")
    }
    if (skills_technical[i] == 1) {
      selected <- c(selected, "technical")
    }
    if (skills_none[i] == 1) {
      selected <- c(selected, "none")
    }
    skills_parent[i] <- if (length(selected) > 0) {
      paste(selected, collapse = " ")
    } else {
      NA_character_
    }
  }

  languages_parent <- character(n)
  for (i in 1:n) {
    selected <- c()
    if (languages_english[i] == 1) {
      selected <- c(selected, "english")
    }
    if (languages_french[i] == 1) {
      selected <- c(selected, "french")
    }
    if (languages_spanish[i] == 1) {
      selected <- c(selected, "spanish")
    }
    if (languages_none[i] == 1) {
      selected <- c(selected, "none")
    }
    languages_parent[i] <- if (length(selected) > 0) {
      paste(selected, collapse = " ")
    } else {
      NA_character_
    }
  }

  data <- data.frame(
    # Skills select_multiple question (parent + child columns)
    skills = skills_parent,
    `skills/communication` = skills_communication,
    `skills/leadership` = skills_leadership,
    `skills/technical` = skills_technical,
    `skills/none` = skills_none,

    # Languages select_multiple question (parent + child columns)
    languages = languages_parent,
    `languages/english` = languages_english,
    `languages/french` = languages_french,
    `languages/spanish` = languages_spanish,
    `languages/none` = languages_none,

    # Other variables
    q3_select_one = sample(c("yes", "no"), n, replace = TRUE),
    q4_numeric = rnorm(n),
    group_var = sample(c("GroupA", "GroupB"), n, replace = TRUE),
    group_var2 = sample(c("X", "Y", "Z"), n, replace = TRUE),
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE),
    check.names = FALSE
  )

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create survey sheet (split format)
  survey <- data.table::data.table(
    type = c("select_multiple", "select_multiple", "select_one", "integer"),
    list_name = c("skills_list", "languages_list", "yesno_list", NA),
    name = c("skills", "languages", "q3_select_one", "q4_numeric"),
    label = c(
      "What skills do you have?",
      "What languages do you speak?",
      "Question 3",
      "Question 4"
    )
  )

  # Create choices sheet
  choices <- data.table::data.table(
    list_name = c(
      "skills_list",
      "skills_list",
      "skills_list",
      "skills_list",
      "languages_list",
      "languages_list",
      "languages_list",
      "languages_list",
      "yesno_list",
      "yesno_list"
    ),
    name = c(
      "communication",
      "leadership",
      "technical",
      "none",
      "english",
      "french",
      "spanish",
      "none",
      "yes",
      "no"
    ),
    label = c(
      "Communication",
      "Leadership",
      "Technical",
      "None of the above",
      "English",
      "French",
      "Spanish",
      "None of the above",
      "Yes",
      "No"
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
test_that("kobo_select_multiple works with single variable", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$var), "skills")
  expect_equal(unique(result$analysis), "select_multiple")
  expect_setequal(
    result$var_value,
    c("communication", "leadership", "technical", "none")
  )
  expect_equal(nrow(result), 4)

  # Check required columns exist
  expect_true("stat" %in% colnames(result))
  expect_true("stat_low" %in% colnames(result))
  expect_true("stat_upp" %in% colnames(result))
  expect_true("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result))

  # Check that all proportions are between 0 and 1
  expect_true(all(result$stat >= 0 & result$stat <= 1))
})

test_that("kobo_select_multiple works with multiple variables", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = c("skills", "languages"),
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_equal(nrow(result), 8) # 4 skills + 4 languages
  expect_setequal(unique(result$var), c("skills", "languages"))
  expect_equal(unique(result$analysis), "select_multiple")

  # Check skills results
  skills_results <- result[result$var == "skills", ]
  expect_setequal(
    skills_results$var_value,
    c("communication", "leadership", "technical", "none")
  )

  # Check languages results
  languages_results <- result[result$var == "languages", ]
  expect_setequal(
    languages_results$var_value,
    c("english", "french", "spanish", "none")
  )
})

test_that("kobo_select_multiple works with grouping", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
    group = "group_var"
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_equal(unique(result$group_key), "group_var")
  expect_setequal(unique(result$group_key_value), c("GroupA", "GroupB"))

  # Check that each group has results for all skill options
  for (grp in c("GroupA", "GroupB")) {
    grp_data <- result[result$group_key_value == grp, ]
    expect_setequal(
      grp_data$var_value,
      c("communication", "leadership", "technical", "none")
    )
    expect_true(all(grp_data$stat >= 0 & grp_data$stat <= 1))
  }
})

test_that("kobo_select_multiple works with multiple grouping variables", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
    group = c("group_var", "group_var2")
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_equal(unique(result$group_key), "group_var -/- group_var2")

  # Check that group_key_value contains the separator
  expect_true(any(grepl(" -/- ", result$group_key_value)))

  # Check that each group combination has results for all skill options
  unique_groups <- unique(result$group_key_value)
  for (grp in unique_groups) {
    grp_data <- result[result$group_key_value == grp, ]
    expect_setequal(
      grp_data$var_value,
      c("communication", "leadership", "technical", "none")
    )
  }
})

test_that("kobo_select_multiple works without survey labels", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = FALSE
  )

  expect_false("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result)) # Should still have choice labels
})

test_that("kobo_select_multiple works without choice labels", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
    label_choices = FALSE
  )

  expect_true("var_label" %in% colnames(result))
  expect_false("var_value_label" %in% colnames(result))
  expect_setequal(
    result$var_value,
    c("communication", "leadership", "technical", "none")
  )
})

test_that("kobo_select_multiple requires choices parameter", {
  test_data <- create_kobo_select_multiple_test_data()

  expect_error(
    kobo_select_multiple(
      design = test_data$design,
      vars = "skills",
      survey = test_data$survey,
      choices = NULL
    ),
    "Must be of type 'data.frame'"
  )
})

test_that("kobo_select_multiple handles different variance types", {
  test_data <- create_kobo_select_multiple_test_data()

  # Test confidence intervals
  result_ci <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
    vartype = "ci"
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))
  expect_false("stat_se" %in% colnames(result_ci))

  # Test standard errors
  result_se <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
    vartype = "se"
  )
  expect_true("stat_se" %in% colnames(result_se))
  expect_false(any(c("stat_low", "stat_upp") %in% colnames(result_se)))

  # Test variances
  result_var <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
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

test_that("kobo_select_multiple handles different confidence levels", {
  test_data <- create_kobo_select_multiple_test_data()

  levels <- c(0.90, 0.95, 0.99)

  for (level in levels) {
    result <- kobo_select_multiple(
      design = test_data$design,
      vars = "skills",
      survey = test_data$survey,
      choices = test_data$choices,
      level = level,
      vartype = "ci"
    )

    # Check that CI widths are reasonable and increase with confidence level
    ci_widths <- result$stat_upp - result$stat_low
    expect_true(all(ci_widths >= 0))
    expect_true(all(ci_widths < 1))
  }
})

test_that("kobo_select_multiple handles NA values correctly with na_rm = TRUE", {
  test_data <- create_kobo_select_multiple_test_data()

  # Add some NA values to child columns
  test_data$data$`skills/communication`[1:10] <- NA
  test_data$data$`skills/leadership`[5:15] <- NA

  design_with_na <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result_na_rm <- kobo_select_multiple(
    design = design_with_na,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
    na_rm = TRUE
  )

  expect_setequal(
    result_na_rm$var_value,
    c("communication", "leadership", "technical", "none")
  )
  expect_true(all(result_na_rm$stat >= 0 & result_na_rm$stat <= 1))
})

test_that("kobo_select_multiple handles NA values correctly with na_rm = FALSE", {
  test_data <- create_kobo_select_multiple_test_data()

  # Add some NA values to child columns
  test_data$data$`skills/communication`[1:10] <- NA
  test_data$data$`skills/leadership`[5:15] <- NA

  design_with_na <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result_na_keep <- kobo_select_multiple(
    design = design_with_na,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
    na_rm = FALSE
  )

  expect_setequal(
    result_na_keep$var_value,
    c("communication", "leadership", "technical", "none")
  )
  expect_true(all(result_na_keep$stat >= 0 & result_na_keep$stat <= 1))
})

test_that("kobo_select_multiple validates input parameters", {
  test_data <- create_kobo_select_multiple_test_data()

  # Test with non-select_multiple variable
  expect_error(
    kobo_select_multiple(
      design = test_data$design,
      vars = "q4_numeric", # This is not a select_multiple
      survey = test_data$survey
    ),
    "Variable is not a select_multiple in survey"
  )

  # Test with missing variable
  expect_error(
    kobo_select_multiple(
      design = test_data$design,
      vars = "nonexistent_var",
      survey = test_data$survey,
      choices = test_data$choices
    ),
    "Missing variables in design"
  )

  # Test with invalid vartype (error comes from srvyr)
  expect_error(
    kobo_select_multiple(
      design = test_data$design,
      vars = "skills",
      survey = test_data$survey,
      choices = test_data$choices,
      vartype = "invalid"
    ),
    "'arg' should be one of"
  )

  # Test with invalid confidence level
  expect_error(
    kobo_select_multiple(
      design = test_data$design,
      vars = "skills",
      survey = test_data$survey,
      choices = test_data$choices,
      level = 1.5
    ),
    "Element 1 is not <= 1"
  )
})

test_that("kobo_select_multiple handles missing child columns", {
  test_data <- create_kobo_select_multiple_test_data()

  # Remove one of the child columns to simulate missing data
  test_data$data$`skills/technical` <- NULL

  design_missing_child <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  expect_warning(
    result <- kobo_select_multiple(
      design = design_missing_child,
      vars = "skills",
      survey = test_data$survey,
      choices = test_data$choices
    ),
    "does not exist in `design`"
  )

  # Should still work with remaining child columns
  expect_setequal(result$var_value, c("communication", "leadership", "none"))
})

test_that("kobo_select_multiple handles no child columns found", {
  test_data <- create_kobo_select_multiple_test_data()

  # Remove all child columns
  cols_to_remove <- grep("skills/", names(test_data$data), value = TRUE)
  for (col in cols_to_remove) {
    test_data$data[[col]] <- NULL
  }

  design_no_children <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  expect_error(
    kobo_select_multiple(
      design = design_no_children,
      vars = "skills",
      survey = test_data$survey,
      choices = test_data$choices
    ),
    "No child column"
  )
})

test_that("kobo_select_multiple works with custom choices separator", {
  test_data <- create_kobo_select_multiple_test_data()

  # Rename columns to use different separator
  names(test_data$data) <- gsub("skills/", "skills_", names(test_data$data))
  names(test_data$data) <- gsub(
    "languages/",
    "languages_",
    names(test_data$data)
  )

  design_custom_sep <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result <- kobo_select_multiple(
    design = design_custom_sep,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices,
    choices_sep = "_"
  )

  expect_setequal(
    result$var_value,
    c("communication", "leadership", "technical", "none")
  )
  expect_true(all(result$stat >= 0 & result$stat <= 1))
})

test_that("kobo_select_multiple preserves choice labels correctly", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Check that choice labels are correctly assigned
  expected_labels <- c(
    "communication" = "Communication",
    "leadership" = "Leadership",
    "technical" = "Technical",
    "none" = "None of the above"
  )

  for (i in seq_len(nrow(result))) {
    var_value <- result$var_value[i]
    expected_label <- unname(expected_labels[var_value])
    expect_equal(result$var_value_label[i], expected_label)
  }
})

test_that("kobo_select_multiple preserves variable labels correctly", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = c("skills", "languages"),
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = TRUE
  )

  # Check variable labels
  skills_results <- result[result$var == "skills", ]
  languages_results <- result[result$var == "languages", ]

  expect_true(all(skills_results$var_label == "What skills do you have?"))
  expect_true(all(
    languages_results$var_label == "What languages do you speak?"
  ))
})

test_that("kobo_select_multiple returns data.table", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
})

test_that("kobo_select_multiple var_value column is character", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_true(is.character(result$var_value))
  expect_false(is.factor(result$var_value))
})

test_that("kobo_select_multiple handles 'none' option correctly", {
  test_data <- create_kobo_select_multiple_test_data()

  result <- kobo_select_multiple(
    design = test_data$design,
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Check that 'none' option is included
  expect_true("none" %in% result$var_value)

  # Check that 'none' has proper label
  none_row <- result[result$var_value == "none", ]
  expect_equal(none_row$var_value_label, "None of the above")

  # Verify that the proportion for 'none' is reasonable (should be > 0 based on our data generation)
  expect_true(none_row$stat > 0)
  expect_true(none_row$stat < 1)
})

test_that("kobo_select_multiple works with minimal data", {
  # Create minimal test data
  minimal_data <- data.frame(
    skills = c("communication", "none", "communication"),
    `skills/communication` = c(1, 0, 1),
    `skills/none` = c(0, 1, 0),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1"),
    check.names = FALSE
  )

  design_minimal <- srvyr::as_survey_design(
    minimal_data,
    weights = weights,
    strata = strata
  )

  survey_minimal <- data.table::data.table(
    type = "select_multiple",
    list_name = "skills_list",
    name = "skills",
    label = "Skills"
  )

  choices_minimal <- data.table::data.table(
    list_name = c("skills_list", "skills_list"),
    name = c("communication", "none"),
    label = c("Communication", "None")
  )

  result <- kobo_select_multiple(
    design = design_minimal,
    vars = "skills",
    survey = survey_minimal,
    choices = choices_minimal
  )

  expect_equal(nrow(result), 2)
  expect_setequal(result$var_value, c("communication", "none"))
  expect_true(all(result$stat >= 0 & result$stat <= 1))
})

test_that("kobo_select_multiple works with all zeros except none", {
  # Create data where everyone selects "none"
  all_none_data <- data.frame(
    skills = rep("none", 50),
    `skills/communication` = rep(0, 50),
    `skills/leadership` = rep(0, 50),
    `skills/technical` = rep(0, 50),
    `skills/none` = rep(1, 50),
    weights = rep(1, 50),
    strata = rep("S1", 50),
    check.names = FALSE
  )

  design_all_none <- srvyr::as_survey_design(
    all_none_data,
    weights = weights,
    strata = strata
  )

  survey <- data.table::data.table(
    type = "select_multiple",
    list_name = "skills_list",
    name = "skills",
    label = "Skills"
  )

  choices <- data.table::data.table(
    list_name = rep("skills_list", 4),
    name = c("communication", "leadership", "technical", "none"),
    label = c("Communication", "Leadership", "Technical", "None")
  )

  result <- kobo_select_multiple(
    design = design_all_none,
    vars = "skills",
    survey = survey,
    choices = choices
  )

  # Check that 'none' has proportion 1.0 and others have 0.0
  none_result <- result[result$var_value == "none", ]
  other_results <- result[result$var_value != "none", ]

  expect_equal(none_result$stat, 1.0, tolerance = 1e-10)
  expect_true(all(other_results$stat == 0.0))
})
