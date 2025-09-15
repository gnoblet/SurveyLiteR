# Tests for kobo_select_multiple_all function

# Helper function to create test data for kobo_select_multiple_all
create_kobo_select_multiple_all_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create skills data with none option
  skills_communication <- sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3))
  skills_leadership <- sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4))
  skills_technical <- sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2))

  # Create 'none' variable for skills: 1 when all other skills are 0
  skills_none <- ifelse(
    skills_communication == 0 & skills_leadership == 0 & skills_technical == 0,
    1,
    0
  )

  # Create languages data with none option
  languages_english <- sample(c(0, 1), n, replace = TRUE, prob = c(0.4, 0.6))
  languages_french <- sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2))
  languages_spanish <- sample(c(0, 1), n, replace = TRUE, prob = c(0.9, 0.1))

  # Create 'none' variable for languages
  languages_none <- ifelse(
    languages_english == 0 & languages_french == 0 & languages_spanish == 0,
    1,
    0
  )

  # Create hobbies data with none option
  hobbies_reading <- sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5))
  hobbies_sports <- sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3))
  hobbies_music <- sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4))

  # Create 'none' variable for hobbies
  hobbies_none <- ifelse(
    hobbies_reading == 0 & hobbies_sports == 0 & hobbies_music == 0,
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

  hobbies_parent <- character(n)
  for (i in 1:n) {
    selected <- c()
    if (hobbies_reading[i] == 1) {
      selected <- c(selected, "reading")
    }
    if (hobbies_sports[i] == 1) {
      selected <- c(selected, "sports")
    }
    if (hobbies_music[i] == 1) {
      selected <- c(selected, "music")
    }
    if (hobbies_none[i] == 1) {
      selected <- c(selected, "none")
    }
    hobbies_parent[i] <- if (length(selected) > 0) {
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

    # Hobbies select_multiple question (parent + child columns)
    hobbies = hobbies_parent,
    `hobbies/reading` = hobbies_reading,
    `hobbies/sports` = hobbies_sports,
    `hobbies/music` = hobbies_music,
    `hobbies/none` = hobbies_none,

    # Other variables
    q1_select_one = sample(c("yes", "no"), n, replace = TRUE),
    q2_numeric = rnorm(n),
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
    type = c(
      "select_multiple",
      "select_multiple",
      "select_multiple",
      "select_one",
      "integer"
    ),
    list_name = c(
      "skills_list",
      "languages_list",
      "hobbies_list",
      "yesno_list",
      NA
    ),
    name = c(
      "skills",
      "languages",
      "hobbies",
      "q1_select_one",
      "q2_numeric"
    ),
    label = c(
      "What skills do you have?",
      "What languages do you speak?",
      "What are your hobbies?",
      "Question 1",
      "Question 2"
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
      "hobbies_list",
      "hobbies_list",
      "hobbies_list",
      "hobbies_list",
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
      "reading",
      "sports",
      "music",
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
      "Reading",
      "Sports",
      "Music",
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
test_that("kobo_select_multiple_all works with all select_multiple variables", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "select_multiple")

  # Should have all three select_multiple variables
  expect_setequal(unique(result$var), c("skills", "languages", "hobbies"))

  # Total rows should be sum of all choice options (4 + 4 + 4 = 12)
  expect_equal(nrow(result), 12)

  # Check required columns exist
  expect_true("stat" %in% colnames(result))
  expect_true("stat_low" %in% colnames(result))
  expect_true("stat_upp" %in% colnames(result))
  expect_true("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result))

  # Check that all proportions are between 0 and 1
  expect_true(all(result$stat >= 0 & result$stat <= 1))

  # Check that each variable has the expected choices including 'none'
  skills_results <- result[result$var == "skills", ]
  expect_setequal(
    skills_results$var_value,
    c("communication", "leadership", "technical", "none")
  )

  languages_results <- result[result$var == "languages", ]
  expect_setequal(
    languages_results$var_value,
    c("english", "french", "spanish", "none")
  )

  hobbies_results <- result[result$var == "hobbies", ]
  expect_setequal(
    hobbies_results$var_value,
    c("reading", "sports", "music", "none")
  )
})

test_that("kobo_select_multiple_all works with grouping", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    group = "group_var"
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_equal(unique(result$group_key), "group_var")
  expect_setequal(unique(result$group_key_value), c("GroupA", "GroupB"))

  # Check that each group has results for all variables and choices
  for (grp in c("GroupA", "GroupB")) {
    grp_data <- result[result$group_key_value == grp, ]
    expect_setequal(unique(grp_data$var), c("skills", "languages", "hobbies"))
    expect_true(all(grp_data$stat >= 0 & grp_data$stat <= 1))
  }
})

test_that("kobo_select_multiple_all works with multiple grouping variables", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    group = c("group_var", "group_var2")
  )

  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_equal(unique(result$group_key), "group_var -/- group_var2")

  # Check that group_key_value contains the separator
  expect_true(any(grepl(" -/- ", result$group_key_value)))

  # Check that each group combination has results for all variables
  unique_groups <- unique(result$group_key_value)
  for (grp in unique_groups) {
    grp_data <- result[result$group_key_value == grp, ]
    expect_setequal(unique(grp_data$var), c("skills", "languages", "hobbies"))
  }
})

test_that("kobo_select_multiple_all works without survey labels", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = FALSE
  )

  expect_false("var_label" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result)) # Should still have choice labels
})

test_that("kobo_select_multiple_all works without choice labels", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    label_choices = FALSE
  )

  expect_true("var_label" %in% colnames(result))
  expect_false("var_value_label" %in% colnames(result))
})

test_that("kobo_select_multiple_all handles different variance types", {
  test_data <- create_kobo_select_multiple_all_test_data()

  # Test confidence intervals
  result_ci <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    vartype = "ci"
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))
  expect_false("stat_se" %in% colnames(result_ci))

  # Test standard errors
  result_se <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    vartype = "se"
  )
  expect_true("stat_se" %in% colnames(result_se))
  expect_false(any(c("stat_low", "stat_upp") %in% colnames(result_se)))

  # Test variances
  result_var <- kobo_select_multiple_all(
    design = test_data$design,
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

test_that("kobo_select_multiple_all handles different confidence levels", {
  test_data <- create_kobo_select_multiple_all_test_data()

  levels <- c(0.90, 0.95, 0.99)

  for (level in levels) {
    result <- kobo_select_multiple_all(
      design = test_data$design,
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

test_that("kobo_select_multiple_all handles NA values correctly", {
  test_data <- create_kobo_select_multiple_all_test_data()

  # Add some NA values to child columns
  test_data$data$`skills/communication`[1:10] <- NA
  test_data$data$`languages/english`[5:15] <- NA

  design_with_na <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result_na_rm <- kobo_select_multiple_all(
    design = design_with_na,
    survey = test_data$survey,
    choices = test_data$choices,
    na_rm = TRUE
  )

  expect_setequal(unique(result_na_rm$var), c("skills", "languages", "hobbies"))
  expect_true(all(result_na_rm$stat >= 0 & result_na_rm$stat <= 1))

  result_na_keep <- kobo_select_multiple_all(
    design = design_with_na,
    survey = test_data$survey,
    choices = test_data$choices,
    na_rm = FALSE
  )

  expect_setequal(
    unique(result_na_keep$var),
    c("skills", "languages", "hobbies")
  )
  expect_true(all(result_na_keep$stat >= 0 & result_na_keep$stat <= 1))
})

test_that("kobo_select_multiple_all handles custom choices separator", {
  test_data <- create_kobo_select_multiple_all_test_data()

  # Rename columns to use different separator
  names(test_data$data) <- gsub("/", "_", names(test_data$data))

  design_custom_sep <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result <- kobo_select_multiple_all(
    design = design_custom_sep,
    survey = test_data$survey,
    choices = test_data$choices,
    choices_sep = "_"
  )

  expect_setequal(unique(result$var), c("skills", "languages", "hobbies"))
  expect_true(all(result$stat >= 0 & result$stat <= 1))
})

test_that("kobo_select_multiple_all handles survey with no select_multiple questions", {
  test_data <- create_kobo_select_multiple_all_test_data()

  # Create survey with only select_one and numeric questions
  survey_no_sm <- data.table::data.table(
    type = c("select_one", "integer"),
    list_name = c("yesno_list", NA),
    name = c("q1_select_one", "q2_numeric"),
    label = c("Question 1", "Question 2")
  )

  expect_warning(
    result <- kobo_select_multiple_all(
      design = test_data$design,
      survey = survey_no_sm,
      choices = test_data$choices
    ),
    "There are no vars of type select_multiple"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("kobo_select_multiple_all preserves variable labels correctly", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = TRUE
  )

  # Check variable labels
  skills_results <- result[result$var == "skills", ]
  languages_results <- result[result$var == "languages", ]
  hobbies_results <- result[result$var == "hobbies", ]

  expect_true(all(skills_results$var_label == "What skills do you have?"))
  expect_true(all(
    languages_results$var_label == "What languages do you speak?"
  ))
  expect_true(all(hobbies_results$var_label == "What are your hobbies?"))
})

test_that("kobo_select_multiple_all preserves choice labels correctly", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    label_choices = TRUE
  )

  # Check some specific choice labels
  none_results <- result[result$var_value == "none", ]
  expect_true(all(none_results$var_value_label == "None of the above"))

  communication_results <- result[
    result$var == "skills" & result$var_value == "communication",
  ]
  expect_true(all(communication_results$var_value_label == "Communication"))

  english_results <- result[
    result$var == "languages" & result$var_value == "english",
  ]
  expect_true(all(english_results$var_value_label == "English"))
})

test_that("kobo_select_multiple_all returns data.table", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
})

test_that("kobo_select_multiple_all var_value column is character", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_true(is.character(result$var_value))
  expect_false(is.factor(result$var_value))
})

test_that("kobo_select_multiple_all handles 'none' options correctly", {
  test_data <- create_kobo_select_multiple_all_test_data()

  result <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Check that 'none' option is included for all variables
  expect_true("none" %in% result$var_value)

  # Check that each variable has a 'none' option
  for (var_name in c("skills", "languages", "hobbies")) {
    var_results <- result[result$var == var_name, ]
    expect_true("none" %in% var_results$var_value)

    # Check that 'none' has proper label
    none_row <- var_results[var_results$var_value == "none", ]
    expect_equal(none_row$var_value_label, "None of the above")

    # Verify that the proportion for 'none' is reasonable
    expect_true(none_row$stat > 0)
    expect_true(none_row$stat < 1)
  }
})

test_that("kobo_select_multiple_all works with minimal survey", {
  # Create minimal test data with just one select_multiple question
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

  result <- kobo_select_multiple_all(
    design = design_minimal,
    survey = survey_minimal,
    choices = choices_minimal
  )

  expect_equal(nrow(result), 2)
  expect_equal(unique(result$var), "skills")
  expect_setequal(result$var_value, c("communication", "none"))
  expect_true(all(result$stat >= 0 & result$stat <= 1))
})

test_that("kobo_select_multiple_all validates input parameters", {
  test_data <- create_kobo_select_multiple_all_test_data()

  # Test with missing design
  expect_error(
    kobo_select_multiple_all(
      survey = test_data$survey,
      choices = test_data$choices
    ),
    "argument \"design\" is missing"
  )

  # Test with missing survey
  expect_error(
    kobo_select_multiple_all(
      design = test_data$design,
      choices = test_data$choices
    ),
    "argument \"survey\" is missing"
  )

  # Test with missing choices
  expect_error(
    kobo_select_multiple_all(
      design = test_data$design,
      survey = test_data$survey
    ),
    "argument \"choices\" is missing"
  )

  # Test with invalid vartype
  expect_error(
    kobo_select_multiple_all(
      design = test_data$design,
      survey = test_data$survey,
      choices = test_data$choices,
      vartype = "invalid"
    ),
    "'arg' should be one of"
  )

  # Test with invalid confidence level
  expect_error(
    kobo_select_multiple_all(
      design = test_data$design,
      survey = test_data$survey,
      choices = test_data$choices,
      level = 1.5
    ),
    "Element 1 is not <= 1"
  )
})

test_that("kobo_select_multiple_all consistent results with kobo_select_multiple", {
  test_data <- create_kobo_select_multiple_all_test_data()

  # Get results from kobo_select_multiple_all
  result_all <- kobo_select_multiple_all(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Get results from individual kobo_select_multiple calls
  result_individual <- kobo_select_multiple(
    design = test_data$design,
    vars = c("skills", "languages", "hobbies"),
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Results should be identical (after sorting for comparison)
  data.table::setorder(result_all, var, var_value)
  data.table::setorder(result_individual, var, var_value)

  expect_equal(result_all$var, result_individual$var)
  expect_equal(result_all$var_value, result_individual$var_value)
  expect_equal(result_all$stat, result_individual$stat, tolerance = 1e-10)
})
