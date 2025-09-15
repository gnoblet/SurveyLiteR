# Tests for auto_kobo_analysis function

# Helper function to create test data for auto_kobo_analysis
create_auto_kobo_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create sample survey data with various variable types
  data <- data.frame(
    # Numeric variables (for mean/median analysis)
    age = round(rnorm(n, mean = 35, sd = 10)),
    income = round(rnorm(n, mean = 50000, sd = 15000)),
    score = rnorm(n, mean = 75, sd = 12),
    rating = runif(n, min = 1, max = 5),

    # Select one variables
    education = sample(
      c("primary", "secondary", "university"),
      n,
      replace = TRUE
    ),
    satisfaction = sample(c("low", "medium", "high"), n, replace = TRUE),

    # Select multiple variables (with proper parent/child structure)
    `skills/programming` = sample(
      c(0, 1),
      n,
      replace = TRUE,
      prob = c(0.6, 0.4)
    ),
    `skills/data_analysis` = sample(
      c(0, 1),
      n,
      replace = TRUE,
      prob = c(0.7, 0.3)
    ),
    `skills/management` = sample(
      c(0, 1),
      n,
      replace = TRUE,
      prob = c(0.8, 0.2)
    ),

    # Grouping variables
    group_var = sample(c("GroupA", "GroupB"), n, replace = TRUE),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),

    # Survey design variables
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE),
    check.names = FALSE
  )

  # Create parent column for skills select_multiple
  skills_parent <- character(n)
  for (i in 1:n) {
    selected <- c()
    if (data$`skills/programming`[i] == 1) {
      selected <- c(selected, "programming")
    }
    if (data$`skills/data_analysis`[i] == 1) {
      selected <- c(selected, "data_analysis")
    }
    if (data$`skills/management`[i] == 1) {
      selected <- c(selected, "management")
    }
    skills_parent[i] <- if (length(selected) > 0) {
      paste(selected, collapse = " ")
    } else {
      NA_character_
    }
  }
  data$skills <- skills_parent

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create survey sheet
  survey <- data.table::data.table(
    type = c(
      "integer",
      "decimal",
      "calculate",
      "decimal",
      "select_one",
      "select_one",
      "select_multiple",
      "text"
    ),
    list_name = c(
      NA,
      NA,
      NA,
      NA,
      "education_list",
      "satisfaction_list",
      "skills_list",
      NA
    ),
    name = c(
      "age",
      "income",
      "score",
      "rating",
      "education",
      "satisfaction",
      "skills",
      "group_var"
    ),
    label = c(
      "Age",
      "Monthly Income",
      "Test Score",
      "Rating",
      "Education Level",
      "Satisfaction Level",
      "Skills",
      "Group Variable"
    )
  )

  # Create choices sheet
  choices <- data.table::data.table(
    list_name = c(
      rep("education_list", 3),
      rep("satisfaction_list", 3),
      rep("skills_list", 3)
    ),
    name = c(
      "primary",
      "secondary",
      "university",
      "low",
      "medium",
      "high",
      "programming",
      "data_analysis",
      "management"
    ),
    label = c(
      "Primary Education",
      "Secondary Education",
      "University Education",
      "Low",
      "Medium",
      "High",
      "Programming",
      "Data Analysis",
      "Management"
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
test_that("auto_kobo_analysis works with basic inputs", {
  test_data <- create_auto_kobo_test_data()

  result <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Should return a data.table
  expect_s3_class(result, "data.table")

  # Should have required columns
  expect_true(all(c("stat_type", "var", "stat") %in% colnames(result)))

  # Should include all stat types (both select_one and select_multiple return "proportion")
  expected_stat_types <- c("median", "mean", "proportion")
  expect_true(all(expected_stat_types %in% result$stat_type))

  # Should include all analysis types in the analysis column
  expected_analysis_types <- c(
    "median",
    "mean",
    "select_one",
    "select_multiple"
  )
  expect_true(all(expected_analysis_types %in% result$analysis))

  # Should analyze numeric variables
  numeric_vars <- c("age", "income", "score", "rating")
  expect_true(all(numeric_vars %in% result$var))

  # Should analyze select_one variables
  select_one_vars <- c("education", "satisfaction")
  expect_true(all(select_one_vars %in% result$var))

  # Should analyze select_multiple variables
  expect_true("skills" %in% result$var)
})

test_that("auto_kobo_analysis works with grouping", {
  test_data <- create_auto_kobo_test_data()

  result <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    group = "group_var"
  )

  # Should return a data.table
  expect_s3_class(result, "data.table")

  # Should have group columns
  expect_true("group_var" %in% colnames(result))
  expect_true("group_key" %in% colnames(result))

  # Should have both group levels
  expect_true(all(c("GroupA", "GroupB") %in% result$group_var))

  # Check that we have results for both groups
  expect_true(length(unique(result$group_key)) >= 1)
})

test_that("auto_kobo_analysis works with multiple grouping variables", {
  test_data <- create_auto_kobo_test_data()

  result <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    group = c("group_var", "region")
  )

  # Should return a data.table
  expect_s3_class(result, "data.table")

  # Should have both group columns
  expect_true(all(c("group_var", "region", "group_key") %in% colnames(result)))

  # Group key should contain the separator
  expect_true(all(grepl(" -/- ", result$group_key)))
})

test_that("auto_kobo_analysis works with custom group_key_sep", {
  test_data <- create_auto_kobo_test_data()

  result <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    group = c("group_var", "region"),
    group_key_sep = " | "
  )

  # Should use custom separator
  expect_true(all(grepl(" \\| ", result$group_key)))
  expect_false(any(grepl(" -/- ", result$group_key)))
})

test_that("auto_kobo_analysis handles na_rm parameter", {
  # Skip on platforms with known survey package CI issues
  if (Sys.info()["sysname"] == "Darwin") {
    skip("Skipping on macOS due to platform-specific survey package CI issues")
  }

  test_data <- create_auto_kobo_test_data()

  # Add some NAs to the data
  test_data$data$age[1:5] <- NA
  test_data$data$education[1:3] <- NA
  design_with_nas <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  # Test with na_rm = TRUE (default)
  result_rm <- auto_kobo_analysis(
    design = design_with_nas,
    survey = test_data$survey,
    choices = test_data$choices,
    na_rm = TRUE
  )

  # Test with na_rm = FALSE
  result_keep <- auto_kobo_analysis(
    design = design_with_nas,
    survey = test_data$survey,
    choices = test_data$choices,
    na_rm = FALSE
  )

  # Both should return data.tables
  expect_s3_class(result_rm, "data.table")
  expect_s3_class(result_keep, "data.table")

  # Results should be different (though we can't easily test specific values)
  expect_true(nrow(result_rm) > 0)
  expect_true(nrow(result_keep) > 0)
})

test_that("auto_kobo_analysis works with different confidence levels", {
  test_data <- create_auto_kobo_test_data()

  result_95 <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    level = 0.95
  )

  result_90 <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    level = 0.90
  )

  # Should have confidence interval columns
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_95)))
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_90)))

  # 90% CI should be narrower than 95% CI (for the same variable)
  age_mean_95 <- result_95[stat_type == "mean" & var == "age"][1]
  age_mean_90 <- result_90[stat_type == "mean" & var == "age"][1]

  ci_width_95 <- age_mean_95$stat_upp - age_mean_95$stat_low
  ci_width_90 <- age_mean_90$stat_upp - age_mean_90$stat_low

  expect_true(ci_width_90 < ci_width_95)
})

test_that("auto_kobo_analysis works with different vartypes", {
  test_data <- create_auto_kobo_test_data()

  # Test with confidence intervals (default)
  result_ci <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    vartype = "ci"
  )
  expect_true(all(c("stat_low", "stat_upp") %in% colnames(result_ci)))

  # Test with standard errors
  result_se <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    vartype = "se"
  )
  expect_true("stat_se" %in% colnames(result_se))
  expect_false(all(c("stat_low", "stat_upp") %in% colnames(result_se)))
})

test_that("auto_kobo_analysis bind parameter works correctly", {
  test_data <- create_auto_kobo_test_data()

  # Test with bind = TRUE (default)
  result_bound <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    bind = TRUE
  )
  expect_s3_class(result_bound, "data.table")

  # Test with bind = FALSE
  result_list <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    bind = FALSE
  )
  expect_type(result_list, "list")
  expect_length(result_list, 4) # num_median, num_mean, select_ones, select_multiples

  # Each element should be a data.table
  expect_true(all(sapply(result_list, function(x) "data.table" %in% class(x))))
})

test_that("auto_kobo_analysis handles custom choices_sep", {
  test_data <- create_auto_kobo_test_data()

  result <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    choices_sep = "/"
  )

  # Should work without error
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("auto_kobo_analysis handles label_survey parameter", {
  test_data <- create_auto_kobo_test_data()

  # Test with labels
  result_labeled <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = TRUE
  )

  # Test without labels
  result_unlabeled <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = FALSE
  )

  # Both should work
  expect_s3_class(result_labeled, "data.table")
  expect_s3_class(result_unlabeled, "data.table")

  # Should have different column structures potentially
  expect_true(nrow(result_labeled) > 0)
  expect_true(nrow(result_unlabeled) > 0)
})

test_that("auto_kobo_analysis validates inputs correctly", {
  test_data <- create_auto_kobo_test_data()

  # Test invalid design
  expect_error(
    auto_kobo_analysis(
      design = "not_a_design",
      survey = test_data$survey,
      choices = test_data$choices
    ),
    "Must inherit from class"
  )

  # Test invalid group variable
  expect_error(
    auto_kobo_analysis(
      design = test_data$design,
      survey = test_data$survey,
      choices = test_data$choices,
      group = "nonexistent_var"
    ),
    "The following variables from group are missing: nonexistent_var"
  )

  # Test invalid level
  expect_error(
    auto_kobo_analysis(
      design = test_data$design,
      survey = test_data$survey,
      choices = test_data$choices,
      level = 1.5
    ),
    "Element 1 is not <= 1"
  )

  expect_error(
    auto_kobo_analysis(
      design = test_data$design,
      survey = test_data$survey,
      choices = test_data$choices,
      level = -0.1
    ),
    "Element 1 is not >= 0"
  )

  # Test invalid vartype
  expect_error(
    auto_kobo_analysis(
      design = test_data$design,
      survey = test_data$survey,
      choices = test_data$choices,
      vartype = "invalid"
    ),
    "should be one of"
  )
})

test_that("auto_kobo_analysis handles edge cases", {
  # Test with only numeric variables
  data_numeric <- data.frame(
    numeric_var1 = rnorm(50),
    numeric_var2 = runif(50),
    weights = rep(1, 50)
  )
  design_numeric <- srvyr::as_survey_design(data_numeric, weights = weights)

  survey_numeric <- data.table::data.table(
    type = c("decimal", "decimal"),
    list_name = c(NA, NA),
    name = c("numeric_var1", "numeric_var2"),
    label = c("Numeric Variable 1", "Numeric Variable 2")
  )

  choices_minimal <- data.table::data.table(
    list_name = character(0),
    name = character(0),
    label = character(0)
  )

  result_numeric <- auto_kobo_analysis(
    design = design_numeric,
    survey = survey_numeric,
    choices = choices_minimal
  )
  expect_s3_class(result_numeric, "data.table")
  expect_true(all(result_numeric$analysis %in% c("mean", "median")))

  # Test with only select_one variables
  data_categorical <- data.frame(
    cat_var1 = sample(c("A", "B"), 50, replace = TRUE),
    cat_var2 = sample(c("X", "Y", "Z"), 50, replace = TRUE),
    weights = rep(1, 50)
  )
  design_categorical <- srvyr::as_survey_design(
    data_categorical,
    weights = weights
  )

  survey_categorical <- data.table::data.table(
    type = c("select_one", "select_one"),
    list_name = c("list1", "list2"),
    name = c("cat_var1", "cat_var2"),
    label = c("Categorical Variable 1", "Categorical Variable 2")
  )

  choices_categorical <- data.table::data.table(
    list_name = c("list1", "list1", "list2", "list2", "list2"),
    name = c("A", "B", "X", "Y", "Z"),
    label = c("Option A", "Option B", "Option X", "Option Y", "Option Z")
  )

  result_categorical <- auto_kobo_analysis(
    design = design_categorical,
    survey = survey_categorical,
    choices = choices_categorical
  )
  expect_s3_class(result_categorical, "data.table")
  expect_true(all(result_categorical$analysis == "select_one"))
})

test_that("auto_kobo_analysis produces consistent results", {
  test_data <- create_auto_kobo_test_data()

  # Run the same analysis twice
  result1 <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  result2 <- auto_kobo_analysis(
    design = test_data$design,
    survey = test_data$survey,
    choices = test_data$choices
  )

  # Results should be identical
  expect_equal(result1, result2)

  # Check that means are reasonable for our test data
  age_mean <- result1[stat_type == "mean" & var == "age"]$stat
  expect_true(age_mean > 25 && age_mean < 45) # Should be around 35

  income_mean <- result1[stat_type == "mean" & var == "income"]$stat
  expect_true(income_mean > 40000 && income_mean < 60000) # Should be around 50000
})

test_that("auto_kobo_analysis respects survey sheet structure", {
  test_data <- create_auto_kobo_test_data()

  # Create a survey sheet with missing variables
  partial_survey <- test_data$survey[1:4, ] # Only first 4 variables

  result <- auto_kobo_analysis(
    design = test_data$design,
    survey = partial_survey,
    choices = test_data$choices
  )

  # Should only analyze variables present in the survey sheet
  survey_vars <- partial_survey$name
  result_vars <- unique(result$var)

  # All result variables should be in the survey sheet
  expect_true(all(result_vars %in% survey_vars))

  # Should have both mean and median for numeric variables in the survey
  numeric_in_survey <- partial_survey[
    type %in% c("integer", "decimal", "calculate")
  ]$name
  for (var in numeric_in_survey) {
    expect_true(any(result$stat_type == "mean" & result$var == var))
    expect_true(any(result$stat_type == "median" & result$var == var))
  }
})
