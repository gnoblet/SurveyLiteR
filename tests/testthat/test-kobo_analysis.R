# Tests for kobo_analysis function

# Helper function to create comprehensive test data for kobo_analysis
create_kobo_analysis_test_data <- function() {
  set.seed(123)
  n <- 100

  # Create sample survey data with various variable types
  data <- data.table::data.table(
    # Numeric variables for mean/median analysis
    age = round(rnorm(n, mean = 35, sd = 10)),
    income = round(rnorm(n, mean = 50000, sd = 15000)),
    score = rnorm(n, mean = 75, sd = 12),
    expenses = round(rnorm(n, mean = 30000, sd = 10000)),
    household_size = round(runif(n, min = 1, max = 8)),

    # Select_one variables
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    education = sample(
      c("Primary", "Secondary", "University"),
      n,
      replace = TRUE
    ),
    employment = sample(
      c("Employed", "Unemployed", "Student"),
      n,
      replace = TRUE
    ),
    satisfaction = sample(c("Low", "Medium", "High"), n, replace = TRUE),

    # Select_multiple variables (binary flags)
    skills_communication = sample(c(0, 1), n, replace = TRUE),
    skills_leadership = sample(c(0, 1), n, replace = TRUE),
    skills_technical = sample(c(0, 1), n, replace = TRUE),

    # Interaction variables
    var1 = sample(c("A", "B", "C"), n, replace = TRUE),
    var2 = sample(c("X", "Y"), n, replace = TRUE),

    # Grouping variables
    survey_round = sample(c("Round1", "Round2"), n, replace = TRUE),
    gender = sample(c("Male", "Female"), n, replace = TRUE),

    # Survey design variables
    weights = runif(n, min = 0.5, max = 2.0),
    strata = sample(c("Stratum1", "Stratum2", "Stratum3"), n, replace = TRUE)
  )

  # Ensure no zero denominators for ratio analysis
  data$household_size[data$household_size == 0] <- 1

  # Create survey design
  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  # Create comprehensive survey sheet
  survey <- data.table::data.table(
    type = c(
      # Numeric types
      "integer",
      "decimal",
      "decimal",
      "decimal",
      "integer",
      # Select_one types
      "select_one",
      "select_one",
      "select_one",
      "select_one",
      # Select_multiple base question
      "select_multiple",
      # Interaction variables
      "select_one",
      "select_one"
    ),
    list_name = c(
      # Numeric types
      NA,
      NA,
      NA,
      NA,
      NA,
      # Select_one types
      "regions",
      "education_levels",
      "employment_status",
      "satisfaction_levels",
      # Select_multiple
      "skills",
      # Interaction variables
      "options1",
      "options2"
    ),
    name = c(
      "age",
      "income",
      "score",
      "expenses",
      "household_size",
      "region",
      "education",
      "employment",
      "satisfaction",
      "skills",
      "var1",
      "var2"
    ),
    label = c(
      "Age in years",
      "Annual income",
      "Test score",
      "Monthly expenses",
      "Household size",
      "Geographic region",
      "Education level",
      "Employment status",
      "Satisfaction level",
      "Skills assessment",
      "Variable 1",
      "Variable 2"
    )
  )

  # Create comprehensive choices sheet
  choices <- data.table::data.table(
    list_name = c(
      # Regions
      "regions",
      "regions",
      "regions",
      "regions",
      # Education levels
      "education_levels",
      "education_levels",
      "education_levels",
      # Employment status
      "employment_status",
      "employment_status",
      "employment_status",
      # Satisfaction levels
      "satisfaction_levels",
      "satisfaction_levels",
      "satisfaction_levels",
      # Skills (for select_multiple)
      "skills",
      "skills",
      "skills",
      # Interaction options
      "options1",
      "options1",
      "options1",
      "options2",
      "options2"
    ),
    name = c(
      "North",
      "South",
      "East",
      "West",
      "Primary",
      "Secondary",
      "University",
      "Employed",
      "Unemployed",
      "Student",
      "Low",
      "Medium",
      "High",
      "communication",
      "leadership",
      "technical",
      "A",
      "B",
      "C",
      "X",
      "Y"
    ),
    label = c(
      "Northern Region",
      "Southern Region",
      "Eastern Region",
      "Western Region",
      "Primary Education",
      "Secondary Education",
      "University Education",
      "Employed Full-time",
      "Unemployed",
      "Student",
      "Low Satisfaction",
      "Medium Satisfaction",
      "High Satisfaction",
      "Communication Skills",
      "Leadership Skills",
      "Technical Skills",
      "Option A",
      "Option B",
      "Option C",
      "Choice X",
      "Choice Y"
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
create_minimal_analysis_test_data <- function() {
  set.seed(456)
  n <- 50

  data <- data.table::data.table(
    numeric_var = rnorm(n, mean = 100, sd = 20),
    select_var = sample(c("Type1", "Type2"), n, replace = TRUE),
    num_ratio = rnorm(n, mean = 50, sd = 10),
    denom_ratio = abs(rnorm(n, mean = 10, sd = 3)) + 1,
    interact1 = sample(c("A", "B"), n, replace = TRUE),
    interact2 = sample(c("X", "Y"), n, replace = TRUE),
    group_var = sample(c("G1", "G2"), n, replace = TRUE),
    weights = rep(1, n),
    strata = rep("S1", n)
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c(
      "decimal",
      "select_one",
      "decimal",
      "decimal",
      "select_one",
      "select_one"
    ),
    list_name = c(NA, "types", NA, NA, "letters", "letters2"),
    name = c(
      "numeric_var",
      "select_var",
      "num_ratio",
      "denom_ratio",
      "interact1",
      "interact2"
    ),
    label = c(
      "Numeric Variable",
      "Select Variable",
      "Numerator",
      "Denominator",
      "First Interact",
      "Second Interact"
    )
  )

  choices <- data.table::data.table(
    list_name = c(
      "types",
      "types",
      "letters",
      "letters",
      "letters2",
      "letters2"
    ),
    name = c("Type1", "Type2", "A", "B", "X", "Y"),
    label = c(
      "First Type",
      "Second Type",
      "Letter A",
      "Letter B",
      "Letter X",
      "Letter Y"
    )
  )

  list(
    data = data,
    design = design,
    survey = survey,
    choices = choices
  )
}

# Test basic functionality for each analysis type
test_that("kobo_analysis works with mean analysis", {
  test_data <- create_kobo_analysis_test_data()

  result <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = "age",
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "mean")
  expect_equal(unique(result$stat_type), "mean")
  expect_equal(nrow(result), 1)
  expect_true("stat" %in% colnames(result))
  expect_true("var_label" %in% colnames(result))
  expect_equal(result$var_label, "Age in years")
})

test_that("kobo_analysis works with median analysis", {
  test_data <- create_kobo_analysis_test_data()

  result <- kobo_analysis(
    design = test_data$design,
    analysis = "median",
    vars = "score",
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "median")
  expect_equal(unique(result$stat_type), "median")
  expect_equal(nrow(result), 1)
  expect_true("stat" %in% colnames(result))
  expect_equal(result$var_label, "Test score")
})

test_that("kobo_analysis works with select_one analysis", {
  test_data <- create_kobo_analysis_test_data()

  result <- kobo_analysis(
    design = test_data$design,
    analysis = "select_one",
    vars = "region",
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "select_one")
  expect_equal(unique(result$stat_type), "proportion")
  expect_true(nrow(result) <= 4) # Max 4 regions
  expect_true("stat" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result))
  expect_true(any(grepl("Northern Region", result$var_value_label)))
})

test_that("kobo_analysis works with select_multiple analysis", {
  test_data <- create_kobo_analysis_test_data()

  skills_names <- c('communication', 'leadership', 'technical')
  test_data$data <- add_select_multiple_parent(
    dt = test_data$data,
    parent_var = "skills",
    child_vars = c(
      "skills_communication",
      "skills_leadership",
      "skills_technical"
    ),
    choices = skills_names,
    choices_sep = "/",
    sep = " "
  )

  # Recreate design with new skills variable
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result <- kobo_analysis(
    design = test_data$design,
    analysis = "select_multiple",
    vars = "skills",
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "select_multiple")
  expect_equal(unique(result$stat_type), "mean")
  expect_true(nrow(result) >= 3) # At least 3 skills
  expect_true("stat" %in% colnames(result))
  expect_true("var_value_label" %in% colnames(result))
})

test_that("kobo_analysis works with ratio analysis", {
  test_data <- create_kobo_analysis_test_data()

  # Create named vector for ratio analysis
  ratio_vars <- c("income" = "household_size")

  result <- kobo_analysis(
    design = test_data$design,
    analysis = "ratio",
    vars = ratio_vars,
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "ratio")
  expect_equal(unique(result$stat_type), "ratio")
  expect_equal(nrow(result), 1)
  expect_true("stat" %in% colnames(result))
  expect_true(grepl("income.*household_size", result$var))
})

test_that("kobo_analysis works with interact analysis", {
  test_data <- create_kobo_analysis_test_data()

  result <- kobo_analysis(
    design = test_data$design,
    analysis = "interact",
    vars = c("var1", "var2"),
    survey = test_data$survey,
    choices = test_data$choices
  )

  expect_s3_class(result, "data.table")
  expect_equal(unique(result$analysis), "interact")
  expect_equal(unique(result$stat_type), "proportion")
  expect_true(nrow(result) > 0)
  expect_true("interact_key" %in% colnames(result))
  expect_true("interact_key_value" %in% colnames(result))
})

# Test multiple variables
test_that("kobo_analysis works with multiple variables", {
  test_data <- create_kobo_analysis_test_data()

  result <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = c("age", "income", "score"),
    survey = test_data$survey
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_setequal(unique(result$var), c("age", "income", "score"))
  expect_equal(unique(result$analysis), "mean")
})

# Test grouping functionality
test_that("kobo_analysis works with grouping", {
  test_data <- create_kobo_analysis_test_data()

  result <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = "age",
    survey = test_data$survey,
    group = "gender"
  )

  expect_s3_class(result, "data.table")
  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_setequal(unique(result$group_key_value), c("Male", "Female"))
})

test_that("kobo_analysis works with multiple grouping variables", {
  test_data <- create_kobo_analysis_test_data()

  result <- kobo_analysis(
    design = test_data$design,
    analysis = "select_one",
    vars = "region",
    survey = test_data$survey,
    choices = test_data$choices,
    group = c("gender", "survey_round")
  )

  expect_s3_class(result, "data.table")
  expect_true("group_key" %in% colnames(result))
  expect_true("group_key_value" %in% colnames(result))
  expect_true(all(grepl("gender.*survey_round", result$group_key)))
})

# Test auto_group_remove functionality
test_that("kobo_analysis auto_group_remove works correctly", {
  test_data <- create_kobo_analysis_test_data()

  # Test with auto_group_remove = TRUE (default)
  expect_warning(
    result_auto <- kobo_analysis(
      design = test_data$design,
      analysis = "mean",
      vars = c("age", "gender"), # gender is also in group
      survey = test_data$survey,
      group = "gender",
      auto_group_remove = TRUE
    ),
    "The analysis is not run for 'vars': gender"
  )

  expect_s3_class(result_auto, "data.table")
  expect_equal(unique(result_auto$var), "age") # Only age should remain

  # Test with auto_group_remove = FALSE
  expect_error(
    kobo_analysis(
      design = test_data$design,
      analysis = "mean",
      vars = c("age", "gender"),
      survey = test_data$survey,
      group = "gender",
      auto_group_remove = FALSE
    )
  )
})

test_that("kobo_analysis auto_group_remove handles all vars in group", {
  test_data <- create_kobo_analysis_test_data()

  expect_warning(
    result <- kobo_analysis(
      design = test_data$design,
      analysis = "mean",
      vars = "gender", # Only variable is also in group
      survey = test_data$survey,
      group = "gender",
      auto_group_remove = TRUE
    ),
    "All 'vars' are in 'group'. An empty data.table is returned."
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

# Test parameter validation
test_that("kobo_analysis validates analysis type", {
  test_data <- create_minimal_analysis_test_data()

  expect_error(
    kobo_analysis(
      design = test_data$design,
      analysis = "invalid_analysis",
      vars = "numeric_var",
      survey = test_data$survey
    ),
    "Please provide an analysis from the following list"
  )
})

test_that("kobo_analysis requires choices for select_multiple", {
  test_data <- create_kobo_analysis_test_data()

  expect_error(
    kobo_analysis(
      design = test_data$design,
      analysis = "select_multiple",
      vars = "skills",
      survey = test_data$survey,
      choices = NULL
    ),
    "For the 'select multiple' analysis type, please provide the choices sheet"
  )
})

# Test different parameter options
test_that("kobo_analysis handles different vartype options", {
  test_data <- create_minimal_analysis_test_data()

  vartypes <- c("ci", "se", "var", "cv")

  for (vt in vartypes) {
    result <- kobo_analysis(
      design = test_data$design,
      analysis = "mean",
      vars = "numeric_var",
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

test_that("kobo_analysis handles different confidence levels", {
  test_data <- create_minimal_analysis_test_data()

  result_95 <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = "numeric_var",
    survey = test_data$survey,
    level = 0.95
  )

  result_99 <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = "numeric_var",
    survey = test_data$survey,
    level = 0.99
  )

  # 99% CI should be wider than 95% CI
  ci_width_95 <- result_95$stat_upp - result_95$stat_low
  ci_width_99 <- result_99$stat_upp - result_99$stat_low

  expect_true(ci_width_99 >= ci_width_95)
})

test_that("kobo_analysis handles NA removal options", {
  test_data <- create_minimal_analysis_test_data()

  # Add some NAs
  test_data$data$numeric_var[1:5] <- NA
  test_data$design <- srvyr::as_survey_design(
    test_data$data,
    weights = weights,
    strata = strata
  )

  result_rm <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = "numeric_var",
    survey = test_data$survey,
    na_rm = TRUE
  )

  result_keep <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = "numeric_var",
    survey = test_data$survey,
    na_rm = FALSE
  )

  expect_s3_class(result_rm, "data.table")
  expect_s3_class(result_keep, "data.table")
  # Results should be different due to NA handling
  expect_false(identical(result_rm$stat, result_keep$stat))
})

# Test custom separators
test_that("kobo_analysis handles custom separators", {
  test_data <- create_minimal_analysis_test_data()

  # Test ratio with custom separator
  ratio_vars <- c("num_ratio" = "denom_ratio")
  result_ratio <- kobo_analysis(
    design = test_data$design,
    analysis = "ratio",
    vars = ratio_vars,
    survey = test_data$survey,
    ratio_key_sep = " | "
  )

  expect_true(grepl("num_ratio.*\\|.*denom_ratio", result_ratio$var))

  # Test interact with custom separator
  result_interact <- kobo_analysis(
    design = test_data$design,
    analysis = "interact",
    vars = c("interact1", "interact2"),
    survey = test_data$survey,
    choices = test_data$choices,
    interact_key_sep = " & "
  )

  expect_true(all(grepl(
    "interact1.*&.*interact2",
    result_interact$interact_key
  )))
})

test_that("kobo_analysis handles labeling options", {
  test_data <- create_minimal_analysis_test_data()

  # Test without variable labels
  result_no_labels <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = "numeric_var",
    survey = test_data$survey,
    label_survey = FALSE
  )

  expect_s3_class(result_no_labels, "data.table")
  expect_false("var_label" %in% colnames(result_no_labels))

  # Test without choice labels for select_one
  result_no_choice_labels <- kobo_analysis(
    design = test_data$design,
    analysis = "select_one",
    vars = "select_var",
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = FALSE
  )

  expect_s3_class(result_no_choice_labels, "data.table")
  expect_false("var_label" %in% colnames(result_no_choice_labels))
  expect_true("var_value_label" %in% colnames(result_no_choice_labels))
})

# Test edge cases
test_that("kobo_analysis handles empty variable list gracefully", {
  test_data <- create_minimal_analysis_test_data()

  expect_error(
    kobo_analysis(
      design = test_data$design,
      analysis = "mean",
      vars = character(0),
      survey = test_data$survey
    )
  )
})

test_that("kobo_analysis handles single observation", {
  # Create data with just one observation (multiple for survey design)
  data <- data.frame(
    numeric_var = c(100, 110, 90),
    select_var = c("Type1", "Type1", "Type1"),
    weights = c(1, 1, 1),
    strata = c("S1", "S1", "S1")
  )

  design <- srvyr::as_survey_design(data, weights = weights, strata = strata)

  survey <- data.table::data.table(
    type = c("decimal", "select_one"),
    list_name = c(NA, "types"),
    name = c("numeric_var", "select_var"),
    label = c("Numeric Variable", "Select Variable")
  )

  choices <- data.table::data.table(
    list_name = "types",
    name = "Type1",
    label = "First Type"
  )

  result_mean <- kobo_analysis(
    design = design,
    analysis = "mean",
    vars = "numeric_var",
    survey = survey
  )

  result_select <- kobo_analysis(
    design = design,
    analysis = "select_one",
    vars = "select_var",
    survey = survey,
    choices = choices
  )

  expect_s3_class(result_mean, "data.table")
  expect_s3_class(result_select, "data.table")
  expect_equal(nrow(result_mean), 1)
  expect_equal(nrow(result_select), 1)
})

# Test consistency with underlying functions
test_that("kobo_analysis produces consistent results with underlying functions", {
  test_data <- create_minimal_analysis_test_data()

  # Test mean consistency
  result_analysis <- kobo_analysis(
    design = test_data$design,
    analysis = "mean",
    vars = "numeric_var",
    survey = test_data$survey,
    label_survey = FALSE
  )

  result_direct <- kobo_mean(
    design = test_data$design,
    vars = "numeric_var",
    survey = test_data$survey,
    label_survey = FALSE
  )

  expect_equal(result_analysis$stat, result_direct$stat, tolerance = 1e-10)

  # Test select_one consistency
  result_analysis_select <- kobo_analysis(
    design = test_data$design,
    analysis = "select_one",
    vars = "select_var",
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = FALSE
  )

  result_direct_select <- kobo_select_one(
    design = test_data$design,
    vars = "select_var",
    survey = test_data$survey,
    choices = test_data$choices,
    label_survey = FALSE
  )

  expect_equal(
    result_analysis_select$stat,
    result_direct_select$stat,
    tolerance = 1e-10
  )
})

test_that("kobo_analysis maintains analysis column correctly", {
  test_data <- create_kobo_analysis_test_data()

  # Test that analysis column is added correctly for each type
  # Note: mean and median show as "numeric" in the analysis column
  analyses <- list(
    list(type = "mean", expected = "mean"),
    list(type = "median", expected = "median"),
    list(type = "select_one", expected = "select_one"),
    list(type = "interact", expected = "interact")
  )

  for (analysis_info in analyses) {
    analysis_type <- analysis_info$type
    expected_analysis <- analysis_info$expected

    if (analysis_type == "select_one") {
      vars <- "region"
      choices <- test_data$choices
    } else if (analysis_type == "interact") {
      vars <- c("var1", "var2")
      choices <- test_data$choices
    } else {
      vars <- "age"
      choices <- NULL
    }

    result <- kobo_analysis(
      design = test_data$design,
      analysis = analysis_type,
      vars = vars,
      survey = test_data$survey,
      choices = choices
    )

    expect_true("analysis" %in% colnames(result))
    expect_equal(unique(result$analysis), expected_analysis)
  }
})
