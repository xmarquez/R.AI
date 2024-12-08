library(testthat)

test_that("call_api works correctly for Groq API", {
  # Skip on CRAN
  skip_on_cran()

  # Skip on CI
  skip_on_ci()

  # Skip on GitHub Actions
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    skip("Skipping tests on GitHub Actions")
  }

  # Skip if offline
  skip_if_offline()

  # Define file paths for the test prompts
  groq_system_file <- test_path("test-prompts/groq_system.md")
  groq_user_file <- test_path("test-prompts/groq_user.md")

  # Ensure the test files exist
  expect_true(file.exists(groq_system_file))
  expect_true(file.exists(groq_user_file))

  # Define the roles and API
  roles <- c("system", "user")
  api <- "groq"
  num_prompts <- 10

  # Create a test data frame
  test_data <- dplyr::tibble(country = unique(countrycode::codelist$country.name.en)[1:num_prompts])

  # Create test prompts using build_prompts_from_files
  prompts <- build_prompts_from_files(
    files = c(groq_system_file, groq_user_file),
    roles = roles,
    api = api,
    data = test_data
  )

  # Define model and other parameters
  model <- "gemma-7b-it"
  temperature <- 0.2
  max_tokens <- 300

  # Call the API
  responses <- call_api(prompts, model = model, temperature = temperature, max_tokens = max_tokens)
  responses <- call_api(prompts, model = model, prompt_name = "test_prompt", temperature = temperature, max_tokens = max_tokens)


  # Check the structure of the returned responses
  expect_true(is.data.frame(responses))
  expect_equal(nrow(responses), num_prompts)
  expect_true("id" %in% names(responses))
  expect_true("api" %in% names(responses))
  expect_true("model" %in% names(responses))
  expect_true("response" %in% names(responses))
  expect_type(responses$response, "list")
})

test_that("call_api works correctly for OpenAI API", {
  # Skip on CRAN
  skip_on_cran()

  # Skip on CI
  skip_on_ci()

  # Skip on GitHub Actions
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    skip("Skipping tests on GitHub Actions")
  }

  # Skip if offline
  skip_if_offline()

  # Define file paths for the test prompts
  openai_system_file <- test_path("test-prompts/groq_system.md")
  openai_user_file <- test_path("test-prompts/groq_user.md")

  # Ensure the test files exist
  expect_true(file.exists(openai_system_file))
  expect_true(file.exists(openai_user_file))

  # Define the roles and API
  roles <- c("system", "user")
  api <- "openai"
  num_prompts <- 24

  # Create a test data frame
  test_data <- dplyr::tibble(country = unique(countrycode::codelist$country.name.en)[1:num_prompts])

  # Create test prompts using build_prompts_from_files
  prompts <- build_prompts_from_files(
    files = c(openai_system_file, openai_user_file),
    roles = roles,
    api = api,
    data = test_data
  )

  # Define model and other parameters
  model <- get_default_model(api)
  temperature <- 0.7
  max_tokens <- 500

  # Call the API
  responses <- call_api(prompts, model = model, temperature = temperature, max_tokens = max_tokens)

  # Check the structure of the returned responses
  expect_true(is.data.frame(responses))
  expect_equal(nrow(responses), num_prompts)
  expect_true("id" %in% names(responses))
  expect_true("api" %in% names(responses))
  expect_true("model" %in% names(responses))
  expect_true("response" %in% names(responses))
  expect_type(responses$response, "list")
})

test_that("call_api works correctly for Claude API", {
  # Skip on CRAN
  skip_on_cran()

  # Skip on CI
  skip_on_ci()

  # Skip on GitHub Actions
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    skip("Skipping tests on GitHub Actions")
  }

  # Skip if offline
  skip_if_offline()

  # Define file paths for the test prompts
  claude_user_file <- test_path("test-prompts/claude_user.md")

  # Ensure the test files exist
  expect_true(file.exists(claude_user_file))

  # Define the roles and API
  roles <- c("user")
  api <- "claude"
  num_prompts <- 10

  # Create a test data frame
  test_data <- dplyr::tibble(country = unique(countrycode::codelist_panel$country.name.en)[1:num_prompts])

  # Create test prompts using build_prompts_from_files
  prompts <- build_prompts_from_files(
    files = c(claude_user_file),
    roles = roles,
    api = api,
    data = test_data
  )

  # Define model and other parameters
  model <- get_default_model(api)
  temperature <- 0.2
  max_tokens <- 300

  # Call the API
  responses <- call_api(prompts, model = model, temperature = temperature, max_tokens = max_tokens)

  # Check the structure of the returned responses
  expect_true(is.data.frame(responses))
  expect_equal(nrow(responses), num_prompts)
  expect_true("id" %in% names(responses))
  expect_true("api" %in% names(responses))
  expect_true("model" %in% names(responses))
  expect_true("response" %in% names(responses))
  expect_type(responses$response, "list")
})

test_that("call_api works correctly for Gemini API", {
  # Skip on CRAN
  skip_on_cran()

  # Skip on CI
  skip_on_ci()

  # Skip on GitHub Actions
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    skip("Skipping tests on GitHub Actions")
  }

  # Skip if offline
  skip_if_offline()

  # Define file paths for the test prompts
  gemini_user_file <- test_path("test-prompts/gemini_user.md")

  # Ensure the test files exist
  expect_true(file.exists(gemini_user_file))

  # Define the roles and API
  roles <- c("user")
  api <- "gemini"
  num_prompts <- 10

  # Create a test data frame
  test_data <- dplyr::tibble(country = unique(countrycode::codelist_panel$country.name.en)[1:num_prompts])

  # Create test prompts using build_prompts_from_files
  prompts <- build_prompts_from_files(
    files = c(gemini_user_file),
    roles = roles,
    api = api,
    data = test_data
  )

  # Define model and other parameters
  model <- get_default_model(api)
  temperature <- 0.2
  max_tokens <- 300

  # Call the API
  responses <- call_api(prompts, model = model, temperature = temperature, max_tokens = max_tokens)

  # Check the structure of the returned responses
  expect_true(is.data.frame(responses))
  expect_equal(nrow(responses), num_prompts)
  expect_true("id" %in% names(responses))
  expect_true("api" %in% names(responses))
  expect_true("model" %in% names(responses))
  expect_true("response" %in% names(responses))
  expect_type(responses$response, "list")
})
