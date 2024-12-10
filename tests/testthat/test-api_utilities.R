library(testthat)

# Ensure the functions are defined in the package for mocking to work
claude_single_request <- NULL
claude_default_content_extraction <- NULL
claude_default_response_validation <- NULL

claude_special_content_extraction <- NULL
claude_special_response_validation <- NULL

test_that("get_default_model works correctly", {
  # Skip on CRAN
  skip_on_cran()

  # Check the default model for various APIs and types
  expect_equal(get_default_model("groq", "cheapest"), "llama-3.1-8b-instant")
  expect_equal(get_default_model("groq", "largest"), "llama-3.2-90b-text-preview")
  expect_equal(get_default_model("claude", "best"), "claude-3-5-sonnet-20241022")
})

test_that("get_available_models works correctly", {
  # Skip on CRAN
  skip_on_cran()

  # Check available models for a specific API
  available_models <- get_available_models("groq")
  expect_true("llama3-8b-8192" %in% available_models)
  expect_true("llama3-70b-8192" %in% available_models)

  # Check all available models
  all_models <- get_available_models()
  expect_true(length(all_models) > 0)
})

test_that("is_api_key_available works correctly", {
  # Skip on CRAN
  skip_on_cran()

  # Skip on CI, including Github
  skip_on_ci()

  # Skip on GitHub Actions
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    skip("Skipping tests on GitHub Actions")
  }

  # Check the function returns TRUE for available keys
  expect_true(is_api_key_available("groq"))
  expect_true(is_api_key_available("claude"))
  expect_true(is_api_key_available("openai"))
  expect_true(is_api_key_available("gemini"))
})

test_that("get_available_apis works correctly", {
  # Skip on CRAN
  skip_on_cran()

  # Skip on CI, including Github
  skip_on_ci()

  # Skip on GitHub Actions
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    skip("Skipping tests on GitHub Actions")
  }

  # Check the function returns correct API availability
  available_apis <- get_available_apis()
  expect_true(all(c("groq", "claude") %in% available_apis$api))
  expect_true(available_apis$key_available[available_apis$api == "groq"])
  expect_true(available_apis$key_available[available_apis$api == "claude"])
  expect_true(available_apis$key_available[available_apis$api == "openai"])
  expect_true(available_apis$key_available[available_apis$api == "gemini"])
})

test_that("resolve_functions uses defaults when prompt_name is missing", {
  local_mocked_bindings(
    claude_single_request = function(...) "mocked_request",
    claude_default_content_extraction = function(...) "default_extraction"
  )

  functions <- resolve_functions("claude")

  expect_equal(functions$single_request_fun(), "mocked_request")
  expect_equal(functions$content_extraction_fun(), "default_extraction")
})


