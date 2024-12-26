validate_args_build_prompt_from_file <- function(files,
                                                 roles,
                                                 api,
                                                 data) {
  checkmate::assert_file(files)
  checkmate::assert_subset(roles, c("user", "system", "assistant"))
  checkmate::assert_choice(api, choices = c("groq", "openai", "claude", "gemini", "llamafile", "mistral"))
  if(length(roles) != 1 && length(roles) != length(files)) {
    cli::cli_abort("{.var roles} must be of length one or the same length as {.var file}.")
  }
  if("system" %in% roles && sum(roles == "system") > 1) {
    cli::cli_abort("{.var roles} must only contain one system prompt.")
  }
  if(!missing(data) && !checkmate::test_data_frame(data)) {
    cli::cli_abort("The {.var data} argument must be a data frame.")
  }
}

validate_single_request <- function(prompts, model, prompt_name, ...) {


  args <- list(...)
  llamafile_path <- args$llamafile_path
  n_candidates <- args$n_candidates
  max_retries <- args$max_retries %||% 10
  temperature <- args$temperature %||% 0.2
  max_tokens <- args$max_tokens %||% 300
  system <- args$system
  pause_cap <- args$pause_cap %||% 1200
  quiet <- args$quiet %||% FALSE

  api <- class(prompts)[1]

  # Validate prompt_name if provided
  if (!missing(prompt_name)) {
    checkmate::assert_string(prompt_name)
  }

  # Validate n_candidates - must be a positive integer. Can only be 1 for groq
  if(!is.null(n_candidates)) {
    if(api == "groq") {
      checkmate::assert_int(n_candidates, lower = 1, upper = 1)
    }
    if(api %in% c("openai", "mistral")) {
      checkmate::assert_count(n_candidates, positive = TRUE)
    } else {
      cli::cli_warn("{.var n_candidates} is not implemented in the {api} API.")
    }
  }

  # Validate max_retries - must be a non-negative integer
  checkmate::assert_int(max_retries, lower = 1)

  # Validate temperature - must be between 0 and 2
  checkmate::assert_number(temperature, lower = 0, upper = 2, finite = TRUE)

  # Validate max_tokens - must be a positive integer
  model_max_tokens <- models_df$max_output_tokens[models_df$model == model]
  if(length(model_max_tokens) > 0) {
    if(!checkmate::check_int(max_tokens, lower = 1, upper = model_max_tokens)) {
      cli::cli_abort("{.var max_tokens} for model {models_df$model} must be ",
                     "an integer between 1 and {model_max_tokens}")
    }
  }

  # Validate system message if provided
  if (!is.null(system)) {
    checkmate::assert_string(system)
  }

  # Validate pause_cap - must be a non-negative numeric value
  checkmate::assert_number(pause_cap, lower = 0)

  # Validate quiet - must be a logical scalar
  checkmate::assert_logical(quiet, len = 1)
}

