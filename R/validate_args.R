validate_args_build_prompt_from_file <- function(files,
                                                 roles,
                                                 api,
                                                 data) {
  checkmate::assert_file(files)
  checkmate::assert_subset(roles, c("user", "system", "assistant"))
  checkmate::assert_choice(api, choices = c("groq", "openai", "claude", "gemini", "local_llamafile"))
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

validate_args_call_api <- function(prompts,
                                   model,
                                   prompt_name,
                                   n_candidates,
                                   max_retries,
                                   temperature,
                                   max_tokens,
                                   json_mode,
                                   system,
                                   pause_cap,
                                   log,
                                   llamafile_path) {

  # Extract the api name from the prompts
  api <- class(prompts)[1]

  # Validate prompts - must be one of the supported API classes
  checkmate::assert_multi_class(prompts, classes = c("groq", "openai", "claude", "gemini", "local_llamafile"))

  # Validate model if provided
  if (!missing(model) && !"local_llamafile" %in% class(prompts) ) {
    checkmate::assert_scalar(model)
    checkmate::assert_choice(model, models_df$model[models_df$api == api])
  } else if (!missing(model) && "local_llamafile" %in% class(prompts) ) {
    checkmate::assert_scalar(model)
    checkmate::assert_choice(model, "LLaMA_CPP")
  }

  # Validate prompt_name if provided
  if (!missing(prompt_name)) {
    checkmate::assert_scalar(prompt_name)
  }

  # Validate n_candidates - must be a positive integer. Can only be 1 for groq
  if(api == "groq") {
    checkmate::assert_int(n_candidates, lower = 1, upper = 1)
  } else {
    checkmate::assert_count(n_candidates, positive = TRUE)
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


  # Validate json_mode - must be a logical scalar
  checkmate::assert_logical(json_mode, len = 1)

  # Validate system message if provided
  if (!is.null(system)) {
    checkmate::assert_scalar(system)
  }

  # Validate pause_cap - must be a non-negative numeric value
  checkmate::assert_number(pause_cap, lower = 0)

  # Validate log - must be a logical scalar
  checkmate::assert_logical(log, len = 1)

  # Valudate llamafile path if provided
  if (!missing(llamafile_path)) {
    checkmate::assert_file_exists(llamafile_path)
  }
}

