validate_args_build_prompt_from_file <- function(files,
                                                 roles,
                                                 api,
                                                 data) {
  checkmate::assert_file(files)
  checkmate::assert_subset(roles, c("user", "system", "assistant"))
  checkmate::assert_choice(api, choices = c("groq", "openai", "claude", "gemini"))
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
                                   rate_limit,
                                   max_retries,
                                   temperature,
                                   max_tokens,
                                   json_mode,
                                   system) {
  checkmate::assert_multi_class(prompts, classes = c("groq", "openai", "claude", "gemini"))
  if(!missing(model)) {
    checkmate::assert_scalar(model)
    checkmate::assert_choice(model, models_df$model[models_df$api == class(prompts)[1]])
  }
  if(!missing(prompt_name)) {
    checkmate::assert_scalar(prompt_name)
  }
  checkmate::assert_number(rate_limit, lower = 0, finite = TRUE)
  checkmate::assert_number(max_retries, lower = 1, finite = TRUE)
  checkmate::assert_number(temperature, lower = 0, finite = TRUE)
  checkmate::assert_number(max_tokens, lower = 1, finite = TRUE)
  checkmate::assert_logical(json_mode, len = 1)
  if(!is.null(system)) {
    checkmate::assert_scalar(system)
  }


}

