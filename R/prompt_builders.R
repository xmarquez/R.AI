#' Build Prompts from Files
#'
#' This function constructs prompts for various language model APIs by reading
#' from specified files and combining them according to the chosen API's
#' requirements.
#'
#' @param files A character vector of file paths containing prompt content.
#'   All files must exist.
#' @param roles A character vector specifying the roles for each file.
#'   Must be one or more of "system", "user", or "assistant". Default is "user".
#'   If multiple roles are provided, the length must match the number of files.
#'   Only one "system" role is allowed.
#' @param api A string specifying the API to use. Must be one of "groq", "openai",
#'   "claude", "mistral", "llamafile", or "gemini".
#' @param data An optional data frame containing variables to be
#'   interpolated into the prompts using `stringr::str_glue_data()`.
#'
#' @return A list of combined prompts, with class matching the specified API and
#'   names corresponding to the row names of the input data.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates input arguments using `validate_args_build_prompt_from_file()`.
#' 2. Constructs function names for prompt building based on the API and roles.
#' 3. Reads and processes each file using the appropriate prompt builder function.
#' 4. Combines the processed prompts using an API-specific combined prompt builder.
#' 5. Sets the class and names of the resulting combined prompts.
#'
#' The function supports different prompt structures for various APIs:
#' - For OpenAI, Groq, and Claude: Can handle "system", "user", and "assistant" roles.
#' - For Gemini: Only uses "user" role prompts.
#'
#' Input validation ensures that:
#' - All specified files exist.
#' - Roles are valid and correctly specified (e.g., only one "system" role).
#' - The API choice is valid.
#' - If provided, the data argument is a data frame.
#'
#' @examples
#' \dontrun{
#' files <- c("system_prompt.txt", "user_prompt.txt")
#' roles <- c("system", "user")
#' api <- "openai"
#' data <- data.frame(variable1 = c("value1", "value2"))
#'
#' prompts <- build_prompt_from_files(files, roles, api, data)
#' }
#'
#' @seealso
#' [call_api()] for using the generated prompts to make API calls.
#'
#' @export
build_prompts_from_files <- function(files,
                                     roles = "user",
                                     api,
                                     data) {
  tryCatch({
    validate_args_build_prompt_from_file(files, roles, api, data)

    if(missing(data)) {
      data <- NULL
    }

    prompts_list <- purrr::map(files, function(x) {
      tryCatch(
        default_prompt_builder(x, data),
        error = function(e) {
          cli::cli_abort("Error processing file {x}: {conditionMessage(e)}")
        }
      )
    })

    names(prompts_list) <- roles

    prompts_df <- tibble::as_tibble(prompts_list)

    combined_prompt_fun <- paste(api, "combined_prompt_builder", sep = "_")

    combined_prompts <- do.call(combined_prompt_fun, list(prompts_df))

    class(combined_prompts) <- c(api, class(combined_prompts))

    if (!is.null(data) && nrow(data) > 0) {
      names(combined_prompts) <- rownames(data)
    } else {
      names(combined_prompts) <- seq_along(combined_prompts)
    }

    combined_prompts
  }, error = function(e) {
    cli::cli_abort("Error in build_prompts_from_files: {conditionMessage(e)}")
  })
}

default_combined_prompt_builder <- function(prompts_df) {
  # Define valid roles and their order
  valid_roles <- c("system", "user", "assistant")

  # Check if prompts_df is empty
  if (ncol(prompts_df) == 0) {
    cli::cli_abort("The prompts_df must contain at least one column.")
  }

  # Ensure at least one user prompt exists
  if (!"user" %in% names(prompts_df)) {
    cli::cli_abort("There must be at least one user prompt in prompts_df.")
  }

  # Sort columns based on the order of valid_roles
  prompts_df <- prompts_df[, order(match(names(prompts_df), valid_roles))]

  # Create the final prompts
  final_prompts <- purrr::pmap(prompts_df, function(...) {
    parts <- list(...)
    roles <- names(parts)

    # Create a list of role-content pairs, filtering out NULL values
    role_content_pairs <- purrr::map2(roles, parts, ~ list(role = .x, content = .y)) |>
      purrr::compact()

    # Sort the pairs based on the order of valid_roles
    sorted_pairs <- role_content_pairs[order(match(sapply(role_content_pairs, `[[`, "role"), valid_roles))]

    sorted_pairs
  })

  # Add a class to the final_prompts for potential method dispatch
  class(final_prompts) <- c("combined_prompts", class(final_prompts))

  final_prompts
}

# Helper function to print combined_prompts objects
#' @export
print.combined_prompts <- function(x, ...) {
  cat("Combined Prompts:\n")
  purrr::walk(x, function(prompt) {
    cat("\nPrompt:\n")
    purrr::walk(prompt, function(part) {
      cat(sprintf("  Role: %s\n  Content: %s\n\n", part$role, substr(part$content, 1, 50)))
    })
  })
  invisible(x)
}

groq_combined_prompt_builder <- function(prompts_df) {
  default_combined_prompt_builder(prompts_df)
}

openai_combined_prompt_builder <- function(prompts_df) {
  default_combined_prompt_builder(prompts_df)
}

mistral_combined_prompt_builder <- function(prompts_df) {
  default_combined_prompt_builder(prompts_df)
}

llamafile_combined_prompt_builder <- function(prompts_df) {
  default_combined_prompt_builder(prompts_df)
}

claude_combined_prompt_builder <- function(prompts_df) {
  default_combined_prompt_builder(prompts_df)
}

gemini_combined_prompt_builder <- function(prompts_df) {
  purrr::pmap(prompts_df, function(...) {
    parts <- list(...)
    purrr::imap(parts, function(content, role) {
      list(
        role = ifelse(role == "assistant", "model", role),
        parts = list(list(text = content))
      )
    })
  }) |>
    purrr::flatten()
}

default_prompt_builder <- function(prompt_filename, data) {
  prompt_text <- readr::read_lines(prompt_filename) |>
    paste(collapse = "\n")

  if(!missing(data) && !is.null(data)) {
    prompts <- stringr::str_glue_data(data, prompt_text)
  } else {
    prompts <- prompt_text
  }

  prompts
}
