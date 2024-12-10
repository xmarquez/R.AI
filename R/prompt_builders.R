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
#' @seealso
#' [call_api()] for using the generated prompts to make API calls.
#'
#' @family prompts
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

#' Create Lists of Prompts for API Requests
#'
#' This function creates structured prompt lists for use with [call_api()]. It
#' processes input `user` content and combines it with optional `system`
#' instructions, returning a list of prompts compatible with the specified API.
#'
#' @param user A character vector of user prompts or file paths containing the
#'   user content. If file paths are provided, the files are read, and their
#'   content is concatenated into a single prompt. If the package
#'   [rtika](https://cran.r-project.org/package=rtika) is installed and rtika is
#'   `TRUE`, the function will read all kinds of content, including PDF files.
#'   Otherwise it will default to [readr::read_lines()].
#' @param api A string specifying the API to use. Must be one of `"gemini"`,
#'   `"openai"`, `"claude"`, `"llamafile"`, or `"mistral"`.
#' @param system Optional. A character string providing system-level
#'   instructions or context for the API.
#' @param data Optional. A data frame containing variables to be interpolated
#'   into the `user` content using `stringr::str_glue_data()`.
#' @param rtika Whether to use the
#'   [rtika](https://cran.r-project.org/package=rtika) package to read files.
#'   Must be installed separately - and it contains a Java dependency. Default
#'   is `FALSE`.
#'
#' @return A list of structured prompts for use with [call_api()]. The
#'   exact structure of each prompt depends on the specified API.
#'
#' @details This function is designed for sending multiple requests to an API,
#'   allowing the user to specify multiple input prompts. It reads and processes
#'   file-based user content if file paths are provided and ensures
#'   compatibility with the target API.
#'
#' @seealso
#' - [prompt()] for creating a single prompt for single-request API functions.
#' - [build_prompts_from_files()] for another way of constructing prompts from file-based content.
#'
#' @family prompts
#' @examples
#' # Create list of prompts for multiple requests to the Gemini API
#' prompts <- prompt_list(
#'   user = c("What is the capital of France?", "Explain quantum computing."),
#'   api = "gemini",
#'   system = "You are a knowledgeable assistant."
#' )
#' print(prompts)
#' @export
prompt_list <- function(user, api, system, data, rtika = FALSE) {
  checkmate::assert_character(user, min.len = 1)
  checkmate::assert_choice(api, c("gemini", "openai", "claude", "llamafile", "mistral", "groq"))

  user <- process_user_prompts(user, rtika)

  if(!missing(system)) {
    checkmate::assert_string(system)
  }
  if(!missing(data)) {
    checkmate::assert_data_frame(data)
  }

  if(missing(data)) {
    data <- NULL
  }

  if(missing(data)) {
    data <- NULL
  }

  user <- text_prompt_builder(user, data)

  if(!missing(system)) {
    prompts_df <- tibble::tibble(user = user, system = system)
  } else {
    prompts_df <- tibble::tibble(user = user)
  }

  combined_prompt_fun <- paste(api, "combined_prompt_builder", sep = "_")

  combined_prompts <- do.call(combined_prompt_fun, list(prompts_df))

  class(combined_prompts) <- c(api, class(combined_prompts))

  if (!is.null(data) && nrow(data) > 0) {
    names(combined_prompts) <- rownames(data)
  } else {
    names(combined_prompts) <- seq_along(combined_prompts)
  }

  combined_prompts
}

#' Create a Single Prompt for API Requests
#'
#' This function creates a single prompt for use with single-request API
#' functions. It processes the input `user` content and combines it with
#' optional `system` instructions, returning a structured prompt compatible with
#' the specified API.
#'
#' @param user A character string containing the user's input prompt.
#' @param api A string specifying the API to use. Must be one of `"gemini"`,
#'   `"openai"`, `"claude"`, `"groq"`, `"llamafile"`, or `"mistral"`.
#' @param system Optional. A character string providing system-level
#'   instructions or context for the API.
#' @param data Optional. A data frame containing variables to be interpolated
#'   into the `user` content using `stringr::str_glue_data()`.
#'
#' @return A structured prompt for use with single-request API functions like
#'   [openai_single_request()] or [claude_single_request()]. The exact structure
#'   depends on the specified API.
#'
#' @details This function processes a single user's input prompt along with
#'   optional system instructions, ensuring compatibility with the target API.
#'   It is designed for single-request functions like `openai_single_request`,
#'   `gemini_single_request`, and `claude_single_request`.
#'
#' @seealso
#' - [prompt_list()] for creating prompt lists for use with [call_api()].
#' - [call_api()] for executing API calls with generated prompts.
#'
#' @examples
#' # Create a single prompt for the OpenAI API
#' single_prompt <- prompt(
#'   user = "What are the benefits of exercise?",
#'   api = "openai",
#'   system = "You are a helpful assistant."
#' )
#' print(single_prompt)
#'
#' @family prompts
#' @export
prompt <- function(user, api, system, data) {
  checkmate::assert_string(user)
  res <- prompt_list(user, api, system, data)[[1]]
  class(res) <- c(api, class(res))
  res
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

  system_prompt <- NULL
  if("system" %in% names(prompts_df)) {
    system_prompt <- unique(prompts_df$system)
  }
  prompts_df$system <- NULL

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

    # Add system attribute
    attr(sorted_pairs, "system") <- system_prompt

    sorted_pairs
  })

  # Add a class to the final_prompts for potential method dispatch
  class(final_prompts) <- c("combined_prompts", class(final_prompts))

  final_prompts
}

gemini_combined_prompt_builder <- function(prompts_df) {
  system_prompt <- NULL
  if("system" %in% names(prompts_df)) {
    system_prompt <- unique(prompts_df$system)
  }
  prompts_df$system <- NULL
  final_prompts <- purrr::pmap(prompts_df, function(...) {
    parts <- list(...)
    purrr::imap(parts, function(content, role) {
      list(
        role = ifelse(role == "assistant", "model", role),
        parts = list(text = content)
      )
    })
  }) |>
    purrr::flatten() |>
    purrr::map(\(x) {
      if(!is.null(system_prompt)) {
        attr(x, "system") <- list(text = system_prompt)
      }
      x})
  class(final_prompts) <- c("combined_prompts", class(final_prompts))
  final_prompts
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

text_prompt_builder <- function(prompt_text, data) {

  if(!missing(data) && !is.null(data)) {
    prompts <- stringr::str_glue_data(data, prompt_text)
  } else {
    prompts <- prompt_text
  }

  prompts
}

process_user_prompts <- function(user, rtika = FALSE) {
  # Validate the input and separate files from text
  file_indices <- fs::file_exists(user)
  files <- user[file_indices]
  texts <- user[!file_indices]

  # Process files
  if (length(files) > 0) {
    cli::cli_alert_info("Found file prompts: {files}. Reading them now.")
    if (length(files) > 1) {
      cli::cli_warn("Multiple files detected. Files will be concatenated into a single user prompt.")
    }
    file_contents <- if (rtika) {
      if (!requireNamespace("rtika", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg rtika} is required to use this option.")
      } else {
        file_contents <- rtika::tika_text(files)
      }
    } else {
      file_contents <- purrr::map(files, readr::read_lines) |>
        purrr::map_chr(paste, collapse = "\n")
    }
  } else {
    file_contents <- character(0)
  }

  # Combine file contents with text inputs
  combined_user <- c(texts, file_contents)

  combined_user[c(which(!file_indices), which(file_indices))] |>
    paste(collapse = "\n")
}
