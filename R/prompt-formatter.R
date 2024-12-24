#' Format Chat Prompts for Different Backends
#'
#' @description `format_chat()` is a generic function that takes user/system
#'   inputs, plus optionally some caching parameters or data, and returns a
#'   structured list of messages suitable for a specific backend (OpenAI,
#'   Claude, Gemini, etc.).
#'
#'   This function normalizes inputs (like reading local files, inlining images,
#'   or embedding PDFs) and produces the final message format expected by each
#'   model's API. The exact structure depends on the S3 method:
#'
#' - **`format_chat.default()`**: A baseline fallback, typically for OpenAI-like
#'   prompt structures.
#' - **`format_chat.claude()`**: Structures text, images, or documents for
#'   Anthropic Claude, including optional ephemeral caching (`cache_control`).
#' - **`format_chat.gemini()`**: Structures text or file references for Google
#'   Gemini, optionally extracting some user messages for caching via `cache`.
#'
#' @param api A character string or an object indicating which backend is used,
#'   e.g. `"openai"`, `"claude"`, `"gemini"`, etc. This is typically set
#'   automatically by the calling functions or by a `class()` on the object.
#' @param user A character vector of user inputs. May be file paths, URLs, or
#'   direct text. Internally, [process_user_prompts()] attempts to convert
#'   images and PDFs into base64 or text, depending on the backend capabilities.
#' @param system (Optional) A string containing the "system" instructions or
#'   context. Some backends (e.g. OpenAI, Claude) treat system prompts as a
#'   separate role. Others (e.g. Gemini) do so differently.
#' @param data (Optional) A data frame containing fields to be templated into
#'   user prompts. If non-empty, each row is used to glue data into `user`
#'   content with [stringr::str_glue_data()].
#' @param rtika Logical. If `TRUE`, and a file is a PDF, attempts to parse text
#'   from PDF using `rtika::tika_text()`. Defaults to `FALSE`.
#' @param quiet Logical. If `FALSE`, shows messages about file reading or PDF
#'   parsing. Defaults to `TRUE`.
#' @param cache For the **Claude** method, a numeric vector specifying which
#'   prompts should get `cache_control = "ephemeral"`. For the **Gemini**
#'   method, a numeric vector of row indices to extract as cached content. If
#'   `NULL`, no caching is applied.
#' @param cache_name For the **Gemini** method, an optional string used as a
#'   display name for the cache. If none is provided, an auto-generated name is
#'   used (e.g. a hash from [digest::digest()]).
#' @param ... Additional arguments (not currently used).
#'
#' @details
#' **Default Method**
#' The default method is a fallback for an “OpenAI-like” structure, creating
#' simple role/content pairs. Any PDFs in `user` become text (file content) and
#' any images remain as an "image_url" block (this includes image urls or image
#' files, which are automatically encoded in base64).
#'
#' **Claude Method**
#' Accepts numeric `cache` indices, marking those rows in the user’s prompt for
#' ephemeral caching with `cache_control = list(type = "ephemeral")`. Produces a
#' list of messages in Anthropic’s `messages` format, each containing a `role`
#' plus `content` with optional ephemeral blocks.
#'
#' **Gemini Method**
#' Splits user prompts into “final” vs. “cache” blocks if their row index is
#' listed in `cache`. The cached blocks are attached as an attribute named
#' `"cache"` on the returned list, so that subsequent calls to
#' [chat.gemini_list()] can create or retrieve the cache. For inline data, PDFs,
#' or local images, these are encoded into `inlineData` or `fileData`.
#'
#' @return A list (or nested list) structured to match the target API’s
#'   expectations. The resulting object typically has a class of the form
#'   `c("{api}_list", "list")` or `c(api, "combined_prompts", "list")`. This
#'   object is then passed to the corresponding `chat.*` function for actual API
#'   calls.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default method:
#' msgs <- format_chat("openai", user = "Hello world")
#'
#' # With Claude ephemeral caching:
#' claude_msgs <- format_chat("claude",
#'   user = c("First user prompt", "Second user prompt"),
#'   cache = 2
#' )
#'
#' # With Gemini caching
#' gemini_msgs <- format_chat("gemini",
#'   user = c("Prompt 1", "Prompt 2"),
#'   cache = 1
#' )
#' }
#'
#' @seealso [chat()] for sending the resulting structured prompts to the
#'   selected backend, and the content documentation for
#'   [OpenAI](https://platform.openai.com/docs/api-reference/chat/create),
#'   [Anthropic](https://docs.anthropic.com/claude/api-reference), and [Google
#'   Gemini](https://ai.google.dev/api/).
#'
#' @export
format_chat <- function(api, user, system, data, rtika, quiet, ...) {
  class(api) <- c(api, class(api))
  UseMethod("format_chat", api)
}

#' @rdname format_chat
#' @exportS3Method
format_chat.default <- function(api, user, data, system, rtika = FALSE, quiet = TRUE, ...) {

  if(missing(system)) {
    system <- NULL
  } else {
    checkmate::assert_string(system)
  }
  if(missing(data)) {
    data <- dplyr::tibble()
  }

  prompts_df <- process_user_prompts(user, rtika, quiet)

  if(nrow(data) > 0) {
    prompts_df_list <- glue_data(prompts_df, data)
  } else {
    prompts_df_list <- list(prompts_df)
  }

  final_prompts <- prompts_df_list |>
    purrr::map(\(x) block_structure_openai(x, api = api, system = system))

  if(nrow(data) > 0) {
    final_prompts <- structure(final_prompts, class = c(api, "combined_prompts",  class(final_prompts)))
  } else {
    final_prompts <- final_prompts[[1]]
  }

  final_prompts

}

#' @rdname format_chat
#' @exportS3Method
format_chat.claude <- function(api, user, data, system, cache, rtika = FALSE, quiet = TRUE, ...) {
  if (!missing(system)) {
    checkmate::assert_string(system)
  } else {
    system <- NULL
  }

  if (missing(data)) {
    data <- dplyr::tibble()
  }

  # -- Process user prompts into a tibble of roles & content
  prompts_df <- process_user_prompts(user, rtika, quiet)



  # -- Mark which rows in prompts_df will get cache_control = "ephemeral"
  #    Here, we assume `cache` is a numeric vector of row indices (1-based).
  #    For example, if cache = c(1, 3), then row 1 and row 3 in prompts_df
  #    will receive the ephemeral cache_control block.
  if (!missing(cache) && length(cache) > 0) {
    prompts_df <- prompts_df |>
      dplyr::mutate(
        .cache_control = dplyr::if_else(
          dplyr::row_number() %in% cache,
          "ephemeral",  # set ephemeral cache
          NA_character_ # otherwise no cache
        )
      )
  } else {
    # No caching requested
    prompts_df <- prompts_df |>
      dplyr::mutate(.cache_control = NA_character_)
  }

  if(nrow(data) > 0) {
    prompts_df_list <- glue_data(prompts_df, data)
  } else {
    prompts_df_list <- list(prompts_df)
  }

  final_prompts <- prompts_df_list |>
    purrr::map(\(x) block_structure_claude(x, api = api, system = system))

  # If no data-glue, system can be stored as an attribute on the top-level list
  if (!is.null(system) && nrow(data) == 0) {
    attr(final_prompts, "system") <- system
  }

  # Wrap everything in the appropriate structure/class
  if (nrow(data) > 0) {
    final_prompts <- structure(
      final_prompts,
      class = c(api, "combined_prompts", class(final_prompts))
    )
  } else {
    final_prompts <- final_prompts[[1]]
  }

  final_prompts
}

#' @rdname format_chat
#' @exportS3Method
#' @rdname format_chat
#' @exportS3Method
format_chat.gemini <- function(api,
                               user,
                               data,
                               system,
                               cache = NULL,      # row indices to extract as cache
                               cache_name = NULL, # optional: a friendly name
                               rtika = FALSE,
                               quiet = TRUE,
                               ...) {

  # Validate system
  if (!missing(system)) {
    checkmate::assert_string(system)
  } else {
    system <- NULL
  }

  # Default to empty tibble if data missing
  if (missing(data)) {
    data <- dplyr::tibble()
  }

  # 1) Process user prompts into a tibble
  prompts_df <- process_user_prompts(user, rtika, quiet)

  # 2) Tag each row with row_index **before** any data expansions
  prompts_df <- prompts_df %>%
    dplyr::mutate(row_index = dplyr::row_number())

  # 3) If user supplied data, glue each row -> multiple expanded tibbles
  if (nrow(data) > 0) {
    prompts_df_list <- glue_data(prompts_df, data)
  } else {
    # Always use a list, even if single tibble
    prompts_df_list <- list(prompts_df)
  }

  # 4) For each tibble, build Gemini blocks
  #    `block_structure_gemini()` returns a *list of row_index + blocks*
  splitted_prompts_list <- purrr::map(
    prompts_df_list,
    ~ block_structure_gemini(.x, api = api, system = system)
  )

  # 5) For each set of splitted prompts, separate out "cached" vs. "final" blocks
  final_prompts_list <- purrr::map(splitted_prompts_list, function(splitted_prompts) {

    # We'll accumulate blocks in final_blocks or cache_blocks
    final_blocks <- list()
    cache_blocks <- list()

    for (p in splitted_prompts) {
      if (!is.null(cache) && p$row_index %in% cache) {
        cache_blocks[[length(cache_blocks) + 1]] <- p$block
      } else {
        final_blocks[[length(final_blocks) + 1]] <- p$block
      }
    }

    # Attach the system attribute to each prompt
    if (!is.null(system)) {
      attr(final_blocks, "system") <- system
    }

    # If we have any cache blocks, attach them as an attribute
    if (length(cache_blocks) > 0) {
      if (is.null(cache_name)) {
        cache_name <- digest::digest(cache_blocks)
      }
      attr(final_blocks, "cache") <- list(
        name = cache_name,
        content = cache_blocks
      )
    }

    # After constructing all blocks, add a top-level class
    final_blocks <- structure(
      final_blocks,
      class = c(paste(api, class(final_blocks), sep = "_"), class(final_blocks))
    )

    final_blocks
  })

  # 6) If data had multiple rows, we get multiple expansions
  if (length(final_prompts_list) > 1) {
    # Combine into "combined_prompts" S3 object
    final_prompts <- structure(
      final_prompts_list,
      class = c(api, "combined_prompts", class(final_prompts_list))
    )
  } else {
    # Single expansion => final_prompts is just the first (only) item
    final_prompts <- final_prompts_list[[1]]
    # Attach a gemini-specific S3 class, e.g. "gemini_list"
    final_prompts <- structure(
      final_prompts,
      class = c(paste(api, class(final_prompts), sep = "_"), class(final_prompts))
    )
  }

  final_prompts
}

process_user_prompts <- function(user, rtika = FALSE, quiet = TRUE) {
  file_indices <- fs::file_exists(user)
  image_file_indices <- file_indices & is_image(user)
  image_url_indices <- !file_indices & is_url(user) & is_image(user)
  text_file_indices <- file_indices & !image_file_indices & !is_pdf(user)
  pdf_file_indices <- is_pdf(user)

  text_files <- user[text_file_indices & !pdf_file_indices]
  image_files <- user[image_file_indices]
  image_urls <- user[image_url_indices]
  pdf_files <- user[pdf_file_indices]
  texts <- user[!file_indices & !image_url_indices & !pdf_file_indices]

  image_data <- character(0)
  pdf_data <- character(0)
  combined_user <- character(length(user))

  if (length(text_files) > 0) {
    if (!quiet) {
      cli::cli_alert_info("Found file prompts: {text_files}. Reading them now.")
    }
    text_file_contents <- if (rtika) {
      rtika::tika_text(text_files)
    } else {
      purrr::map_chr(text_files, ~ paste(readr::read_lines(.), collapse = "\n"))
    }
  } else {
    text_file_contents <- character(0)
  }

  if (length(image_files) > 0) {
    if (!quiet) {
      cli::cli_alert_info("Found images: {image_files}. Processing them now.")
    }
    image_data <- purrr::map_chr(image_files, ~ base64enc::dataURI(file = ., mime = mime::guess_type(.), encoding = "base64"))
  }

  if (length(pdf_files) > 0) {
    if (!quiet) {
      cli::cli_alert_info("Found PDFs: {pdf_files}. Processing them now.")
    }
    pdf_data <- if (rtika) rtika::tika_text(pdf_files) else purrr::map_chr(pdf_files, ~ base64enc::dataURI(file = ., mime = mime::guess_type(.), encoding = "base64"))
  }

  combined_user[text_file_indices & !pdf_file_indices] <- text_file_contents
  combined_user[image_file_indices] <- image_data
  combined_user[image_url_indices] <- user[image_url_indices]
  combined_user[!file_indices & !image_url_indices] <- texts
  combined_user[pdf_file_indices] <- pdf_data

  dplyr::tibble(
    role = "user",
    content = combined_user,
    mime_type = mime::guess_type(user),
    openai_type = dplyr::case_when(text_file_indices ~ "text",
                                   image_file_indices ~ "image_url",
                                   image_url_indices ~ "image_url",
                                   TRUE ~ "text"),
    gemini_type = dplyr::case_when(image_file_indices ~ "inlineData",
                                   image_url_indices ~ "fileData",
                                   pdf_file_indices & !rtika ~ "inlineData",
                                   TRUE ~ "text"),
    claude_type = dplyr::case_when(image_file_indices ~ "image",
                                   image_url_indices ~ "image",
                                   pdf_file_indices & !rtika ~ "document",
                                   TRUE ~ "text")
  )
}


is_url <- function(x) {
  urls <- logical(length(x))
  for(i in 1:length(x)) {
    urls[i] <- !is.null(httr::parse_url(x[i])$scheme)
  }
  urls
}

is_image <- function(x) {
  allowed_extensions <- c("jpeg","jpg", "png", "gif", "webp", "heic", "heif")
  ext <- tools::file_ext(x)
  ext %in% c(allowed_extensions, toupper(allowed_extensions))
}

is_pdf <- function(x, allowed_extensions = c("pdf")) {
  ext <- tools::file_ext(x)
  ext %in% allowed_extensions
}

glue_data <- function(prompts_df, data) {
  checkmate::assert_data_frame(data)
  final_data <- list(nrow(data))
  for(i in 1:nrow(data)) {
    df <-  prompts_df |>
      dplyr::group_by(dplyr::across(-"content")) |>
      dplyr::rowwise() |>
      dplyr::mutate(content = stringr::str_glue_data(data[i, ], content))
    final_data[[i]] <- df
  }
  final_data
}

block_structure_openai <- function(prompts_df, api, system) {
  block <- purrr::pmap(prompts_df, function(...) {
    parts <- list(...)

    if(parts$openai_type == "text") {
      role_content_pairs <- list(role = parts$role,
                                 content = parts$content)
    } else if(parts$openai_type == "image_url") {
      role_content_pairs <- list(role = parts$role,
                                 content = list(list(type = "image_url",
                                                     image_url = list(url = parts$content))))
    } else {
      cli::cli_abort("Unsupported openai_type: {parts$openai_type}")
    }

    role_content_pairs
  })
  if(!is.null(system)) {
    new_block <- list()
    new_block[[1]] <- list(role = "system", content = system)
    new_block <- c(new_block, block)
    block <- structure(new_block, class = c(paste(api, class(block), sep = "_"), "list"))
  } else {
    block <- structure(block, class = c(paste(api, class(block), sep = "_"), "list"))
  }
  block
}

block_structure_claude <- function(prompts_df, api, system) {
  block <- purrr::pmap(prompts_df, function(...) {
    parts <- list(...)

    # Create the top-level "role" object
    # For Claude, each message is of the form:
    # {
    #   "role": "user" (or "assistant", etc),
    #   "content": [
    #       {
    #          "type": "text" | "image" | "document",
    #          "text": "...",              # if type="text"
    #          "cache_control": {"type": "ephemeral"} # optional
    #       }
    #    ]
    # }

    # -- We'll build a single content block:
    this_content_block <- list(type = parts$claude_type)

    if (parts$claude_type == "text") {
      # For plain text
      this_content_block$text <- parts$content
    } else {
      # e.g. image or document
      # store the base64 data in "source"
      this_content_block$source <- list(
        type = "base64",
        media_type = parts$mime_type,
        data = stringr::str_remove(parts$content, ".+;base64,")
      )
    }

    # -- MODIFIED: if .cache_control says "ephemeral", attach it to this block
    if (!is.na(parts$.cache_control) && identical(parts$.cache_control, "ephemeral")) {
      this_content_block$cache_control <- list(type = "ephemeral")
    }

    # Wrap up in final "role" + "content" shape
    role_content_pairs <- list(
      role    = parts$role,
      content = list(this_content_block)
    )

    role_content_pairs
  })

  if(!is.null(system)) {
    attr(block, "system") <- system
  }
  block <- structure(block, class = c(paste(api, class(block), sep = "_"), "list"))
  block
}

#' Build Gemini-Style Blocks
#'
#' @keywords internal
block_structure_gemini <- function(prompts_df, api, system = NULL, ...) {
  # Use pmap to loop over each row of prompts_df
  splitted_prompts <- purrr::pmap(prompts_df, function(...) {
    parts <- list(...)

    # Build the Gemini "parts" object
    gemini_part <- switch(
      parts$gemini_type,
      "text" = list(text = parts$content),
      "inlineData" = list(
        inlineData = list(
          mimeType = parts$mime_type,
          data = stringr::str_remove(parts$content, ".+;base64,")
        )
      ),
      "fileData" = list(
        fileData = list(
          mimeType = parts$mime_type,
          fileUri = parts$content
        )
      ),
      stop("Unsupported gemini_type: ", parts$gemini_type)
    )

    # Gemini uses "model" in place of "assistant"
    role_plus_parts <- list(
      role = ifelse(parts$role == "assistant", "model", parts$role),
      parts = list(gemini_part)
    )

    # Return a small structure containing row_index + the final block
    list(
      row_index = parts$row_index,
      block = role_plus_parts
    )
  })

  splitted_prompts
}
