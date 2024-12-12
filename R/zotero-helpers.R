#' Get PDF Attachments from a Zotero Collection
#'
#' This function retrieves all PDF attachments in a specified Zotero collection.
#' It connects to the Zotero database, identifies items within the given
#' collection, and then locates associated PDF files stored in Zotero's
#' `storage` directory. Finally, it returns a [tibble()] including full file
#' paths. The zotero database must be closed before using this function;
#' otherwise it will throw an error. The function also requires the `DBI` and
#' `RSQLite` packages to be installed.
#'
#' @param collection_name A character string specifying the name of the Zotero
#'   collection from which to retrieve PDF attachments.
#' @param zotero_dir The directory where your Zotero database lives. If not
#'   specified, the function will try to guess.
#'
#' @return A [tibble()] (data frame) with an added class "zotero_collection"
#'   containing:
#'   * `libraryID`: The library ID associated with the item.
#'   * `collectionID`: The collection ID from Zotero.
#'   * `itemID`: The ID of the item in Zotero.
#'   * `collectionKey`: The collection key, useful to retrieve it from the zotero API.
#'   * `attachmentItemID`: The attachment ID.
#'   * `attachmentKey`: The subfolder (attachment key) within the Zotero `storage` directory.
#'   * `collectionName`: The collection name in Zotero.
#'   * `path`: The full file path to the PDF attachment on the local system.
#'   * `itemKey`: The key of the item in Zotero - useful for the API.
#'   * `fileLastModified`: When the file was last modified.
#'   And, if `better-bibtex` was installed:
#'   * `citationKey`: The `better-bibtex` citationKey for the item.
#'   * `pinned`: Whether the citation key is pinned or auto-generated.
#'
#' @export
#' @family zotero helpers
#'
#' @examples
#' \dontrun{
#' # Retrieve PDF attachments from a collection named "My Research"
#' pdfs <- zotero_collection_attachments("My Research")
#' head(pdfs)
#' }
zotero_collection_attachments <- function(collection_name, zotero_dir) {
  if(!rlang::is_installed("DBI") || !rlang::is_installed("RSQLite")) {
    cli::cli_abort("Interfacing with zotero collections requires the packages {.pkg DBI} and {.pkg RSQLite}")
  }
  conn_info <- zotero_connect(zotero_dir)
  on.exit(DBI::dbDisconnect(conn_info$con), add = TRUE)
  pdf_attachments <- get_pdf_attachments_from_collection(
    conn_info$con,
    conn_info$zotero_dir,
    collection_name) |>
    dplyr::rename(attachmentKey = "key")
  bbt_citation_keys <- get_better_bibtex_citation_keys(zotero_dir)
  if(!is.null(bbt_citation_keys)) {
    pdf_attachments <- pdf_attachments |>
      dplyr::left_join(bbt_citation_keys, by = dplyr::join_by("itemID", "libraryID"))
  }
  class(pdf_attachments) <- c("zotero_collection", class(pdf_attachments))
  pdf_attachments
}

guess_zotero_dir <- function() {
  sys_name <- Sys.info()[["sysname"]]

  if (sys_name == "Windows") {
    username <- Sys.getenv("USERNAME")
    zotero_dir <- file.path("/Users", username, "Zotero")
  } else if (sys_name == "Darwin") { # macOS
    zotero_dir <- file.path(fs::path_expand("~"), "Zotero")
  } else if (sys_name == "Linux") {
    # On Linux, Zotero might be in ~/.zotero/zotero or ~/Zotero. Adjust as needed.
    # If you know your Linux path exactly, use it.
    # As a default guess:
    zotero_dir <- file.path(fs::path_expand("~"), "Zotero")
  } else {
    cli::cli_abort("Unsupported OS. Please specify Zotero directory manually.")
  }
  zotero_dir
}

zotero_connect <- function(zotero_dir) {
  if(missing(zotero_dir)) {
    zotero_dir <- guess_zotero_dir()
  }

  db_path <- file.path(zotero_dir, "zotero.sqlite")

  if (!file.exists(db_path)) {
    stop("zotero.sqlite not found at: ", db_path)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  list(con = con, zotero_dir = zotero_dir)
}

# Helper function to get PDF attachments from a given collection
get_pdf_attachments_from_collection <- function(con, zotero_dir, collection_name) {

  collectionName <- key <- contentType <- path <- fileExists <- NULL
  # Get collection key
  collection_key <- dplyr::tbl(con, "collections") |>
    dplyr::filter(collectionName == collection_name) |>
    dplyr::pull("key")

  # Get items in that collection
  collection_items <- dplyr::tbl(con, "collectionItems") |>
    # join collections to filter on the desired collection by collectionID
    dplyr::inner_join(dplyr::tbl(con, "collections"), by = c("collectionID")) |>
    dplyr::filter(key == collection_key) |>
    dplyr::select("collectionID", "itemID", "libraryID", collectionKey = "key", "collectionName")

  items <- dplyr::tbl(con, "items")

  attachments <-  dplyr::tbl(con, "itemAttachments") |>
    dplyr::filter(contentType == "application/pdf")

  items_joined <- dplyr::inner_join(items,
                                    attachments,
                                    by = "itemID") |>
    dplyr::select("libraryID", "itemID", "key", "parentItemID", "path")

  pdf_attachments <- collection_items |>
    dplyr::inner_join(items_joined, by = c("itemID" = "parentItemID", "libraryID")) |>
    dplyr::select("libraryID", "collectionID", "itemID", "collectionKey",
                  attachmentItemID = "itemID.y", "key", "collectionName", "path") |>
    dplyr::distinct() |>
    dplyr::collect()

  # Construct full paths
  pdf_attachments <- pdf_attachments |>
    dplyr::mutate(path = stringr::str_remove(path, "^storage:"),
                  path = glue::glue("{zotero_dir}/storage/{key}/{path}"),
                  fileExists = fs::file_exists(path),
                  fileLastModified = fs::file_info(path)$modification_time)

  pdf_attachments |>
    dplyr::filter(fileExists) |>
    dplyr::select(-fileExists)
}


get_better_bibtex_citation_keys <- function(zotero_dir) {
  if(missing(zotero_dir)) {
    zotero_dir <- guess_zotero_dir()
  }

  db_path <- file.path(zotero_dir, "better-bibtex.sqlite")

  if (!file.exists(db_path)) {
    cli::cli_alert_warning("better-bibtex.sqlite not found at: ", db_path)
    cli::cli_alert_warning("Could not retrieve Better Bibtex citation keys.")
    return(NULL)

  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  citationKeys <- dplyr::tbl(con, "citationkey") |>
    dplyr::collect()

  citationKeys

}

