#' Classify publications using Google's Gemini LLM with an external NSF taxonomy
#'
#' @description
#' Uses Google's Gemini LLM to classify academic publications according to the
#' National Science Foundation (NSF) fields-of-study classification system.
#' The function accepts a dataframe of publication abstracts and a taxonomy
#' dataframe, uploads the taxonomy to the LLM, and returns hierarchical
#' classifications.
#'
#' @param pubs_df A dataframe containing publication information. Must include
#'   columns 'doi' and 'abstract_text'.
#' @param taxonomy_df A dataframe containing the NSF taxonomy. Must include
#'   columns 'first_level', 'second_level', and 'third_level'.
#' @param model Character string specifying which Gemini model to use.
#'   Default is "gemini-2.0-flash".
#'
#' @return A tibble with the following columns:
#'   \item{doi}{Digital Object Identifier of the publication}
#'   \item{first_level}{NSF first-level classification field}
#'   \item{second_level}{NSF second-level classification field}
#'   \item{third_level}{NSF third-level classification field}
#'   \item{raw_response}{List column containing the complete raw JSON response}
#'
#' @examples
#' \dontrun{
#' classifications <- classify_pubs_gemini(
#'   my_publications,
#'   taxonomy_df = nsf_sed_taxonomy
#' )
#' }
#'
#' @importFrom ellmer chat_google_gemini google_upload
#' @importFrom dplyr filter transmute
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom jsonlite fromJSON
#' @importFrom purrr imap_dfr
#' @importFrom progress progress_bar
#' @importFrom stringr str_extract
#' @importFrom tibble tibble
#' @export
classify_pubs_gemini <- function(pubs_df, taxonomy_df, model = "gemini-2.0-flash") {
  # input validation
  required_cols <- c("first_level", "second_level", "third_level")
  if (!all(required_cols %in% colnames(taxonomy_df))) {
    stop("taxonomy_df must contain columns: ", paste(required_cols, collapse = ", "), ".")
  }
  if (!all(c("doi", "abstract_text") %in% colnames(pubs_df))) {
    stop("pubs_df must contain columns 'doi' and 'abstract_text'.")
  }

  # serialize taxonomy to temporary CSV file
  tmp_csv <- tempfile(fileext = ".csv")
  readr::write_csv(taxonomy_df, tmp_csv)

  # build a system prompt that instructs Gemini to load the taxonomy
  system_prompt <- paste(
    "You are an expert classifier who works for the National Science Foundation (NSF).",
    "I am uploading a file containing the complete NSF fields-of-study taxonomy.",
    "Please load that CSV into memory and use it to classify any abstracts I send.",
    "When I send an abstract, reply ONLY with JSON containing: doi, first_level,",
    "second_level, and third_level (matching exactly the taxonomy values).",
    "Do not wrap output in markdown or code fences or add any extra text."
  )

  # initialize the Gemini chat client
  chat <- ellmer::chat_google_gemini(
    system_prompt = system_prompt,
    model         = model
  )

  # upload taxonomy CSV into the chat context
  taxonomy_payload <- ellmer::google_upload(tmp_csv)
  chat$chat(taxonomy_payload)

  # prepare prompts for classification
  prompts_df <- pubs_df |>
    dplyr::filter(!is.na(abstract_text)) |>
    dplyr::transmute(
      doi,
      prompt = glue::glue(
        "DOI: {doi}\n\n",
        "Abstract: \"{abstract_text}\"\n\n",
        "Classify this abstract using the taxonomy I just uploaded.",
        " Return ONLY JSON with keys: doi, first_level, second_level, third_level."
      )
    )

  # progress bar
  pb <- progress::progress_bar$new(
    format = "  Classifying abstracts [:bar] :percent eta: :eta",
    total  = nrow(prompts_df),
    clear  = FALSE
  )

  # run classification
  results <- purrr::imap_dfr(
    prompts_df$prompt,
    function(prompt_text, i) {
      pb$tick()
      raw_txt  <- chat$chat(prompt_text)
      json_txt <- stringr::str_extract(raw_txt, "\\{[\\s\\S]*\\}")
      parsed   <- jsonlite::fromJSON(json_txt)

      tibble::tibble(
        doi          = parsed$doi,
        first_level  = parsed$first_level,
        second_level = parsed$second_level,
        third_level  = parsed$third_level,
        raw_response = list(raw_txt)
      )
    }
  )

  return(results)
}
