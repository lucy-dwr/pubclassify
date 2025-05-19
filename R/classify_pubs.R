#' Classify publications using Google's Gemini LLM
#'
#' @description
#' Uses Google's Gemini LLM to classify academic publications according to the
#' National Science Foundation (NSF) fields of study classification system.
#' The function takes a dataframe of publications with abstracts and returns
#' classifications at three hierarchical levels of the NSF taxonomy.
#'
#' @param pubs_df A dataframe containing publication information. Must include
#'   columns for 'doi' and 'abstract_text'.
#' @param model Character string specifying which Gemini model to use.
#'   Default is "gemini-2.0-flash".
#'
#' @return A tibble with the following columns:
#'   \item{doi}{Digital Object Identifier of the publication}
#'   \item{first_level}{NSF first-level classification field}
#'   \item{second_level}{NSF second-level classification field}
#'   \item{third_level}{NSF third-level classification field}
#'   \item{raw_response}{List column containing the complete raw JSON response from the Gemini API}
#'
#' @details
#' This function processes each publication abstract through the Gemini LLM model
#' with a specialized prompt that instructs the model to classify according to
#' NSF fields. It extracts JSON from the model's response and parses it into a
#' structured format. The function displays a progress bar during processing to
#' track completion status.
#'
#' @note
#' Requires the 'ellmer' package for interfacing with Google's Gemini LLM,
#' and an appropriate API key must be configured for the ellmer package.
#'
#' @examples
#' \dontrun{
#' # assume 'my_publications' is a dataframe with 'doi' and 'abstract_text' columns
#' classifications <- classify_pubs_gemini(my_publications)
#'
#' # use a different Gemini model
#' classifications <- classify_pubs_gemini(
#'   my_publications,
#'   model = "gemini-1.5-pro"
#' )
#' }
#'
#' @importFrom dplyr filter transmute
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom progress progress_bar
#' @importFrom purrr imap_dfr
#' @importFrom stringr str_extract
#' @importFrom tibble tibble
#'
#' @export
classify_pubs_gemini <- function(pubs_df, model = "gemini-2.0-flash") {
  # build the system prompt
  system_prompt <- paste(
    "You are an expert classifier who works for the National Science Foundation, NSF.",
    "You do not use acronyms to communicate classifications, ever.",
    "You aim for a high level of rigor and adherence to the NSF classification system.",
    "When given an abstract, respond ONLY with JSON containing:",
    "- doi: the article DOI",
    "- first_level: NSF first-level classification field",
    "- second_level: NSF second-level classification field",
    "- third_level: NSF third-level classification field"
  )

  # initialize the chat client
  chat <- ellmer::chat_google_gemini(
    system_prompt = system_prompt,
    model         = model
  )

  # build prompts
  prompts_df <- pubs_df |>
    dplyr::filter(!is.na(abstract_text)) |>
    dplyr::transmute(
      doi,
      abstract = abstract_text,
      prompt = glue::glue(
        "DOI: {doi}\n\n",
        "Abstract: \"{abstract}\"\n\n",
        "Classify this abstract according to the NSF fields of study. ",
        "Return ONLY JSON with keys: doi, title, first_level, second_level, third_level. ",
        "DO NOT wrap your output in markdown or code fences or add any extra text."
      )
    )

  # set up a progress bar
  pb <- progress::progress_bar$new(
    format = "  Classifying abstracts [:bar] :percent eta: :eta",
    total  = nrow(prompts_df),
    clear  = FALSE
  )

  # run through each prompt, extract the JSON, parse it, and collect results
  results <- purrr::imap_dfr(
    prompts_df$prompt,
    function(prompt_text, i) {
      pb$tick()
      raw_txt  <- chat$chat(prompt_text)
      json_txt <- stringr::str_extract(raw_txt, "\\{[\\s\\S]*\\}")
      parsed   <- jsonlite::fromJSON(json_txt)

      tibble::tibble(
        doi           = parsed$doi,
        first_level   = parsed$first_level,
        second_level  = parsed$second_level,
        third_level   = parsed$third_level,
        raw_response  = list(raw_txt)
      )
    }
  )

  return(results)
}
