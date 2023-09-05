#' Seach for CRediT keywords and extract section.
#'
#' The algorithm searchers for a CRediT statements and extracts it.
#' If no known section title was found, the algorithm searches for several categories of similar keywords
#' in each sentence of the whole manuscript.
#'
#' @param PDF_text_sentences Document corpus loaded with the pdf_load function.
#'
#' @return Tibble with one row per screened document and the file name and logical values for CRediT
#' detection as columns plus additional column that contains the statement that was extracted.
#'
#' @examples
#' \dontrun{
#' screen_credit(pdf_load("examples/"))
#' }
#'
#' @export
screen_credit <- function(PDF_text_sentences)
{
  credit_section_list <- c(
    "Author\\W?s?\\W? Contributions? Statement",
    "Author\\W?s?\\W? Contributions?",
    "CRediT Authors?hip? Contributions?(statement)?",
    # "CRediT authorship contribution statement",
    "CRediT Author Statement",
    "Contributions",
    "Contributors",
    "Authorship"
  ) |>
    .format_keyword_vector() |>
    stringr::str_replace_all("w", "W")

# str_detect("credit authors contributions", "credit authors?(hip)? contributions?(statement)?")

  ackn_section_list <- c(
    "A ?c ?k ?n ?o ?w ?l ?e ?d ?g ?e? ?m ?e ?n ?t ?s?",
    "We (would like to )?thank"
  ) |>
    .format_keyword_vector()

  orcid_section_list <- c(

  ) |>
    .format_keyword_vector()

  # PDF_text_sentences <- pdf_text_corpus
  print("Extracting Contributions...")
  credit_text_sentences <- PDF_text_sentences |>
    furrr::future_map(\(x) .extract_section(x, credit_section_list), .progress = TRUE)

  print("Extracting Acknowledgements...")
  ackn_text_sentences <- PDF_text_sentences |>
    furrr::future_map(\(x) .extract_section(x, ackn_section_list), .progress = TRUE)

  ackn_results <- ackn_text_sentences |>
    .enframe_results(name = "article", value = "ackn_statement")

  credit_text_sentences |>
    .enframe_results(name = "article", value = "credit_statement") |>
    dplyr::left_join(ackn_results, by = "article") |>
    dplyr::mutate(has_acs = credit_statement != "",
                  has_ackn = ackn_statement != "")

}


#' convert results from list to tibble
#' @noRd
.enframe_results <- function(ls, name, value) {
  ls |>
    purrr::map_chr(\(x) paste(x, collapse = " ")) |>
    tibble::enframe(name = name, value = value)
}

# section_regexes <- credit_section_list
#' extract data availability statement
#' @noRd
.extract_section <- function(PDF_text_sentences, section_regexes) {

  # TODO: validate that text extraction stops at the right spots for ackn and credit

  section_string <- paste0("(<section>)\\W+[\\d,\\W]*(", section_regexes, ")\\b")
  # stringr::str_detect(PDF_text_sentence, data_availability)
  section_detections <- furrr::future_map_lgl(PDF_text_sentences,
                                          \(sentence) stringr::str_detect(sentence, section_string))

  # if (sum(section_detections) > 0) {
  #   DAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
  #                                           \(sentence) .has_DAS(sentence, keyword_list))
  # }

  section_start <- which(section_detections)

  # if more than one detections of a section were made, then return empty string
  if (length(section_start) == 2) {
    section_start <- min(section_start)
  } else if (length(section_start) != 1 ) {
    return("")
  }

  str_section <- PDF_text_sentences[section_start] |>
    stringr::str_trim()
  str_section_sameline <- str_section |>
    stringr::str_remove(section_string)

  credit_regex <- c("manuscript\\.?$",
                    "editing\\.$",
                    "version\\.$",
                    "paper\\.$",
                    "work\\.$",
                    "project\\.$",
                    "entirety\\.$",
                    "validation\\.$",
                    "review",
                    "analysis",
                    "supervision",
                    "\\(equal\\)",
                    "author",
                    "conceptuali(z|s)ation"
                    ) |>
    paste(collapse = "|")
  # candidates are sentences after the first section but before the next
  # which begin with <section> or digit. (reference number at start of line)
  is_plos <- any(stringr::str_detect(PDF_text_sentences[1:10], "^plos ")) #explosive?

  if (is_plos == TRUE) {

    section_end <- furrr::future_map_lgl(PDF_text_sentences[(section_start + 1):length(PDF_text_sentences)],
                                         \(sentence) stringr::str_detect(sentence, "section> references")) |>
      which() - 1

  } else {

    if (stringr::str_detect(section_regexes, "contribution")) {

      section_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(section_start + 1):length(PDF_text_sentences)],
                                           \(sentence) !stringr::str_detect(sentence, credit_regex)) |>
        which() - 1

      section_end <- section_end_candidates[1]

    } else {

      section_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(section_start + 1):length(PDF_text_sentences)],
                                                      \(sentence) stringr::str_detect(sentence, "section> (?!d )|^\\d\\.|section> references")) |>
        which() - 1
      # check if candidates are full sentences ending in full stop. This achieves splicing if section contines on next page
      completed_sentences <- furrr::future_map_lgl(PDF_text_sentences[section_start + section_end_candidates],
                                                   \(sentence) stringr::str_detect(sentence, "\\.$"))

      if (stringr::str_length(str_section_sameline) < 5 & str_section_sameline != ".") {
        # first_sentence <- DAS_start + 1

        section_end <- section_end_candidates[-1][completed_sentences[-1]][1]#

      } else {
        section_end <- section_end_candidates[completed_sentences][1] # the first complete sentence before the beginning of a section

      }

    }



    # if (section_start / length(section_detections) < 0.1 & section_end != 0) { # for plos journals with DAS on first page
    #
    #   section_second_part <- furrr::future_map_lgl(PDF_text_sentences[(section_start + section_end + 1):length(PDF_text_sentences)],
    #                                            \(sentence) stringr::str_detect(sentence, "<section> funding:"))
    #   if (sum(section_second_part) == 0) {
    #     section_end <- section_end
    #   } else {
    #     section_end <- section_end + which(section_second_part) - 1
    #   }
    # }
  }

  if (is.na(section_end)) section_end <- 0
  section_end <- section_start + section_end

  section <- PDF_text_sentences[section_start:section_end]

  if (section_start < 50 & any(stringr::str_detect(PDF_text_sentences[1:10], "plos"))) {
    section <- .splice_plos_twopager(section)
  }
  section |>
    # paste(collapse = " ") |>
    stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
    stringr::str_trim()

}


#' standardize the format of different keyword vectors
#' @noRd
.format_keyword_vector <- function(keywords, end_boundary = FALSE) {
  #typically word boundaries are added in the beginning only to allow for different possible endings
  if(end_boundary) {
    keywords_formatted <- paste0("\\b", keywords, "\\b")
  } else {
    keywords_formatted <- paste0("\\b", keywords)
  }
  #collapse keywords into one string with OR symbol between them and convert to lowercase
  keywords_formatted <- paste(keywords_formatted, collapse = "|") |> tolower()

  return(keywords_formatted)
}
