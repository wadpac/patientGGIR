#' writeReportTable
#'
#' @param data Data.frame with data to be reported
#' @param id Character, the id of the recording
#' @param lang Character, language to use fr=french, en=english
#' @return no object is returned, table is added to report.
#' @import kableExtra
#' @import extrafont
#' @export
#' 

writeReportTable = function(data, id, lang) {
  if (lang == "fr") {
    label = "En l'absence d'information du questionnaire journalier pour ce jour, ces informations pourraient être de qualité moindre."
  } else if (lang == "en") {
    label = "In the absence of information from the daily questionnaire for this day, this information could be of lower quality."
  }
  explainAsterisk = ifelse(test = length(grep(pattern = "[*]", x = colnames(data))) > 0,
                           yes = paste0("; * ", label), no = "")
  kableExtra::kbl(data, booktabs = TRUE) |>
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
    footnote(general = paste0("ID: ", id, explainAsterisk),
             footnote_as_chunk = TRUE,  general_title = "") |>
    column_spec(1, width = "4cm") |>
    row_spec(0, bold = TRUE) |>
    row_spec(1, bold = TRUE) |>
    row_spec(6, bold = TRUE) |>
    kable_styling(font_size = 8)
}