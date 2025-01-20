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
    label1 = paste0("En l'absence d'information du questionnaire journalier pour",
                    " ce jour, ces informations pourraient être de qualité moindre.")
    label2 = paste0("En l’absence d’information du questionnaire journalier, il ",
                    "n’a pas été possible de distinguer les périodes de sommeil ",
                    "diurnes et nocturnes.")
  } else if (lang == "en") {
    label1 = paste0("In the absence of information from the daily questionnaire ",
                    "for this day, this information could be of lower quality.")
    label2 = paste0("In the absence of information from the daily questionnaire, ",
                   "it was not possible to distinguish daytime and nighttime ",
                   "sleep periods.")
  }
  AsteriskInColumns =  length(grep(pattern = "[*]", x = colnames(data))) > 0
  explainAsterisk = ifelse(test = AsteriskInColumns,
                           yes = paste0("; * ", label1), no = "")
  if (length(data) > 0) {
    AbsenceOfSleep = length(grep("-", x = data[(nrow(data) - 4):nrow(data), 1:(ncol(data) - 1)], 
                                 invert = TRUE)) == 0
  } else {
    AbsenceOfSleep = FALSE
  }
  explainAbsenceOfSleep = ifelse(test = AbsenceOfSleep,
                                 yes = paste0("; * ", label2), no = "")
  kableExtra::kbl(data, booktabs = TRUE) |>
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
    footnote(general = paste0("ID: ", id, explainAsterisk, explainAbsenceOfSleep),
             threeparttable = TRUE,
             footnote_as_chunk = TRUE,
             general_title = "") |>
    column_spec(1, width = "4cm") |>
    row_spec(0, bold = TRUE) |>
    row_spec(1, bold = TRUE) |>
    row_spec(6, bold = TRUE) |>
    kable_styling(font_size = 8)
}