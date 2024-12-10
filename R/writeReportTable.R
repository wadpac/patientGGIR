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
    label = "Le journal était incomplet ou peu clair, ce que nous avons corrigé"
  } else if (lang == "en") {
    label = "The diary was incomplete or unclear, which we corrected for"
  }
  explainAsterisk = ifelse(test = length(grep(pattern = "[*]", x = colnames(data))) > 0,
                           yes = paste0("; * = ", label), no = "")
  kableExtra::kbl(data, booktabs = TRUE) |>
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
    footnote(general = paste0("ID: ", id, explainAsterisk),
             footnote_as_chunk = TRUE) |>
    column_spec(1, width = "4cm") |>
    row_spec(0, bold = TRUE) |>
    row_spec(1, bold = TRUE) |>
    row_spec(7, bold = TRUE) |>
    kable_styling(font_size = 8)
}