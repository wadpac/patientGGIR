#' writeReportTable
#'
#' @param data Data.frame with data to be reported
#' @param id Character, the id of the recording
#' @return no object is returned, table is added to report.
#' @import kableExtra
#' @import extrafont
#' @export
#' 

writeReportTable = function(data, id) {
  kableExtra::kbl(data, booktabs = TRUE) |>
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
    footnote(general = paste0("ID: ", id),
             footnote_as_chunk = TRUE) |>
    column_spec(1, width = "5cm") |>
    row_spec(0, bold = TRUE) |>
    row_spec(1, bold = TRUE) |>
    row_spec(7, bold = TRUE) |>
    kable_styling(font_size = 8)
}