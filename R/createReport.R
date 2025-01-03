#' creatReport
#'
#' @param GGIRoutputdir Path to GGIR output folder, e.g. C:/output_mystudy
#' @param lang Character, language to use fr=french, en=english
#' @param idsep Character, to split filename and use first segment as participant id
#' @param desiredtz Character, timezone from timezone data base names, see also GGIR documentation.
#' @param type Character to specify type of report, current options: onepage_luxsleepactcr_A4
#' @param deviceName Character to be used as device name, if not specified then we call it movement sensor
#' @param maskingFile Character to point to csv file with dates to be masked per ID
#' @return no object is returned, a pdf is saves is save in the GGIr output directory
#' @export
#' 
creatReport = function(GGIRoutputdir = NULL, lang = "fr", idsep = "_", desiredtz = "", type = NULL,
                       deviceName = NULL, maskingFile = NULL) {
  
  # Check input  
  if (!is.null(type)) {
    if (type == "onepage_luxsleepactcr_A4") {
      type_template = system.file("templates/onepage_luxsleepactcr_A4.Rmd", package = "patientGGIR")[1]
    } else {
      stop(paste0("Value for type (", type, ") is not facilitated"), call. = FALSE)
    }
  } else {
    stop("Argument type not specified.", call. = FALSE)
  }
  if (lang %in% c("fr", "en") == FALSE) {
    stop(paste0("Value for lang (", lang, ") is not facilitated"), call. = FALSE)
  }
  if (is.null(GGIRoutputdir)) {
    stop("Please specific argument GGIRoutputdir.", call. = FALSE)
  }
  
  # Extract ids to process
  getID = function(x) {
     return(unlist(strsplit(x, idsep))[1])
  }
  ids = unique(unlist(lapply(basename(dir(paste0(GGIRoutputdir, "/meta/ms5.outraw"),
                                          pattern = "[.]RData",
                                          full.names = FALSE,
                                          recursive = TRUE)), FUN = getID)))
  if (lang == "fr") {
    if (is.null(deviceName)) deviceName = "capteur de mouvement"
    docTitle = paste0("Mesures de la montre ", deviceName)
  } else if (lang == "en") {
    if (is.null(deviceName)) deviceName = "Motion sensor watch"
    docTitle = paste0(deviceName, " measurements")
  }
  
  reportDir = paste0(GGIRoutputdir, "/patientReports")
  if (!dir.exists(reportDir)) dir.create(reportDir)
  # Generate repots
  for (id in ids) {
    # plotfile = paste0(GGIRoutputdir, "/plot.png")
    plotfile = "./plot.png"
    pdffilename =  paste0(id , "_report_", lang,".pdf")
    
    if (!file.exists(paste0(reportDir, "/", pdffilename))) {
      cat(paste0("\nGenerate report for ", id, "\n"))
      rmarkdown::render(
        input = type_template,
        output_format = "pdf_document",
        output_file = pdffilename,
        output_dir = GGIRoutputdir,
        params = list(GGIRoutputdir = GGIRoutputdir, id = id, plotfile = plotfile, lang = lang, desiredtz = desiredtz, docTitle = docTitle,
                      deviceName = deviceName, maskingFile = maskingFile))
      if (file.exists(plotfile)) file.remove(plotfile)
      file.rename(from = paste0(GGIRoutputdir, "/", pdffilename),
                to = paste0(reportDir, "/", pdffilename))
    }
  }
}