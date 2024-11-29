#' creatReport
#'
#' @param GGIRoutputdir Path to GGIR output folder, e.g. C:/output_mystudy
#' @param lang Character, language to use fr=french, en=english
#' @param idsep Character, to split filename and use first segment as participant id
#' @param desiredtz Character, timezone from timezone data base names, see also GGIR documentation.
#' @param type Character to specify type of report, current options: onepage_luxsleepactcr_A4
#' @param deviceName Character to be used as device name, if not specified then we call it movement sensor
#' @return no object is returned, a pdf is saves is save in the GGIr output directory
#' @export
#' 
creatReport = function(GGIRoutputdir = NULL, lang = "fr", idsep = "_", desiredtz = "", type = NULL,
                       deviceName = NULL) {
  
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
    x = gsub(pattern = "meta_", replacement = "", x = x)
    return(unlist(strsplit(x, idsep))[1])
  }
  ids = unlist(lapply(dir(paste0(GGIRoutputdir, "/meta/basic"), full.names = FALSE), FUN = getID))
  
  if (lang == "fr") {
    if (is.null(deviceName)) deviceName = "capteur de mouvement"
    docTitle = paste0("Mesures de la montre ", deviceName)
  } else if (lang == "en") {
    if (is.null(deviceName)) deviceName = "Motion sensor watch"
    docTitle = paste0(deviceName, " measurements")
  }
  # Generate repots
  for (id in ids) {
    # plotfile = paste0(GGIRoutputdir, "/plot.png")
    plotfile = "./plot.png"
    
    rmarkdown::render(
      input = type_template,
      output_format = "pdf_document",
      output_file = paste0(id , "_report_", lang,".pdf"),
      output_dir = GGIRoutputdir,
      params = list(GGIRoutputdir = GGIRoutputdir, id = id, plotfile = plotfile, lang = lang, desiredtz = desiredtz, docTitle = docTitle,
                    deviceName = deviceName))
    if (file.exists(plotfile)) file.remove(plotfile)
  }
}