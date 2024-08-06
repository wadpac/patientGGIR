#' introText
#'
#' @param GGIRoutputdir Path to GGIR output folder, e.g. C:/output_mystudy
#' @param id Character, the id of the recording
#' @param lang Character, language to use fr=french, en=english
#' @param deviceName Character with the device name to be mention in report, which defaults to movement sensor watch in the chosen language if not specified
#' @return no object is returned, text is printed with cat
#' @export
#' 

introText = function(GGIRoutputdir = NULL, id = NULL, lang = "fr", deviceName = NULL) {
  
  if (!is.null(GGIRoutputdir) && !is.null(id)) {
    P4N = read.csv(file = paste0(GGIRoutputdir, "/results/part4_nightsummary_sleep_cleaned.csv"))
    date = min(P4N[grep(pattern = id, x = P4N$ID), "calendar_date"])
  }
  
  if (lang == "en") {
    
    if (is.null(deviceName)) {
      deviceName = "motion sensor watch"
    }
    cat(paste0("### You wore a ", deviceName, " on your wrist for several days from ", date, "\n"))
    cat(paste0("The ", deviceName, " recorded the different accelerations of ",
               "your wrist. This allowed us to accurately assess your physical activity and ",
               "sleep time over this period. The watch also measured the intensity of the light.", 
               "\n\nThese data are collected as part of research with the aim of improving ",
               "knowledge on circadian rhythm alterations in memory center patients and ",
               "thus providing better management of their symptoms, improving the quality ",
               "of life of patients. and their loved ones, and potentially slow the ",
               "progression of the disease. \n\nIn this document we offer you a description of ",
               "what the watch measured when you wore it. The extraction of these measures ",
               "is still under development. Furthermore, these results taken individually only ",
               "have an informative value and are only of interest when they are analyzed in ",
               "a study population.\n"))
  }
  
  if (lang == "fr") {
    if (is.null(deviceName)) {
      deviceName = "capteur de mouvement"
    }
    
    cat(paste0("### Vous avez port\u00E9 une montre ", deviceName, " \u00E0 votre",
               " poignet pendant plusieurs jours \u00E0 partir du ", date, "\n"))
    cat(paste0("La montre ", deviceName, " a enregistr\u00E9 les diff\u00E9rentes ",
               "acc\u00E9l\u00E9rations de votre poignet. Cela nous a permis d'\u00E9valuer ",
               "votre activit\u00E9 physique et votre temps de sommeil avec pr\u00E9cision ",
               "sur cette p\u00E9riode. La montre a mesur\u00E9 \u00E9galement l'intensit\u00E9 de",
               " la lumi\u00E8re. \n\nCes donn\u00E9es sont recueillies dans le cadre de la recherche ",
               "avec pour objectif d'am\u00E9liorer les connaissances sur les alt\u00E9rations ",
               "du rythme circadien chez les patients de centre m\u00E9moire et ainsi de ",
               "proposer une meilleure prise en charge de leurs sympt\u00F4mes, am\u00E9liorer ",
               "la qualit\u00E9 de vie des patients et de leurs proches, et potentiellement ",
               "ralentir l'\u00E9volution de la maladie. \n\nNous vous proposons dans ce document ",
               "un descriptif de ce qu'a mesur\u00E9 la montre lorsque vous la portiez. ",
               "L'extraction de ces mesures est encore en cours d'\u00E9laboration. De plus ",
               "ces r\u00E9sultats pris individuellement n'ont qu'une valeur informative et ",
               "n'ont d'int\u00E9r\u00EAt que lorsqu'ils sont analys\u00E9s dans une ",
               "population d'\u00E9tude.\n"))
  }
}