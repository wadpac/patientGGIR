rm(list = ls())
graphics.off()
# Install: https://miktex.org/download
# On Windows: install tabu package: https://stackoverflow.com/questions/47613096/tabu-sty-not-found
library(patientGGIR)
library(kableExtra)
# source("R/createReport.R")
# source("R/plot_lux_sleep_act_cr.R")
# source("R/prepareTable.R")
# source("R/introText.R")
# source("R/writeReportTable.R")
# GGIRoutputdir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Clinic/output_pilot2022"
# idsep = "_"
# GGIRoutputdir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Clinic/Circame_2024_bin/output_S20_2024_copie"
# GGIRoutputdir = "~/Dropbox/Work/sharedfolder/projects/ERC_Paris/Clinic/Circame_2024_bin/output_S20_2024_copie"
GGIRoutputdir = "~/data/ERC_Circame/output_bins"
idsep = "[.]"
lang = "fr"

# # # ================================================
# source("R/plot_lux_sleep_act_cr.R")
# getID = function(x) {
#   x = gsub(pattern = "meta_", replacement = "", x = x)
#   return(unlist(strsplit(x, idsep))[1])
# }
# ids = unlist(lapply(dir(paste0(GGIRoutputdir, "/meta/basic"), full.names = FALSE), FUN = getID))
# 
# 
# x11()
# plot_lux_sleep_act_cr(GGIRoutputdir = GGIRoutputdir, id = ids[6],
#                       lang = lang, desiredtz = "Europe/London")

# 
# 
# # # ================================================
# # # Run prepare table code in isolation
# source("R/prepareTable.R")
# getID = function(x) {
#   x = gsub(pattern = "meta_", replacement = "", x = x)
#   return(unlist(strsplit(x, idsep))[1])
# }
# ids = unlist(lapply(dir(paste0(GGIRoutputdir, "/meta/basic"), full.names = FALSE), FUN = getID))
# 
# data = prepareTable(GGIRoutputdir, id = ids[6], lang)


# Run code as called from within Markdown
creatReport(GGIRoutputdir = GGIRoutputdir,
            lang = lang, idsep = idsep, desiredtz = "Europe/Paris", type = "onepage_luxsleepactcr_A4",
            deviceName = "GENEActiv")


