rm(list = ls())
graphics.off()
# Install: https://miktex.org/download
# On Windows: install tabu package: https://stackoverflow.com/questions/47613096/tabu-sty-not-found
library(patientGGIR)

GGIRoutputdir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Clinic/output_pilot2022"
lang = "fr"
# # Run code in isolation
# source("R/plot_lux_sleep_act_cr.R")
# getID = function(x) {
#   x = gsub(pattern = "meta_", replacement = "", x = x)
#   return(unlist(strsplit(x, "_"))[1])
# }
# ids = unlist(lapply(dir(paste0(GGIRoutputdir, "/meta/basic"), full.names = FALSE), FUN = getID))
# 
# 
# x11()
# plot_lux_sleep_act_cr(GGIRoutputdir = GGIRoutputdir, id = ids[1],
#                       lang = lang, desiredtz = "Europe/London")
# 
# 
# kkkk
# Run code as called from within Markdown
creatReport(GGIRoutputdir = GGIRoutputdir,
            lang = lang, idsep = "_", desiredtz = "Europe/London", type = "onepage_luxsleepactcr_A4",
            deviceName = "GENEActiv")



col = rainbow(n = 200)
plot(1:200, rep(1, 200), col = col, pch = 20, type = "p", cex= 10)

