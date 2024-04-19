
# Install: https://miktex.org/download
# On Windows: install tabu package: https://stackoverflow.com/questions/47613096/tabu-sty-not-found
library(patientGGIR)

GGIRoutputdir = "D:/Dropbox/Work/sharedfolder/projects/ERC_Paris/Clinic/output_pilot2022"
creatReport(GGIRoutputdir = GGIRoutputdir,
            lang = "en", idsep = "_", desiredtz = "Europe/London", type = "onepage_luxsleepactcr_A4")
