#' prepareTable
#'
#' @param GGIRoutputdir Path to GGIR output folder, e.g. C:/output_mystudy
#' @param id Character, the id of the recording
#' @param lang Character, language to use fr=french, en=english
#' @return no object is returned, text is printed with cat
#' @export
#' 
prepareTable = function(GGIRoutputdir, id, lang) {
  # labels in multiple languages
  labels = matrix("", 12, 2)
  labels[1, ] = c("Heure de l'endormissement", "Sleep time") #00E0 is à
  labels[2, ] = c("Heure du r\u00E9veil", "Wake up time")
  labels[3, ] = c("Moyenne", "Average")
  labels[4, ] = c("D\u00E9but de la p\u00E9riode de 5 heures la plus active", "Start of the most active 5 hour period")
  labels[5, ] = c("Sommeil noturne:", "Nighttime sleep:")
  labels[6, ] = c("Activit\u00E9 en journ\u00E9e:", "Daytime activity:")
  labels[7, ] = c("Nombre d'heures pass\u00E9es au lit", "Number of hours spent in bed")
  labels[8, ] = c("Nombre d'heures pass\u00E9es \u00E0 dormir", "Number of hours spent sleeping")
  labels[9, ] = c("Proportion de temps dormi par rapport au temps pass\u00E9 au lit",
                  "Proportion of time slept compared to time spent in bed")
  labels[10, ] = c("Nombre d'heures d'activit\u00E9 mod\u00E9r\u00E9e",
                   "Number of hours of moderate activity")
  labels[11, ] = c("Nombre d'heures d'activit\u00E9 l\u00E9g\u00E8re",
                   "Number of hours of light activity")
  labels[12, ] = c("Temps inactif ou de repos", "Inactive or rest time")
  
  labels = as.data.frame(x = labels)
  colnames(labels) = c("fr", "en")
  
  # Load all GGIR results (always in English)
  P2D = read.csv(file = paste0(GGIRoutputdir, "/results/part2_daysummary.csv"))
  P2D = P2D[grep(pattern = id, x = P2D$ID), c("M5hr_ENMO_mg_0.24hr",
                                              "weekday", "calendar_date")]
  P2D = P2D[!is.na(P2D$M5hr_ENMO_mg_0.24hr),]
  
  P4N = read.csv(file = paste0(GGIRoutputdir, "/results/part4_nightsummary_sleep_cleaned.csv"))
  P4N = P4N[grep(pattern = id, x = P4N$ID), c("sleeponset_ts",
                                              "wakeup_ts",
                                              "SptDuration",
                                              "SleepDurationInSpt", "weekday", "calendar_date")]
  
  P5D = read.csv(file = paste0(GGIRoutputdir, "/results/part5_daysummary_WW_L40M100V400_T5A5.csv"))
  P5D = P5D[grep(pattern = id, x = P5D$ID), c("dur_day_total_MOD_min",
                                              "dur_day_total_LIG_min",
                                              "dur_day_total_IN_min",
                                              "weekday", "calendar_date")]
  summaryColumn = rep("", 10)
  
  P5D$calendar_date = as.Date(P5D$calendar_date, format = "%Y-%m-%d")
  # Sleep
  shortenTime = function(time) {
    return(paste0(unlist(strsplit(time, ":"))[1:2], collapse = ":"))
  }
  P4N$sleeponset_ts = unlist(lapply(X = P4N$sleeponset_ts, FUN = shortenTime))
  P4N$wakeup_ts = unlist(lapply(X = P4N$wakeup_ts, FUN = shortenTime))
  P4N$calendar_date = as.Date(P4N$calendar_date, format = "%d/%m/%Y")
  names(P4N)[grep(pattern = "sleeponset_ts", x = names(P4N))] = labels[1, lang] #Bed time / Onset?
  names(P4N)[grep(pattern = "wakeup_ts", x = names(P4N))] = labels[2, lang] #Wake up
  P4N$ratio = paste0(round((P4N$SleepDurationInSpt / P4N$SptDuration) * 100), "%")
  
  readableHours = function(hours) {
    HR = floor(hours)
    MINS = floor((hours - HR) * 60)
    MINS = paste0(ifelse(MINS < 10, yes = "0", no = ""), MINS)
    return(paste0(HR, "h", MINS, "min"))
  }
  summaryColumn[1] = labels[3, lang] #sur les 7 jours
  summaryColumn[4] = readableHours(mean(P4N$SptDuration, rm.na = TRUE))
  summaryColumn[5] = readableHours(mean(P4N$SleepDurationInSpt, rm.na = TRUE))
  summaryColumn[6] = paste0(round(mean((P4N$SleepDurationInSpt / P4N$SptDuration) * 100,
                                       rm.na = TRUE)), "%")
  
  P4N$SptDuration = unlist(lapply(X = P4N$SptDuration, FUN = readableHours))
  P4N$SleepDurationInSpt = unlist(lapply(X = P4N$SleepDurationInSpt, FUN = readableHours))
  
  names(P4N)[grep(pattern = "SptDuration", x = names(P4N))] = labels[7, lang]
  names(P4N)[grep(pattern = "SleepDurationInSpt", x = names(P4N))] = labels[8, lang]
  names(P4N)[grep(pattern = "ratio", x = names(P4N))] = labels[9, lang]
  P4N = P4N[order(P4N$calendar_date), ]
  
  # Physical activity
  modvar = grep(pattern = "total_MOD", x = names(P5D))
  ligvar = grep(pattern = "total_LIG", x = names(P5D))
  invar = grep(pattern = "total_IN", x = names(P5D))
  summaryColumn[7] = readableHours(mean(P5D[, modvar], rm.na = TRUE) / 60)
  summaryColumn[8] = readableHours(mean(P5D[, ligvar], rm.na = TRUE) / 60)
  summaryColumn[9] = readableHours(mean(P5D[, invar], rm.na = TRUE) / 60)
  
  P5D[, modvar] = unlist(lapply(X = P5D[, modvar] / 60, FUN = readableHours))
  P5D[, ligvar] = unlist(lapply(X = P5D[, ligvar] / 60, FUN = readableHours))
  P5D[, invar] = unlist(lapply(X = P5D[, invar] / 60, FUN = readableHours))
  
  names(P5D)[modvar] = labels[10, lang]
  names(P5D)[ligvar] = labels[11, lang]
  names(P5D)[invar] = labels[12, lang]
  P5D = P5D[order(P5D$calendar_date), ]
  
  # M5 timing
  M5HR = floor(P2D$M5hr_ENMO_mg_0.24hr)
  M5MIN = floor((P2D$M5hr_ENMO_mg_0.24hr - M5HR) * 60)
  P2D$M5hr_ENMO_mg_0.24hr = paste0(M5HR, ":", ifelse(M5MIN < 10, yes = "0", no = ""), M5MIN)
  
  P2D$calendar_date = as.Date(P2D$calendar_date, format = "%Y-%m-%dT")
  names(P2D)[grep(pattern = "M5hr_ENMO_mg_0.24hr", x = names(P2D))] = labels[4, lang]
  
  daydata = merge(P4N, P5D, by = c( "calendar_date","weekday")) #
  daydata = merge(daydata, P2D, by = c("calendar_date", "weekday")) #,
  daydata = daydata[order(daydata$calendar_date), ]
  # Add missing days as empty columns
  
  # Dummy data for testing
  # daydata = data.frame(calendar_date = as.Date(c("2024-06-03", "2024-06-04", "2024-06-06", "2024-06-07")),
  #                      value = 1:4)
  dRange = range(daydata$calendar_date)
  expectedDate = dRange[1]:dRange[2]
  missingDates = as.Date(expectedDate[which(expectedDate %in% daydata$calendar_date == FALSE)])
  if (length(missingDates) > 0) {
    tmp_daydata = data.frame(calendar_date = missingDates)
    daydata = merge(tmp_daydata, daydata, by = "calendar_date", all.x = TRUE)
    daydata$weekday = weekdays(daydata$calendar_date)
    isna = is.na(daydata)
    if (any(isna)) {
      daydata[isna] = "-"
    }
  }
  daydata = daydata[, grep(pattern = "calendar", x = colnames(daydata), invert = TRUE)]
  if (lang != "en") {
    # Translate
    weekday_English = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    if (lang == "fr") {
      weekday_NewLang = c("samedi", "dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi")
    }
    for (i in 1:7) {
      daydata$weekday = gsub(pattern = weekday_English[i], replacement = weekday_NewLang[i], x = daydata$weekday)
    }
  }
  
  daydata = cbind(t(daydata), summaryColumn)
  daydata = rbind(daydata[1,],
                  rep("", ncol(daydata)),
                  daydata[2:6,],
                  rep("", ncol(daydata)),
                  daydata[7:nrow(daydata),])
  
  row.names(daydata)[c(2, 8)] = labels[5:6, lang]
  
  row.names(daydata)[grep(pattern = "weekday", x = row.names(daydata))] = "Day of the week"
  colnames(daydata) = daydata[1,]
  daydata = daydata[-1,]
  return(daydata)
}