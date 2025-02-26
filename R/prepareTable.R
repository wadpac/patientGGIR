#' prepareTable
#'
#' @param GGIRoutputdir Path to GGIR output folder, e.g. C:/output_mystudy
#' @param id Character, the id of the recording
#' @param lang Character, language to use fr=french, en=english
#' @param maskingFile Character to point to csv file with dates to be masked per ID
#' @return no object is returned, text is printed with cat
#' @export
#' 
prepareTable = function(GGIRoutputdir, id, lang, maskingFile = NULL) {
  # labels in multiple languages
  # \u00E0 = à # a with line to the left
  # \u00E9 = é # e with line to the right
  # \u00E8 = è # e with line to the left
  labels = matrix("", 13, 2)
  labels[1, ] = c("Heure de l'endormissement", "Sleep time") #00E0 is à
  labels[2, ] = c("Heure du r\u00E9veil", "Wake up time")
  labels[3, ] = c("Moyenne", "Average")
  labels[5, ] = c("Activit\u00E9 en journ\u00E9e:", "Daytime activity:")
  labels[6, ] = c("Sommeil nocturne:", "Nighttime sleep:")
  labels[7, ] = c("Temps total \u00E9coul\u00E9 entre l'endormissement et le r\u00E9veil",
                  "Total time from falling asleep to waking up")
  labels[8, ] = c("Temps pass\u00E9 \u00E0 dormir", "Time spent sleeping")
  labels[9, ] = c("Proportion de temps dormi entre l'endormissement et le r\u00E9veil",
                  "Proportion of time slept between falling asleep and waking up")
  labels[10, ] = c("Temps d'activit\u00E9 physique",
                   "Time spent in moderate or vigorous activity")
  labels[11, ] = c("Temps d'activit\u00E9 l\u00E9g\u00E8re",
                   "Time spent in light activity")
  labels[12, ] = c("Temps inactif ou de repos", "Inactive or rest time")
  labels[13, ] = c("Code d'imputation du journal", "Diary Imputation Code")
  
  labels = as.data.frame(x = labels)
  colnames(labels) = c("fr", "en")
  
  # Load all GGIR results (always in English)
  P2D = read.csv(file = paste0(GGIRoutputdir, "/results/part2_daysummary.csv"))
  P2D = P2D[grep(pattern = id, x = P2D$ID), c("M5hr_ENMO_mg_0.24hr",
                                              "weekday", "calendar_date")]
  P2D = P2D[!is.na(P2D$M5hr_ENMO_mg_0.24hr),]
  
  P4N = read.csv(file = paste0(GGIRoutputdir, "/results/QC/part4_nightsummary_sleep_full.csv"))
  P4N = P4N[grep(pattern = id, x = P4N$ID), c("sleeponset_ts",
                                              "wakeup_ts",
                                              "SptDuration",
                                              "SleepDurationInSpt", "weekday", "calendar_date", "cleaningcode")]
  
  NP4 = nrow(P4N)
  P5D = read.csv(file = paste0(GGIRoutputdir, "/results/part5_daysummary_WW_L40M100V400_T5A5.csv"))
  P5D = P5D[grep(pattern = id, x = P5D$ID), c("dur_day_total_VIG_min",
                                              "dur_day_total_MOD_min",
                                              "dur_day_total_LIG_min",
                                              "dur_day_total_IN_min",
                                              "weekday", "calendar_date", "window_number")]
  summaryColumn = rep("", 10)
  P5D$calendar_date = as.Date(P5D$calendar_date, format = "%Y-%m-%d")
  
  
  # Mask windows listed in the masking file
  maskDates = NULL
  if (!is.null(maskingFile)) {
    mask = data.table::fread(file = maskingFile, data.table = FALSE)
    if (id %in% mask$ID) {
      mask = mask[which(mask$ID == id),]
      tmp = which(P5D$window_number %in% mask$window)
      if (length(tmp) > 0) {
        maskDates = P5D$calendar_date[tmp]
      }
    }
  }
  P5D = P5D[, which(colnames(P5D) != "window_number")]
  
  if (nrow(P5D) == 0 | nrow(P4N) == 0 | nrow(P2D) == 0) {
    warning(paste0("Report for ", id, " failed."), call. = FALSE)
    return()
  }
  #=============================================================================
  # Sleep
  shortenTime = function(time) {
    return(paste0(unlist(strsplit(time, ":"))[1:2], collapse = ":"))
  }
  readableHours = function(hours) {
    if (hours == "-") {
      return(hours)
    } else {
      hours = as.numeric(hours)
    }
    HR = floor(hours)
    MINS = floor((hours - HR) * 60)
    MINS = paste0(ifelse(MINS < 10, yes = "0", no = ""), MINS)
    return(paste0(HR, "h", MINS, "min"))
  }
  averageTime = function(x) {
    x = x[which(!is.na(x) & x != "-" & x != "")] 
    times = as.POSIXlt(x, format = "%H:%M")
    before6pm = which(times$hour < 18)
    if (length(before6pm) > 0) {
      times[before6pm] = times[before6pm] + 24 * 3600
    }
    times = as.POSIXct(mean(as.numeric(times)), tz = "")
    averageTime = format(times, format = "%H:%M")
    return(averageTime)
  }
  
  P4N$sleeponset_ts = unlist(lapply(X = P4N$sleeponset_ts, FUN = shortenTime))
  P4N$wakeup_ts = unlist(lapply(X = P4N$wakeup_ts, FUN = shortenTime))
  P4N$calendar_date = as.Date(P4N$calendar_date, format = "%Y-%m-%d")
  
  names(P4N)[grep(pattern = "sleeponset_ts", x = names(P4N))] = labels[1, lang] #Bed time / Onset?
  names(P4N)[grep(pattern = "wakeup_ts", x = names(P4N))] = labels[2, lang] #Wake up
  P4N$ratio = paste0(round((P4N$SleepDurationInSpt / P4N$SptDuration) * 100), "%")
  
  # Mask dates from maskDates file
  if (!is.null(maskDates)) {
    rem = which(P5D$calendar_date %in% maskDates)
    if (length(rem) > 0) {
      P5D = P5D[-rem, ]
    }
    rem = which(P4N$calendar_date %in% maskDates)
    if (length(rem) > 0) {
      P4N = P4N[-rem, ]
    }
  }
  rowIndex = 7
  
  # Add diary imputaton code, if present, as final row
  load(paste0(GGIRoutputdir, "/meta/sleeplog.RData"))
  impcode = which(logs_diaries$imputecodelog$ID == id)
  diaryImputationCodeAvailable = FALSE
  if (length(impcode) > 0) {
    diaryImputationCodeAvailable = TRUE
    logs_diaries$imputecodelog = logs_diaries$imputecodelog[impcode , ]
    colnames(logs_diaries$imputecodelog) = c("id", "calendar_date", labels[13, lang])
    P4N = merge(P4N, logs_diaries$imputecodelog, by = c("calendar_date"))
    P4N = P4N[, which(colnames(P4N) != "id")]
    summaryColumn[rowIndex] = ""
  }
  names(P4N)[grep(pattern = "SptDuration", x = names(P4N))] = labels[7, lang]
  names(P4N)[grep(pattern = "SleepDurationInSpt", x = names(P4N))] = labels[8, lang]
  names(P4N)[grep(pattern = "ratio", x = names(P4N))] = labels[9, lang]
  P4N = P4N[order(P4N$calendar_date), ]
  missingNights = which(P4N$cleaningcode > 0)
  if (length(missingNights) > 0) {
    P4N[missingNights, grep(pattern = "calendar|weekday", x = colnames(P4N), invert = TRUE)] = "-"
  }
  P4N = P4N[, grep(pattern = "cleaningcode", x = colnames(P4N), invert = TRUE)]
  
  # Average
  summaryColumn[1] = labels[3, lang] #sur les 7 jours
  validP4N = which(P4N[,labels[7, lang]] != "-")
  if (length(validP4N) > 0) {
    SptDuration = as.numeric(P4N[validP4N, labels[7, lang]])
    SleepSptDurationInSpt = as.numeric(P4N[validP4N, labels[8, lang]])
    Onset = P4N[validP4N, labels[1, lang]]
    Wakeup = P4N[validP4N, labels[2, lang]]
    summaryColumn[2] = averageTime(Onset) # onset
    summaryColumn[3] = averageTime(Wakeup) # wakeup
    summaryColumn[4] = readableHours(mean(SptDuration, rm.na = TRUE))
    summaryColumn[5] = readableHours(mean(SleepSptDurationInSpt, rm.na = TRUE))
    summaryColumn[6] = paste0(round(mean((SleepSptDurationInSpt / SptDuration) * 100,
                                         rm.na = TRUE)), "%")
  }
  P4N[, labels[7, lang]] = unlist(lapply(X = P4N[, labels[7, lang]], FUN = readableHours))
  P4N[, labels[8, lang]] = unlist(lapply(X = P4N[, labels[8, lang]], FUN = readableHours))
  
  #=============================================================================
  # Physical activity
  vigvar = grep(pattern = "total_VIG", x = names(P5D))
  modvar = grep(pattern = "total_MOD", x = names(P5D))
  ligvar = grep(pattern = "total_LIG", x = names(P5D))
  invar = grep(pattern = "total_IN", x = names(P5D))
  P5D[, modvar] = P5D[, modvar] + P5D[, vigvar] # MVPA
  summaryColumn[rowIndex + 1] = readableHours(mean(P5D[, modvar], rm.na = TRUE) / 60)
  summaryColumn[rowIndex + 2] = readableHours(mean(P5D[, ligvar], rm.na = TRUE) / 60)
  summaryColumn[rowIndex + 3] = readableHours(mean(P5D[, invar], rm.na = TRUE) / 60)
  
  P5D[, modvar] = unlist(lapply(X = P5D[, modvar] / 60, FUN = readableHours))
  P5D[, ligvar] = unlist(lapply(X = P5D[, ligvar] / 60, FUN = readableHours))
  P5D[, invar] = unlist(lapply(X = P5D[, invar] / 60, FUN = readableHours))
  
  names(P5D)[modvar] = labels[10, lang]
  names(P5D)[ligvar] = labels[11, lang]
  names(P5D)[invar] = labels[12, lang]
  P5D = P5D[order(P5D$calendar_date), ]
  P5D = P5D[, -vigvar]  
 
  daydata = merge(P4N, P5D, by = c( "calendar_date","weekday"), all = TRUE)
  if (nrow(daydata) == 0) {
    warning(paste0("Report for ", id, " failed."), call. = FALSE)
    return()
  }
  daydata = daydata[order(daydata$calendar_date), ]
  # ignore last day if it has 9 or more missing values,
  # this indicates a day included in part 2 (M5) but not in part 5
  if (rev(as.numeric(rowSums(is.na(daydata))))[1] >= 9) {
    daydata = daydata[-nrow(daydata), ]
  }
  # Add missing days as empty columns (this will not affect average)
  dRange = range(sort(unique(c(daydata$calendar_date, maskDates))))
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
      weekday_NewLang = c("Samedi", "Dimanche", "Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi")
    }
    for (i in 1:7) {
      daydata$weekday = gsub(pattern = weekday_English[i], replacement = weekday_NewLang[i], x = daydata$weekday)
    }
  }
  imputeColumID = which(names(daydata) == labels[13, lang])
  if (length(imputeColumID) > 0) {
    days_imputed = which(daydata[,imputeColumID] %in% c("0000", "-") == FALSE)
    if (length(days_imputed) > 0) {
      daydata$weekday[days_imputed] = paste0(daydata$weekday[days_imputed], "*")
    }
    daydata = daydata[, -imputeColumID]
    summaryColumn = summaryColumn[-imputeColumID]
  } else {
    summaryColumn = summaryColumn[1:(length(summaryColumn) - 1)]
  }
  daydata = cbind(t(daydata), summaryColumn)
  endSleepSection = 6
  daydata = rbind(daydata[1,],
                  rep("", ncol(daydata)),
                  daydata[(endSleepSection + 1):nrow(daydata),],
                  rep("", ncol(daydata)),
                  # matrix("", ifelse(diaryImputationCodeAvailable == FALSE, 2, 1), ncol(daydata)),
                  daydata[2:endSleepSection,])
  row.names(daydata)[c(2, endSleepSection)] = labels[5:6, lang]
  row.names(daydata)[grep(pattern = "weekday", x = row.names(daydata))] = "Day of the week"
  colnames(daydata) = daydata[1,]
  daydata = daydata[-1,]
  
  if (length(daydata) > 0) {
    AbsenceOfSleep = length(grep("-", x = daydata[(nrow(daydata) - 4):nrow(daydata), 1:(ncol(daydata) - 1)], 
                                 invert = TRUE)) == 0
    if (AbsenceOfSleep == TRUE) {
      row.names(daydata)[6] = paste0(row.names(daydata)[6], "*")
    }
  }
  return(daydata)
}