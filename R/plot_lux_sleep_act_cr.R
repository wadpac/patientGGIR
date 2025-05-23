#' plot_lux_sleep_act_cr
#'
#' @param GGIRoutputdir Path to GGIR output folder, e.g. C:/output_mystudy
#' @param id Character, participant id
#' @param lang Character, language to use fr=french, en=english
#' @param desiredtz Character, timezone from timezone data base names, see also GGIR documentation.
#' @param maskingFile Character to point to csv file with dates to be masked per ID
#' @param onlysleep Boolean to indicate whether only sleep should be plotted
#' @return no object is returned, only a plot is generated
#' @importFrom graphics abline axis layout legend lines mtext par rect
#' @importFrom stats aggregate ccf
#' @importFrom utils read.csv
#' @importFrom data.table fread
#' @export

plot_lux_sleep_act_cr = function(GGIRoutputdir, id, lang = "fr", desiredtz = "",
                                 maskingFile = NULL, onlysleep = FALSE) {
  P5ts = dir(paste0(GGIRoutputdir, "/meta/ms5.outraw/40_100_400"), full.names = T)
  P1 = dir(paste0(GGIRoutputdir, "/meta/basic"), full.names = T)
  path = paste0(GGIRoutputdir, "/meta/ms2.out")
  mdat = SUM = NULL
  
  # Define multilingual labels
  labels = matrix("", 13, 2)
  labels[1, ] = c("Exposition\n\u00E0 la lumiere", "Light exposure\nduring night") #00E0 is à
  labels[2, ] = c("Faible exposition", "Low exposure")
  labels[3, ] = c("Forte exposition", "High exposure")
  labels[4, ] = c("Sommeil", "Sleep")
  labels[5, ] = c("journ\u00E9e", "day") #00E9 is é / 00E8 is è 
  labels[6, ] = c("nocturne", "night")
  labels[7, ] = c("Activit\u00E9\nmesur\u00E9e par\nl'acc\u00E9l\u00E9ration", 
                  "Activity\nmeasured by\nacceleration")
  labels[8, ] = c("Acc\u00E9l\u00E9ration faible", "Low acceleration")
  labels[9, ] = c("Acc\u00E9l\u00E9ration \u00E9lev\u00E9e", "High acceleration")
  labels[10, ] = c("Rythme circadien", "Circadian Rhythm")
  labels[11, ] = c("Jour ", "Day ")
  labels[12, ] = c("Rythme\ncircadien", "Circadian\nrhythm")
  labels[13, ] = c(paste0("Amplitude du rythme\ncircadien estim\u00E9e\n\u00E0 partir",
                          " des donn\u00E9es\nd'acc\u00E9l\u00E9ration (en vert)"),
                   paste0("Amplitude of the\ncircadian rhythm\nestimated ",
                          "from the\naccelero meter data\n(in green)"))
  labels = as.data.frame(x = labels)
  colnames(labels) = c("fr", "en")
  
  col = c("red", "black",  "blue", "green", "purple")
  
  fns = dir(path, full.names = TRUE, pattern = as.character(id))
  # fns = fns[grep(pattern = "220169C", x = fns, value = FALSE, invert = TRUE)]
  
  reso = 300
  if (onlysleep == FALSE) {
    par(mfcol = c(4, 1), omi = rep(0, 4), oma = c(0, 9, 0, 4), mar = c(4, 4.5, 2, 10))
  } else {
    par(omi = rep(0, 4), oma = c(0, 9, 0, 4), mar = c(4, 4.5, 2, 10))
  }
  
  # identify dates that will be table
  P5D = read.csv(file = paste0(GGIRoutputdir, "/results/part5_daysummary_WW_L40M100V400_T5A5.csv"))
  P4N = read.csv(file = paste0(GGIRoutputdir, "/results/part4_nightsummary_sleep_cleaned.csv"))
  
  windows_in_table = P5D[grep(pattern = id, x = P5D$ID), "window_number"]
  dates_in_part4_table = as.Date(P4N[grep(pattern = id, x = P4N$ID), "calendar_date"])
  
  if (length(grep(pattern = "[.]RData", x = P5ts[grep(id, P5ts)])) > 0) {
    f2load = P5ts[grep(id, basename(P5ts))]
    if (length(f2load) > 1) f2load = f2load[which.min(nchar(f2load))]
    load(file = f2load)
    D = mdat
  } else {
    D = read.csv(file = P5ts[grep(id, basename(P5ts))])
  }
  f2load = P1[grep(id,basename(P1))]
  if (length(f2load) > 1) f2load = f2load[which.min(nchar(f2load))]
  load(file = f2load )
  # aggregate D
  D$timenum = floor(D$timenum / reso) * reso
  D$sib = 0
  D$sib[which(D$class_id == 0)] = 1
  D = aggregate(D[,c("ACC","window","sib","SleepPeriodTime", "invalidepoch")], by = list(D$timenum), FUN = mean)
  colnames(D)[1] = "timenum"
  D$time = as.POSIXlt(D$timenum, tz = desiredtz, origin = "1970-1-1")
  constrainTime = function(x, timecol = "time", timeRange) {
    x = x[which(x[, timecol] >= timeRange[1] &
                  x[, timecol] <= timeRange[2]), ]
  }
  # Constrain time series to window range as found in cleaned part 5 report
  D = D[which((D$window >= min(windows_in_table) & D$window <= max(windows_in_table)) |
                (as.Date(D$time) >= min(dates_in_part4_table) &
                   as.Date(D$time) <= max(dates_in_part4_table))), ]
  timeRange = range(D$timenum)
  
  D$clocktime = format(D$time,"%H:%M")
  D$sib = round(D$sib)
  D$sib[which(D$SleepPeriodTime == 0)] = 0
  M$metashort$timestamp = as.POSIXct(M$metashort$timestamp,
                                     format = "%Y-%m-%dT%H:%M:%S%z", desiredtz)
  M$metalong$timestamp = as.POSIXct(M$metalong$timestamp,
                                    format = "%Y-%m-%dT%H:%M:%S%z", desiredtz)
  # Constrain time series to time range matching windows as found in cleaned part 5 report
  M$metashort = constrainTime(x = M$metashort, timecol = "timestamp", timeRange = timeRange)
  M$metalong = constrainTime(x = M$metalong, timecol = "timestamp", timeRange = timeRange)
  if (nrow(M$metalong) == 0 | nrow(M$metashort) == 0) {
    warning(paste0("Report for ", id, " failed."), call. = FALSE)
    return()
  }
  LUXTEMP = M$metalong
  Mshort = M$metashort
  #-------------------------------------------------
  # detect sibs:
  Mshort$anglez[which(is.na(Mshort$anglez) == T)] = 0
  tt = 5 #timethreshold
  j = 5 #anglethreshol
  ws3 = 5

  sdl1 = rep(0,nrow(Mshort))
  postch = which(abs(diff(Mshort$anglez)) > j) #posture change of at least j degrees
  # count posture changes that happen less than once per ten minutes
  q1 = c()
  if (length(postch) > 1) {
    q1 = which(diff(postch) > (tt*(60/ws3))) #less than once per tt minutes
  }
  if (length(q1) > 0) {
    for (gi in 1:length(q1)) {
      sdl1[postch[q1[gi]]:postch[q1[gi] + 1]] = 1 #periods with no posture change
    }
  } else { #possibly a day without wearing
    if (length(postch) < 10) {  #possibly a day without wearing
      sdl1[1:length(sdl1)] = 1 #periods with no posture change
    } else {  #possibly a day with constantly posture changes
      sdl1[1:length(sdl1)] = 0 #periodsposture change
    }
  }
  Mshort$sibs = sdl1
  #-------------------------------------------------
  # aggregate Mshort
  Mshort$timenum = as.numeric(Mshort$timestamp)
  Mshort$timenum = floor(Mshort$timenum / reso) * reso
  Mshort = aggregate(Mshort[,c("ENMO","anglez", "sibs")], by = list(Mshort$timenum), FUN = mean)
  colnames(Mshort)[1] = "timenum"
  Mshort$timestamp = as.POSIXct(Mshort$timenum, tz = desiredtz, origin = "1970-1-1")
  Mshort$sibs = round(Mshort$sibs)
  minT = min(D$time)
  maxT = max(D$time)
  Mshort = Mshort[which(Mshort$timestamp >= minT & Mshort$timestamp <= maxT),]
  LUXTEMP = LUXTEMP[which(LUXTEMP$timestamp >= minT & LUXTEMP$timestamp <= maxT),]
  # tickmarks = which(D$clocktime == "00:00" | D$clocktime == "12:00")
  tickmarks = which(D$clocktime == "00:00") # long tickmarks
  timeat = D$time[tickmarks]
  timeat = c(timeat[1]-24*3600, timeat, timeat[length(timeat)] + 24*3600)
  timeval = D$clocktime[tickmarks]
  tickmarks2 = which(D$clocktime == "12:00")
  timeat2 = D$time[tickmarks2]
  wkday = weekdays(D$time[tickmarks2])
  if (lang == "fr") {
    wkday = gsub(pattern = "Sunday", replacement = "dimanche", x = wkday)
    wkday = gsub(pattern = "Saturday", replacement = "samedi", x = wkday)
    wkday = gsub(pattern = "Friday", replacement = "vendredi", x = wkday)
    wkday = gsub(pattern = "Thursday", replacement = "jeudi", x = wkday)
    wkday = gsub(pattern = "Wednesday", replacement = "mercredi", x = wkday)
    wkday = gsub(pattern = "Tuesday", replacement = "mardi", x = wkday)
    wkday = gsub(pattern = "Monday", replacement = "lundi", x = wkday)
  }
  # wkday = substr(wkday, start = 1, stop = 3)
  
  windowBorders = which(diff(D$window) != 0)
  dayBorders = Mshort$timenum[which(format(Mshort$timestamp,"%H:%M") == "00:00")]
  windowBorders2 = which(format(Mshort$timestamp) %in% format(D$time[windowBorders]))
  # Night time sib blocks
  Mshort$sib_day = Mshort$sibs
  Mshort$sib_night = Mshort$sibs
  Mshort$sib_night[which(D$SleepPeriodTime == 0)] = 0
  Mshort$sib_day[which(D$SleepPeriodTime == 1)] = 0
  Mshort$sib_day
  y0 = 90
  y1 = -90
  
  starttime = Mshort$timestamp[1]
  
  if (onlysleep == FALSE) {
    # prepare Circam data
    f2load = grep(pattern = as.character(id), x = fns, value =  TRUE)
    if (length(f2load) > 1) f2load = f2load[which.min(nchar(f2load))]
    load(f2load)
    cosinor_ts = SUM$cosinor_ts
    
    # align cosinor timeseries
    
    # add timestamps
    cosvars = SUM$summary[grep(pattern = "cosinor", x = names(SUM$summary), value = TRUE)]
    epochSize = min(diff(cosinor_ts$time_across_days[1:20]) * 3600)
    cosinor_ts$timestamp = starttime + (cosinor_ts$time_across_days - cosinor_ts$time_across_days[1]) * 3600
    # downsample to match resolution needed for plot
    cosinor_ts$time_epoch = round(as.numeric(cosinor_ts$timestamp) / reso) * reso
    cosinor_ts2 = aggregate(x = cosinor_ts, by = list(cosinor_ts$time_epoch), FUN = mean)
    cosinor_ts2$timestamp = as.POSIXct(cosinor_ts2$timestamp, tz = desiredtz)
    # match times based on correlation, because data is the same and timestamps are not stored
    out = ccf(cosinor_ts2$original, log(Mshort$ENMO * 1000 + 1), lag.max = 300)
    timeShiftSeconds = (which.max(out$acf) - 300) * reso
    cosinor_ts2$timestamp = cosinor_ts2$timestamp - timeShiftSeconds
    # merge with Mshort
    cosinor_ts2 =  cosinor_ts2[, c("timestamp", "original", "fittedY")]
    Mshort = merge(Mshort, cosinor_ts2, by = "timestamp", all.x = TRUE)
  }
  # Set all invalid epochs to NA
  if (1 %in% D$invalidepoch) {
    Mshort[which(D$invalidepoch == 1), grep(pattern = "time", x = colnames(Mshort))] = NA
    LUXTEMP[which(LUXTEMP$nonwearscore >= 2), grep(pattern = "time", x = colnames(LUXTEMP))] = NA
  }
  # Mask windows listed in the masking file
  maskDates = NULL
  if (!is.null(maskingFile)) {
    mask = data.table::fread(file = maskingFile, data.table = FALSE)
    if (id %in% mask$ID) {
      mask = mask[which(mask$ID == id),]
      tmp = which(Mshort$timenum %in% D$timenum[which(D$window %in% mask$window)])
      if (length(tmp) > 0)  {
        if (tmp[1] == 1) tmp = tmp[-1] # prevent entire first day to be excluded
        
        Mshort[tmp, grep(pattern = "time", x = colnames(Mshort))] = NA
      }
      tmp = which(LUXTEMP$timestamp %in% D$timenum[which(D$window %in% mask$window)])
      if (length(tmp) > 0)  {
        if (tmp[1] == 1) tmp = tmp[-1] # prevent entire first day to be excluded
        LUXTEMP[tmp, grep(pattern = "time", x = colnames(LUXTEMP))] = NA
      }
    }
  }
  #==================================
  # plots
  lab_cex_left = 1.1 #0.9
  lab_cex_right = 1.1 #0.8
  
  if (onlysleep == FALSE) {
    lab_cex_below1 = 1.4
    lab_cex_below2 = 1.4
  } else {
    lab_cex_below1 = 1.2
    lab_cex_below2 = 1.2
  }
  if (onlysleep == FALSE) {
    layout.matrix <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1)
    layout(mat = layout.matrix,
           heights = c(1.5, 1.5, 1.5, 2), # Heights rows
           widths = c(6)) # Widths columns
  }
  if (onlysleep == FALSE) {
    #----- LUX
    par(mar = c(1, 4, 1, 10))
    
    #=== ROW (Light)
    par(mar = c(0.5, 0.5, 0.1, 10), family = "serif", cex.lab = 1.3, cex.axis = 1, las = 1)
    plot(LUXTEMP$timestamp, LUXTEMP$lightpeak / 1000, type = "l",
         xlab = "", ylab = "", cex.main = 2,  axes = F, ylim = c(0,  20),
         font.lab = 2, lwd = 0.8, col = "gold2")
    # abline(v = Mshort$timenum[windowBorders2], lty = 2, col = "grey")
    abline(h = 1, lty = 2, col = "grey")
    mtext(labels[1, lang], side = 2, line = 1, las = 1, 
          cex = lab_cex_left, col = "black", outer = FALSE, font = 2)
    axis(4)
    mtext(text = c(labels[2, lang], labels[3, lang]), at = c(1, 19),
          side = 4, line = 3, las = 1, cex = lab_cex_right, col = "black", outer = FALSE)
    
    #=== ROW (Acceleration)
    plot(Mshort$timestamp, Mshort$ENMO * 1000, type = "l", xlab = "", ylab = "",
         axes = F, ylim = c(0, 200), font.lab = 2, col = "red", lwd = 0.8)
    mtext(labels[7, lang], adj = 1, side = 2, line = 1,
          las = 1, cex = lab_cex_left, col = "black", outer = FALSE, font = 2)
    axis(4)
    abline(h = 100, lty = 2, col = "grey")
    mtext(text = c(labels[8, lang], labels[9, lang]),
          at = c(10, 190),
          side = 4, line = 3, las = 1, cex = lab_cex_right,
          col = "black", outer = FALSE)
  }
  #=== ROW (Sleep)
  # Set all invalid epochs to NA
  plot(0:1, 0:1, type = "l", xlab = "", ylab = "", axes = F, 
       xlim = range(as.numeric(Mshort$timestamp[range(which(is.na(Mshort$timestamp) == FALSE))])),
       ylim = c(-95, 95), font.lab = 2, col = "white")
  for (window in c("day", "night")) {
    if (window == "day") {
      delta_sib = diff(c(0L, Mshort$sib_day, 0L))
      col12 = "purple"
    } else if (window == "night") {
      delta_sib = diff(c(0L, Mshort$sib_night, 0L))
      col12 = "blue"
    }
    x0 = Mshort$timestamp[which(delta_sib == 1L)]
    x1 = Mshort$timestamp[which(delta_sib == -1L) + 1]
    Nrect = min(length(x0),length(x1))
    if (Nrect > 0) {
      for (k in 1:Nrect) { # SIB during SPT
        rect_invalid = D$invalidepoch[which(delta_sib == 1L)[k]:which(delta_sib == -1L)[k]]
        if (length(which(rect_invalid == 1)) < length(which(rect_invalid == 0))) {
          rect(x0[k],y0,x1[k],y1, col = col12, border = FALSE)
        }
      }
    }
  }
  lines(Mshort$timestamp, Mshort$anglez, type = "l", lwd = 0.8)
  mtext(labels[4, lang], side = 2, line = 1, las = 1, cex = lab_cex_left, 
        col = "black", outer = FALSE, font = 2)
  
  if (onlysleep == TRUE) {
    legend_inset = c(-0.2, 0)
    legend_cex = 1.2
  } else {
    legend_inset = c(-0.12, 0)
    legend_cex = 1.4
  }
  legend("topright", legend = c(labels[5, lang], labels[6, lang]),
         col = c("purple", "blue"),lty = c(1, 1), lwd = c(2.5, 2.5),
         title = labels[4, lang], inset = legend_inset, xpd = TRUE, cex = legend_cex)
  if (onlysleep == FALSE) {
    #==== ROW (Circadian rhythm)
    par(mar = c(4, 0.5, 0.1, 10))
    LWD = 1.5
    
    plot(Mshort$timestamp, Mshort$original, type = "l", # pch = 20, cex = 0.2,
         ylab = labels[10, lang], xlab = "", axes = FALSE,
         lwd = 0.8, col = "lightgreen", ylim = c(0, log((500) + 1)))
    lines(Mshort$timestamp, Mshort$fittedY, type = "l", # pch = 20, cex = 0.2,
          col = "black", lty = 1, lwd = LWD)
  }
  axis(side = 1, line = 1, at = as.numeric(timeat2), labels = wkday,
       col = NA, col.ticks = NA, cex.axis = lab_cex_below1)
  axis(side = 1, line = 0, at = as.numeric(timeat), labels = rep("0h", length(timeat)),
       cex.axis = lab_cex_below2, tick = TRUE) #col = NA, col.ticks = NA,
  if (onlysleep == FALSE) {
    mtext(labels[12, lang], padj = 0, side = 2, line = 1, las = 1, 
          cex = lab_cex_left, col = "black", outer = FALSE, font = 2)
    mtext(labels[13, lang],
          side = 4, line = 1, las = 1, cex = lab_cex_right, col = "black", outer = FALSE)
  }
}