#' plot_lux_sleep_act_cr
#'
#' @param GGIRoutputdir Path to GGIR output folder, e.g. C:/output_mystudy
#' @param id Character, participant id
#' @param lang Character, language to use fr=french, en=english
#' @param desiredtz Character, timezone from timezone data base names, see also GGIR documentation.
#' @param maskingFile Character to point to csv file with dates to be masked per ID
#' @return no object is returned, only a plot is generated
#' @importFrom graphics abline axis layout legend lines mtext par rect
#' @importFrom stats aggregate
#' @importFrom utils read.csv
#' @export

plot_lux_sleep_act_cr = function(GGIRoutputdir, id, lang = "fr", desiredtz = "",
                                 maskingFile = NULL) {
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
  fns = fns[grep(pattern = "220169C", x = fns, value = FALSE, invert = TRUE)]
  
  reso = 300
  par(mfcol = c(4, 1), omi = rep(0, 4), oma = c(0, 9, 0, 4), mar = c(4, 4.5, 2, 9))
  
  # identify dates that will be table
  P5D = read.csv(file = paste0(GGIRoutputdir, "/results/part5_daysummary_WW_L40M100V400_T5A5.csv"))
  dates_in_table = as.Date(P5D[grep(pattern = id, x = P5D$ID), "calendar_date"])
  
  if (length(grep(pattern = "[.]RData", x = P5ts[grep(id, P5ts)])) > 0) {
    load(file = P5ts[grep(id, basename(P5ts))])
    D = mdat
  } else {
    D = read.csv(file = P5ts[grep(id, basename(P5ts))])
  }
  load(file = P1[grep(id,basename(P1))])
  # aggregate D
  D$timenum = floor(D$timenum / reso) * reso
  D$sib = 0
  D$sib[which(D$class_id == 0)] = 1
  D = aggregate(D[,c("ACC","window","sib","SleepPeriodTime", "invalidepoch")], by = list(D$timenum), FUN = mean)
  colnames(D)[1] = "timenum"
  D$time = as.POSIXlt(D$timenum, tz = desiredtz, origin = "1970-1-1")
  # Constrain time series to date range as found in cleaned part 5 report
  constrainDates = function(x, timecol = "time", dateRange) {
    x = x[which(as.Date(x[, timecol]) >= dateRange[1] &
                  as.Date(x[, timecol]) <= dateRange[2]), ]
  }
  D = constrainDates(x = D, timecol = "time", dateRange = range(dates_in_table))

  
  D$clocktime = format(D$time,"%H:%M")
  D$sib = round(D$sib)
  D$sib[which(D$SleepPeriodTime == 0)] = 0
  M$metashort$timestamp = as.POSIXlt(M$metashort$timestamp,
                                     format = "%Y-%m-%dT%H:%M:%S%z", desiredtz)
  # Constrain time series to date range as found in cleaned part 5 report
  M$metashort = constrainDates(x = M$metashort, timecol = "timestamp", dateRange = range(dates_in_table))
  M$metalong = constrainDates(x = M$metalong, timecol = "timestamp", dateRange = range(dates_in_table))
  LUXTEMP = M$metalong
  Mshort = M$metashort
  
  LUXTEMP$timestamp = as.POSIXlt(LUXTEMP$timestamp, format = "%Y-%m-%dT%H:%M:%S%z", desiredtz)
  
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
  tickmarks = which(D$clocktime == "00:00" & D$windwow) # long tickmarks
  timeat = D$time[tickmarks]
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
  
  dayborders = which(D$clocktime == "00:00")
  dayborders2 = which(format(Mshort$timestamp,"%H:%M") == "00:00")
  # Night time sib blocks
  Mshort$sib_day = Mshort$sibs
  Mshort$sib_night = Mshort$sibs
  Mshort$sib_night[which(D$SleepPeriodTime == 0)] = 0
  Mshort$sib_day[which(D$SleepPeriodTime == 1)] = 0
  Mshort$sib_day
  y0 = 90
  y1 = -90
  # Set all invalid epochs to NA
  if (1 %in% D$invalidepoch) {
    Mshort[which(D$invalidepoch == 1), grep(pattern = "time", x = colnames(Mshort))] = NA
    LUXTEMP[which(LUXTEMP$nonwearscore >= 2), grep(pattern = "time", x = colnames(LUXTEMP))] = NA
  }
  
  # prepare Circam data
  fn2 = grep(pattern = as.character(id), x = fns, value =  TRUE)
  load(fn2)
  cosinor_ts = SUM$cosinor_ts
  
  # attempt to align cosinor timeseries
  cosvars = SUM$summary[grep(pattern = "cosinor", x = names(SUM$summary), value = TRUE)]
  minute_0 = as.numeric(format(Mshort$timestamp[1], "%H")) * 60 + as.numeric(format(Mshort$timestamp[1], "%M"))
  minute_1 = minute_0 + floor((nrow(Mshort) * reso) / 60)
  cosinor_ts = cosinor_ts[minute_0:minute_1,]
  
  drops = which(cosinor_ts$time[2:nrow(cosinor_ts)] < cosinor_ts$time[1:(nrow(cosinor_ts) - 1)]) + 1
  for (k in drops) {
    cosinor_ts$time[k:nrow(cosinor_ts)] = cosinor_ts$time[k:nrow(cosinor_ts)] + 24
  }
  cosinor_ts$time_sec =  cosinor_ts$time * 3600
  cosinor_ts$time_epoch = round(cosinor_ts$time_sec / reso) * reso
  cosinor_ts2 = aggregate(x = cosinor_ts, by = list(cosinor_ts$time_epoch), FUN = mean)
  if (nrow(cosinor_ts2) > nrow(Mshort)) {
    cosinor_ts2 = cosinor_ts2[1:nrow(Mshort),]
  }
  
  deltatime = table(diff(as.numeric(cosinor_ts2$time_epoch)))
  if (length(deltatime) > 1) {
    epochSize = as.numeric(names(deltatime)[1])
    NEWTIME = seq(cosinor_ts2$time_epoch[1], cosinor_ts2$time_epoch[nrow(cosinor_ts2)], by = epochSize)
    tmpdf = data.frame(time_epoch = NEWTIME)
    cosinor_ts2 = merge(tmpdf, cosinor_ts2, by = "time_epoch", all.x = TRUE)
  }
  cosinor_ts2$timestamp = Mshort$timestamp[1] + cosinor_ts2$time_epoch
  cosinor_ts2 =  cosinor_ts2[, c("timestamp", "original", "fittedY")]
  Mshort = merge(Mshort, cosinor_ts2, by = "timestamp", all.x = TRUE)
  # Set all invalid epochs to NA
  if (1 %in% D$invalidepoch) {
    Mshort[which(D$invalidepoch == 1), grep(pattern = "time", x = colnames(Mshort))] = NA
  }
  
  
  # Mask dates listed in the masking file
  maskDates = NULL
  if (!is.null(maskingFile)) {
    mask = data.table::fread(file = maskingFile, data.table = FALSE)
    if (id %in% mask$ID) {
      mask = mask[which(mask$ID == id),]
      if (length(grep("/", mask$date)) > 0) {
        dsep = "/"
      } else {
        dsep = "-"
      }
      maskDates = as.Date(mask$date, paste0("%d", dsep, "%m", dsep, "%Y"))
      tmp = which(as.Date(Mshort$timestamp) %in% maskDates)
      if (tmp[1] == 1) tmp = tmp[-1] # prevent entire first day to be excluded
      if (length(tmp) > 0)  {
        Mshort[tmp, grep(pattern = "time", x = colnames(Mshort))] = NA
      }
      tmp = which(as.Date(LUXTEMP$timestamp) %in% maskDates)
      if (tmp[1] == 1) tmp = tmp[-1] # prevent entire first day to be excluded
      if (length(tmp) > 0)  {
        LUXTEMP[tmp, grep(pattern = "time", x = colnames(LUXTEMP))] = NA
      }
    }
  }
  #==================================
  # plots
  lab_cex_left = 0.9
  lab_cex_right = 0.8
  layout.matrix <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1)
  layout(mat = layout.matrix,
         heights = c(1.5, 1.5, 1.5, 2), # Heights rows
         widths = c(6)) # Widths columns
  #----- LUX
  par(mar = c(1, 4, 1, 9))
  
  #=== ROW (Light)
  par(mar = c(0.5, 0.5, 0.1, 9), family = "serif", cex.lab = 1.3, cex.axis = 1, las = 1)
  plot(LUXTEMP$timestamp, LUXTEMP$lightpeak / 1000, type = "l",
       xlab = "", ylab = "", cex.main = 2,  axes = F, ylim = c(0,  20),
       font.lab = 2, lwd = 0.8, col = "gold2")
  abline(v = Mshort$timenum[dayborders2], lty = 2, col = "grey")
  abline(h = 1, lty = 2, col = "grey")
  mtext(labels[1, lang], side = 2, line = 1, las = 1, 
        cex = lab_cex_left, col = "black", outer = FALSE, font = 2)
  axis(4)
  mtext(text = c(labels[2, lang], labels[3, lang]), at = c(1, 19),
        side = 4, line = 3, las = 1, cex = lab_cex_right, col = "black", outer = FALSE)

  #=== ROW (Sleep)
  # Set all invalid epochs to NA
  plot(0:1, 0:1, type = "l", xlab = "", ylab = "", axes = F, 
       xlim = range(as.numeric(Mshort$timestamp[range(which(is.na(Mshort$timestamp) == FALSE))])),
       ylim = c(-90, 90), font.lab = 2, col = "white")
  for (window in c("day", "night")) {
    if (window == "day") {
      delta_sib = diff(c(0L, Mshort$sib_day, 0L))
      col12 = "purple"
    } else if (window == "night") {
      delta_sib = diff(c(0L, Mshort$sib_night, 0L))
      col12 = "blue"
    }
    x0 = Mshort$timestamp[which(delta_sib == 1L)]
    x1 = Mshort$timestamp[which(delta_sib == -1L)]
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
  abline(v = Mshort$timenum[dayborders2], lty = 2, col = "grey")
  mtext(labels[4, lang], side = 2, line = 1, las = 1, cex = lab_cex_left, 
        col = "black", outer = FALSE, font = 2)
  legend("topright", legend = c(labels[5, lang], labels[6, lang]),
         col = c("purple", "blue"),lty = c(1, 1), lwd = c(1.5, 1.5),
         title = labels[4, lang], inset = c(-0.2, 0), xpd = TRUE, cex = 1.1)
  
  #=== ROW (Acceleration)
  plot(Mshort$timestamp, Mshort$ENMO * 1000, type = "l", xlab = "", ylab = "",
       axes = F, ylim = c(0, 200), font.lab = 2, col = "red", lwd = 0.8)
  mtext(labels[7, lang], adj = 1, side = 2, line = 1,
        las = 1, cex = lab_cex_left, col = "black", outer = FALSE, font = 2)
  axis(4)
  abline(h = 100, lty = 2, col = "grey")
  abline(v = Mshort$timenum[dayborders2], lty = 2, col = "grey")
  mtext(text = c(labels[8, lang], labels[9, lang]),
        at = c(10, 190),
        side = 4, line = 3, las = 1, cex = lab_cex_right,
        col = "black", outer = FALSE)
  
  #==== ROW (Circadian rhythm)
  par(mar = c(4, 0.5, 0.1, 9))
  LWD = 1.5
  
  plot(Mshort$timestamp, Mshort$original, type = "l", # pch = 20, cex = 0.2,
       ylab = labels[10, lang], xlab = "", axes = FALSE,
       lwd = 0.8, col = "lightgreen", ylim = c(0, log((500) + 1)))
  lines(Mshort$timestamp, Mshort$fittedY, type = "l", # pch = 20, cex = 0.2,
        col = "black", lty = 1, lwd = LWD)
  # lines(Mshort$timestamp, Mshort$fittedYext, type = "l",
  #       col = "black", lty = 3, lwd = LWD)
  axis(side = 1, line = 0, at = as.numeric(timeat2), labels = wkday,
       col = NA, col.ticks = NA, cex.axis = 1.1)
  abline(v = D$timenum[dayborders], lty = 2, col = "grey")
  mtext(labels[12, lang], padj = 0, side = 2, line = 1, las = 1, 
        cex = lab_cex_left, col = "black", outer = FALSE, font = 2)
  mtext(labels[13, lang],
        side = 4, line = 1, las = 1, cex = lab_cex_right, col = "black", outer = FALSE)
}