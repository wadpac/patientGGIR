# patientReportGGIR

**patientReportGGIR** is an R package to help generate patient reports from the output of R package GGIR.

## Installation

- Install: https://miktex.org/download
- When installing on Windows:
  - Inside Miktex Console
  - Inside MikTex and under package, find `tabu`.
  - Click it, and press the "plus" to install it
  
- Inside RStudio:
  
```
install.packages(pkgs = "remotes",  dependencies = TRUE)
remotes::install_github("wadpac/patientReportGGIR", dependencies = TRUE)
```

## Usage

- Run GGIR as you normally do

- Create an R script where you define the `quartile_thresholds` to be used as reference:

```

quartile_thresholds = data.frame(MVPA = c("0:15", "0:25", "0:30"),
                                 LIPA = c("3:30", "4:30", "5:40"),
                                 SB = c("10:00", "10:30", "11:30"),
                                 Sleeptime = c("8:00", "8:30", "9:15"),
                                 SleepPercentage = c(90, 92,  95))
```

The above example would indicate that the threshold between the first and second quartile in time spent
in MVPA is 0 hours and 15 minutes.

- Next, specify the `createReport()` function call, including the path to the GGIR output folder:
```
createReport(GGIRoutputdir = "C:/output_pilot",
            lang = "fr",
            idsep = "_",
            desiredtz = "Europe/Paris",
            type = "onepage_luxsleepactcr_A4",
            quartile_thresholds = quartile_thresholds)
          
```

**Notes:**

- lang is the language, with options fr or en
- idsep is the separator for participant id in the file name, e.g. from 12_0872.bin the code will extract 12.
- desiredtz is the desired timezone in which data collection took place
- type is the type of report, currently only one report type facilitated