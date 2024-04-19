# patientGGIR

**patientGGIR** is an R package to help generate patient reports from the output of R package GGIR.

## Installation

- Install: https://miktex.org/download
- When installing on Windows:
  - Inside MIktex Console
  - Under package, find `tabu`
  - Click it, and press the "plus" to install it
  
- Inside RStudio:
  
```
install.packages("remotes")
remotes::install_github("wadpac/patientGGIR")
```

## Usage

- Run GGIR as you normally do

- Specify path to GGIR output folder, e.g.

```
creatReport(GGIRoutputdir = "C:/output_pilot",
            lang = "fr",
            idsep = "_",
            desiredtz = "Europe/Paris",
            type = "onepage_luxsleepactcr_A4")
          
```

**Notes:**

- lang is the language, with options fr or en
- idsep is the separator for participant id in the file name, e.g. from 12_0872.bin the code will extract 12.
- desiredtz is the desired timezone in which data collection took place
- type is the type of report, currently only one report type facilitated