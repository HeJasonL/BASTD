# functions & packages ---------------------------------------------------------------
load.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[ , "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = T)
  }
  sapply(pkg, require, character.only = T)
}

packages <- c("tidyverse", "nlme", "ggpubr", "Hmisc", "plyr", "Rmisc", "retimes", "data.table", "lme4","multcomp",
              "pastecs", "effects", "DataCombine", "gridExtra", "leaps", "ppcor", "ggm", "readxl", "emmeans",
              "eeptools", "psych","weights", "here", "cowplot", "reghelper", "sjstats", "devtools", "readr")

load.packages(packages)


# BASTD tester ------------------------------------------------------------
# install the latest version of the package -------------------------------
install_github("HeJasonL/BASTD", force = TRUE) #install latest version of BASTD from GitHub
library(BASTD) #read the package into the library

# setup -------------------------------------------------------------------


# STOP-IT -----------------------------------------------------------------
example_STOP_IT_data <- "https://raw.githubusercontent.com/HeJasonL/BASTD/master/example-data/STOP-IT_raw.csv"
STOP_IT_data <- read.csv(example_STOP_IT_data, header = TRUE) #read the example STOP-IT data
STOPIT_analyze(data = STOP_IT_data, maximum_go_trial_RT = 1200) #STOPIT_analyze

# OSARI  ------------------------------------------------------------------
example_OSARI_data <- "https://raw.githubusercontent.com/HeJasonL/BASTD/master/example-data/OSARI_raw_OSARI_2020_Aug_25_1336.txt"
OSARI_data <- read.csv(example_OSARI_data, header = TRUE, sep = "\t")
OSARI_analyze(data = OSARI_data) #OSARI analyze



# Analyze all examples (W.I.P) --------------------------------------------
# OSARI_analyze_all("~/Dropbox/Documents/Work/GitHub/BASTD/example-data") #OSARI analyze all
