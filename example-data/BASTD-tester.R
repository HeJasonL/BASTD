# functions & packages ---------------------------------------------------------------
# load.packages <- function(pkg) {
#   new.pkg <- pkg[!(pkg %in% installed.packages()[ , "Package"])]
#   if (length(new.pkg)) {
#     install.packages(new.pkg, dependencies = T)
#   }
#   sapply(pkg, require, character.only = T)
# }
#
# packages <- c("tidyverse", "nlme", "ggpubr", "Hmisc", "plyr", "Rmisc", "retimes", "data.table", "lme4","multcomp",
#               "pastecs", "effects", "DataCombine", "gridExtra", "leaps", "ppcor", "ggm", "readxl", "emmeans",
#               "eeptools", "psych","weights", "here", "cowplot", "reghelper", "sjstats", "devtools", "reader")
#
# load.packages(packages)

#To install the retimes package, required for BASTD, you will need to install retimes from the CRAN archive
#retimes will require you to have Xcode install (see: https://stackoverflow.com/questions/24194409/how-do-i-install-a-package-that-has-been-archived-from-cran)
# install_url("https://cran.r-project.org/src/contrib/Archive/retimes/retimes_0.1-2.tar.gz") #this will install xcode if you do not already have it installed, followed by retimes
# library(retimes) #initialise retimes

# BASTD tester ------------------------------------------------------------
# install the latest version of the package -------------------------------
install_github("HeJasonL/BASTD", force = TRUE) #install latest version of BASTD from GitHub
library(BASTD) #read the package into the library

# OSARI  ------------------------------------------------------------------
example_OSARI_data <- "https://raw.githubusercontent.com/HeJasonL/BASTD/master/example-data/OSARI_raw.txt"
OSARI_data <- read.csv(example_OSARI_data, header = TRUE, sep = "") #read the example OSARI data
OSARI_analyze(data = OSARI_data) #OSARI analyze


# OSARI visualized --------------------------------------------------------
OSARI_visualize(data = OSARI_data)


