# functions & packages ---------------------------------------------------------------
# BASTD tester ------------------------------------------------------------
# install the latest version of the package -------------------------------
devtools::install_github("HeJasonL/BASTD", force = TRUE) #install latest version of BASTD from GitHub
library(BASTD) #read the package into the library

load.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[ , "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = T)
  }
  sapply(pkg, require, character.only = T)
}

packages <- c("here", "ggpubr")

load.packages(packages)

# setup -------------------------------------------------------------------

# STOP-IT -----------------------------------------------------------------
example_STOP_IT_data <- "https://raw.githubusercontent.com/HeJasonL/BASTD/master/example-data/STOP-IT_raw.csv"
STOP_IT_data <- read.csv(example_STOP_IT_data, header = TRUE) #read the example STOP-IT data
STOPIT_analyze(data = STOP_IT_data) #STOPIT_analyze

# OSARI  ------------------------------------------------------------------
OSARI_data <- read.csv(here::here("example-data", "s_0000_OSARI_2021_Mar_08_0807.txt"), header = TRUE, sep = "")
OSARI_analyze(data = OSARI_data) #OSARI analyze
OSARI_visualize(OSARI_data) #OSARI visualize
plot <- OSARI_visualize(OSARI_data) #OSARI visualize
pdf(paste0(OSARI_files[f],".pdf"), onefile = TRUE, width = 10, height = 5)

ggpubr::ggsave(plot, width = 10, height = 5, file = "OSARI_example_participant_plot.pdf")

# Analyze all examples (W.I.P) --------------------------------------------
OSARI_analyze_all("~/Documents/Documents - Jason's iCloud/Work/GitHub/BASTD/example-data")
OSARI_visualize_all("~/Documents/Documents - Jason's iCloud/Work/GitHub/BASTD/example-data")

