#Below is a working example of how OSARI_analyze and OSARI_visualize are used:
  #If you have any difficulties with using the code, contact me at: jasonhe93@gmail.com
    #and I will provide support as quickly as possible
      #Otheerwise, if you are familiar with R, then you will be able to make sense of the functions in the 'R' folder of BASTD

# setup -------------------------------------------------------------------
# install the latest version of the package -------------------------------
devtools::install_github("HeJasonL/BASTD", force = TRUE) #install latest version of BASTD from GitHub
install.packages("here")
library(BASTD)
library(here)

# OSARI  ------------------------------------------------------------------
example_OSARI_data <- "https://raw.githubusercontent.com/HeJasonL/BASTD/master/example-data/OSARI_raw.txt" #read data in from my GitHub
OSARI_data <- read.csv(example_OSARI_data, header = TRUE, sep = "") #read the example STOP-IT data
OSARI_analyze(data = OSARI_data) #OSARI analyze
OSARI_visualize(OSARI_data) #OSARI visualize

# Analyze all examples ----------------------------------------------------
#OSARI_analyzed_all works by creating a folder called 'analyzed' in the working directory where you specify (within the brackets of the function)
#OSARI_visualized all is exactyl the same, except the folder is called 'visualized'
#These scripts work by looking for all the files in that folder with the term 'OSARI' and then analyzes or visualizes those data
OSARI_analyze_all(here("example-data")) #analyzed data will be saved as a .csv file
OSARI_visualize_all(here("example-data")) #visualized data will be saved as a .pdf file

# STOP-IT -----------------------------------------------------------------
#BASTD can also be used for traditional choice reaction stop signal task performance:
example_STOP_IT_data <- "https://raw.githubusercontent.com/HeJasonL/BASTD/master/example-data/STOP-IT_raw.csv"
STOP_IT_data <- read.csv(example_STOP_IT_data, header = TRUE) #read the example STOP-IT data
STOPIT_analyze(data = STOP_IT_data) #STOPIT_analyze
