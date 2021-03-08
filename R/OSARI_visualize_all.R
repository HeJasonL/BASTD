#' OSARI_visualize_all
#'
#' Analyze OSARI performance data for all participants
#'
#' @param dataframe refers to a dataframe containing participant's performance
#'
#' @return OSARI_visualize_all returns a dataframe with rows representing each participant's analyzed data (using the OSARI_analyze function)
#'
#' @examples
#'
#' @export

OSARI_visualize_all <- function(working_directory){


  #working_directory <- here("example-data") #debugging
  setwd(working_directory) #setwd
  dir.create("visualized") #create a directory called 'visualized'

  #comment out lines below if not debugging
  OSARI_files <- list.files(pattern = "OSARI") #look for files with the pattern OSARI

  for(f in 1:length(OSARI_files)){
    data <- read.csv(OSARI_files[f], header = TRUE, sep = "") #read in the file
    plot <- OSARI_visualize(data) #analyze the file using OSARI_analyze
    setwd(paste0(working_directory, "/", "visualized"))
    print(paste0("Now plotting file: ", OSARI_files[f]))
    pdf(paste0(OSARI_files[f],".pdf"), onefile = TRUE, width = 10, height = 5)
    print(plot)
    dev.off()
    setwd(working_directory) #setwd
  }
}



