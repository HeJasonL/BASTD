#' OSARI_visualize_all
#'
#' Analyze OSARI performance data for all participants
#'
#' @param dataframe refers to a dataframe containing participant's performance
#'
#' @return OSARI_visualize_all returns a dataframe with rows representing each participant's analyzed data (using the OSARI_analyze function)
#'
#' @examples
#' Examples are currently NA
#'
#' @export

OSARI_analyze_all <- function(working_directory){
  # debugging ---------------------------------------------------------------
  setwd(working_directory)
  #comment out lines below if not debugging
  OSARI_files <- list.files(pattern = "OSARI") #look for files with the pattern OSARI

  for(f in 1:length(OSARI_files)){
    data <- read.csv(OSARI_files[f], header = TRUE, sep = "\t") #read in the file
    performance <- OSARI_analyze(data) #analyze the file using OSARI_analyze
  }

  analyzed_osari_data_combined <- dplyr::bind_rows(OSARI_analyzed_files)

  dir.create("analyzed") #create a directory called 'analyzed'
  write.csv(OSARI_data, file = file.path(working_directory, "analyzed", "analyzed_OSARI_data.csv")) #save the file

  return(analyzed_osari_data_combined)

}



