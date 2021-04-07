#' OSARI_analyze_all
#'
#' Analyze OSARI performance data for all participants
#'
#' @param dataframe refers to a dataframe containing participant's performance
#'
#' @return OSARI_analyze_all returns a dataframe with rows representing each participant's analyzed data (using the OSARI_analyze function)
#'
#' @examples
#'
#' @export

OSARI_analyze_all <- function(working_directory){

  OSARI_files <- list.files(pattern = "OSARI") #look for files with the pattern OSARI
  OSARI_analyzed_files <- list() #create a list for the analyzed OSARI files

  for(f in 1:length(OSARI_files)){
    data <- read.csv(OSARI_files[f], header = TRUE, sep = "")
    OSARI_analyzed_files[[f]] <- OSARI_analyze(data)
  }

  analyzed_osari_data_combined <- dplyr::bind_rows(OSARI_analyzed_files)

  dir.create("analyzed") #create a directory called 'analyzed'
  write.csv(analyzed_osari_data_combined, file = file.path(working_directory, "analyzed", "analyzed_OSARI_data.csv")) #save the file

  return(analyzed_osari_data_combined)
}



