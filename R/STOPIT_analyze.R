#' STOPIT_analyze
#'
#' Analyze OSARI performance data for a single participant
#'
#' @param data refers to a dataframe containing participant's performance
#'
#' @return STOPIT_analyze will return a dataframe with a single row, containing the performance metrics for all the protocols completed by a given participant.
#'
#' @examples
#' example_STOP_IT_data <- "https://raw.githubusercontent.com/HeJasonL/BASTD/master/example-data/STOP-IT_raw.csv"
#' STOP_IT_data <- read.csv(example_STOP_IT_data, header = TRUE) #read the example STOP-IT data
#' STOPIT_analyze(data = STOP_IT_data) #STOPIT_analyze
#'
#' @export

STOPIT_analyze <- function(data){

  # setup -------------------------------------------------------------------
  stopit_data <- data

  # Convert the readout to universal columns names and values ---------------
  ID <- stopit_data$ID
  Block <- stopit_data$block
  Trial <- stopit_data$trial
  TrialType <- NA
  Stimulus <- stopit_data$stimulus
  Signal <- stopit_data$signal
  Correct <- stopit_data$correct
  Response <- stopit_data$respons
  RT <- stopit_data$rt
  RE <- stopit_data$re
  SSD <- stopit_data$ssd

  converted_stopit_data <- as.data.frame(cbind(ID, Block, Trial, Stimulus, Signal, Correct, Response, RT, RE, SSD, TrialType)) #create the dataframe used for BASTD_analyze
  analyzed_stopit_data <- BASTD_analyze(converted_stopit_data, 1200) #run the converted_osari_data through BASTD_analyze

  return(analyzed_stopit_data)
}



