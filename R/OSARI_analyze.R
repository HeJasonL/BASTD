#' OSARI_analyze
#'
#' Analyze OSARI performance data for a single participant
#'
#' @param dataframe refers to a dataframe containing participant's performance
#'
#' @return BASTD_analyze will return a dataframe with a single row, containing the performance metrics for all the protocols completed by a given participant.
#'
#' @examples
#'example_OSARI_data <- "https://raw.githubusercontent.com/HeJasonL/BASTD/master/example-data/OSARI_raw_OSARI_2020_Aug_25_1336.txt"
#'OSARI_data <- read.csv(example_OSARI_data, header = TRUE, sep = "\t")
#'OSARI_analyze(data = OSARI_data) #OSARI analyze
#'
#' @export

OSARI_analyze <- function(data){

# debugging ---------------------------------------------------------------
# example_OSARI_data <- "https://raw.githubusercontent.com/HeJasonL/BASTD/master/example-data/OSARI_raw_OSARI_2020_Aug_25_1336.txt"
# data <- read.csv(example_OSARI_data, header = TRUE, sep = "\t")

# setup -------------------------------------------------------------------
osari_data <- data

# Convert the readout to universal columns names and values ---------------
ID <- "JH"
Block <- osari_data$block
Trial <- osari_data$trial
TrialType <- osari_data$trialType
Stimulus <- NA
Signal <- osari_data$signal
Correct <- ifelse(osari_data$signal==1 & osari_data$response == 1, 0, 2)
Response <- osari_data$response
RT <- osari_data$rt * 1000
RE <- NA
SSD <- osari_data$ssd * 1000

converted_osari_data <- as.data.frame(cbind(ID, Block, Trial, Stimulus, Signal, Correct, Response, RT, RE, SSD, TrialType)) #create the dataframe used for BASTD_analyze
analyzed_osari_data <- BASTD_analyze(converted_osari_data, 1000) #run the converted_osari_data through BASTD_analyze

return(analyzed_osari_data)
}



