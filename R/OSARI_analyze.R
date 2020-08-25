#' BASTD_analyze
#'
#' Analyze extracted data for from "BATD_extract_NF" or "BATD_extract_OF" for a single participant. 
#' BASTD_analyze is used to analyze the datafrom a single participant. In order to analyze data from multiple participants, we recommend users use "BASTD_analyze_all".
#'
#' @param dataframe refers to a dataframe containing participant's performance 
#'
#' @return BASTD_analyze will return a dataframe with a single row, containing the performance metrics for all the protocols completed by a given participant.
#'
#' @examples
#' Examples are currently NA
#'
#' @export

OSARI_analyze <- function(data, maximum_go_trial_RT){

# debugging ---------------------------------------------------------------
#dataframe <- read.csv("STOP-IT_RAW.csv", header = TRUE) #read in thbe raw csv file
  
data <- data #assign data to 'data'
maximum_go_trial_RT <- maximum_go_trial_RT
names(data) <- c("ID", "Block", "Trial","Stimulus","Signal","Correct","Response","RT","RE","SSD") #rename the columns
id_number <- data$ID[[1]] #store the participant id number 

# Analyse performance  ----------------------------------------------------

# Standard Go trial performance outcomes ---------------------------------

#Go Performance
all_go_trials <- data[data$Signal==0,] #subset dataframe to only go trials 
n_of_go_trials <- nrow(all_go_trials) #number of go trials 

#accuracy and omissions 
go_trial_accuracy <- (nrow(all_go_trials[all_go_trials$Correct==2,])/n_of_go_trials) * 100
omission_error <- (nrow(all_go_trials[all_go_trials$Response==0,])/n_of_go_trials) * 100

#Go trial RT irrespective of accuracy
go_trial_RT_mean <- mean(as.numeric(as.character(all_go_trials$RT))) 
go_trial_RT_sd <- sd(as.numeric(as.character(all_go_trials$RT))) 

#Accurate Go trial RT
accurate_go_trial_RT_mean <- mean(as.numeric(as.character(all_go_trials$RT[all_go_trials$Correct==2]))) 
accurate_go_trial_RT_sd <- sd(as.numeric(as.character(all_go_trials$RT[all_go_trials$Correct==2])))

#Inaccurate Go trial RT
inaccurate_go_trial_RT_mean <- mean(as.numeric(as.character(all_go_trials$RT[all_go_trials$Response!=0 & all_go_trials$Correct==0])))
inaccurate_go_trial_RT_sd <- sd(as.numeric(as.character(all_go_trials$RT[all_go_trials$Response!=0 & all_go_trials$Correct==0])))

#Accurate Go trial RT with omissions replaced
all_go_trials_omissions_replaced_with_max_duration <- all_go_trials
all_go_trials_omissions_replaced_with_max_duration$RT[all_go_trials_omissions_replaced_with_max_duration$Response==0] <- maximum_go_trial_RT #replace any omission errors with the maximum go trial RT
all_go_trials_omissions_replaced_with_max_duration$Correct[all_go_trials_omissions_replaced_with_max_duration$Response==0] <- 2 #replace any omission errors that are deemed to be inaccurate to be accurate

go_trial_RT_mean_omissions_replaced <- mean(as.numeric(as.character(all_go_trials_omissions_replaced_with_max_duration$RT))) #RT with omission errors replaced as maximum go trial RT (using all trials regardless of accuracy)
go_trial_RT_sd_omissions_replaced <- sd(as.numeric(as.character(all_go_trials_omissions_replaced_with_max_duration$RT))) #RT with omission errors replaced as maximum go trial RT (using all trials regardless of accuracy)

accurate_go_trial_RT_mean_omissions_replaced <- mean(as.numeric(as.character(all_go_trials_omissions_replaced_with_max_duration$RT[all_go_trials_omissions_replaced_with_max_duration$Correct==2]))) #RT with omission errors replaced as maximum go trial RT (using only accurate trials)
accurate_go_trial_RT_sd_omissions_replaced <- sd(as.numeric(as.character(all_go_trials_omissions_replaced_with_max_duration$RT[all_go_trials_omissions_replaced_with_max_duration$Correct==2]))) #RT with omission errors replaced as maximum go trial RT (using only accurate trials)

#combine all the go trial performance data 
go_trial_performance_outcomes <- cbind(go_trial_accuracy, omission_error, 
                                       go_trial_RT_mean, go_trial_RT_sd,
                                       accurate_go_trial_RT_mean, accurate_go_trial_RT_sd,
                                       inaccurate_go_trial_RT_mean, inaccurate_go_trial_RT_sd, 
                                       go_trial_RT_mean_omissions_replaced, go_trial_RT_sd_omissions_replaced,
                                       accurate_go_trial_RT_mean_omissions_replaced, accurate_go_trial_RT_sd_omissions_replaced)

# Standard Stop trial performance outcomes --------------------------------

#Stop Performance 
all_stop_trials <- data[data$Signal==1,] #subset dataframe to only go trials 
n_of_stop_trials <- nrow(all_stop_trials) #number of go trials 

#Accuracy 
stop_trial_accuracy <- (nrow(all_stop_trials[all_stop_trials$Correct==2,]))/(nrow(all_stop_trials)) * 100

#Failed RTs 
failed_stop_RT_mean <- mean(as.numeric(as.character(all_stop_trials$RT[all_stop_trials$Correct==0])))
failed_stop_RT_sd <- sd(as.numeric(as.character(all_stop_trials$RT[all_stop_trials$Correct==0])))

# # Failed RTs by SSD (W.I.P)
#unique_ssds <- unique(all_stop_trials$SSD[all_stop_trials$Correct==0]) #list the unique ssds
# list_for_RTs_by_ssd <- list()
#  for(u in 1:length(unique_ssds)){
#    current_ssd_subset <- all_stop_trials[all_stop_trials$SSD == unique_ssds[u],]
#    mean <- mean(as.numeric((as.character(current_ssd_subset$RT))))
#    ssd <- as.character(unique_ssds[[u]])
#    failed_stop_RT_mean_at_current_ssd <- as.data.frame(cbind(mean, ssd))
# 
#    list_for_RTs_by_ssd[[u]] <- failed_stop_RT_mean_at_current_ssd
#    # assign(paste0("failed_stop_RT_", ssd, "_mean"), mean)
#  }
# 
# list_for_RTs_by_ssd

stop_trial_performance_outcomes <- cbind(stop_trial_accuracy, failed_stop_RT_mean, failed_stop_RT_sd)

# Ex-Gaussian parameters of Go RT distribution  -------------------------

#Go trial RT irrespective of accuracy
exGaus_go_trial_RT <- retimes::mexgauss(as.numeric(as.character(all_go_trials$RT)))
mu_go <- exGaus_go_trial_RT[[1]]
sigma_go <- exGaus_go_trial_RT[[2]]
tau_go <- exGaus_go_trial_RT[[3]]

#Accurate Go trial RT
accurate_go_trial_RTs <- as.numeric(as.character(all_go_trials$RT[all_go_trials$Response!=0 & all_go_trials$Correct==2]))
if(length(accurate_go_trial_RTs) > 1){
  exGaus_accurate_go_trial_RT <- retimes::mexgauss(accurate_go_trial_RTs)
  mu_accurate_go <- exGaus_accurate_go_trial_RT[[1]]
  sigma_accurate_go <- exGaus_accurate_go_trial_RT[[2]]
  tau_accurate_go <- exGaus_accurate_go_trial_RT[[3]]
} else {
  mu_accurate_go <- NA
  sigma_accurate_go <- NA
  tau_accurate_go <- NA
}

#Inaccurate Go trial RT
inaccurate_go_trial_RTs <- as.numeric(as.character(all_go_trials$RT[all_go_trials$Response!=0 & all_go_trials$Correct==0]))
if(length(inaccurate_go_trial_RTs) > 1){
  exGaus_inaccurate_go_trial_RT <- retimes::mexgauss(inaccurate_go_trial_RTs)
  mu_inaccurate_go <- exGaus_inaccurate_go_trial_RT[[1]]
  sigma_inaccurate_go <- exGaus_inaccurate_go_trial_RT[[2]]
  tau_inaccurate_go <- exGaus_inaccurate_go_trial_RT[[3]]
} else {
  mu_inaccurate_go <- NA
  sigma_inaccurate_go <- NA
  tau_inaccurate_go <- NA
}

exGaussian_go_outcomes <- cbind(mu_go, sigma_go, tau_go, 
                                mu_accurate_go, sigma_accurate_go, tau_accurate_go)


# Ex-Gaussian parameters of SSRT distribution (W.I.P) ---------------------


# SSRT --------------------------------------------------------------------
unique_ssds <- unique(all_stop_trials$SSD) #list the unique ssds
mean_ssd <- mean(as.numeric(as.character(all_stop_trials$SSD))) #mean SSD 
mean_presp <- mean(as.numeric(as.character(all_stop_trials$Correct))/2) #nb: divide by 2 is because correct = 2

#estimating SSRTs using the mean method 
go_trial_RT_SSRT_mean_method <- go_trial_RT_mean - mean_ssd #SSRT irrespective of Go trial accuracy
accurate_go_trial_SSRT_mean_method <- accurate_go_trial_RT_mean - mean_ssd #Accurate Go trial SSRT
go_trial_RT_SSRT_mean_method_omissions_replaced <- go_trial_RT_mean_omissions_replaced - mean_ssd #Go trial SSRT with omissions replaced
accurate_go_trial_SSRT_mean_method_omissions_replaced <- accurate_go_trial_RT_mean_omissions_replaced - mean_ssd #Accurate Go trial SSRT with omissions replaced

#estimating SSRTs using the integration method 
nth_all_go_RTs <- quantile(as.numeric(as.character(all_go_trials$RT)), probs = mean_presp, type = 6)
nth_correct_go_RTs <- quantile(as.numeric(as.character(all_go_trials$RT[all_go_trials$Correct==2])), probs = mean_presp, type = 6)
nth_correct_go_RTs_with_omissions_replaced <- quantile(as.numeric(as.character(all_go_trials_omissions_replaced_with_max_duration$RT[all_go_trials_omissions_replaced_with_max_duration$Correct==2])), probs = mean_presp, type = 6)

all_go_SSRT_integration_method <- nth_all_go_RTs - mean_ssd #SSRT regardless of go accuracy
correct_go_SSRT_integration_method <- nth_correct_go_RTs - mean_ssd #SSRT using only accurate go RTs
correct_go_SSRT_omissions_replaced_integration_method <- nth_correct_go_RTs_with_omissions_replaced - mean_ssd #SSRT using only accurate go RTs with omission errors being replaced with maximum go RT

SSRT_outcomes <- cbind(go_trial_RT_SSRT_mean_method, accurate_go_trial_SSRT_mean_method, 
                       go_trial_RT_SSRT_mean_method_omissions_replaced, accurate_go_trial_SSRT_mean_method_omissions_replaced,
                       all_go_SSRT_integration_method, correct_go_SSRT_integration_method, correct_go_SSRT_omissions_replaced_integration_method)



all_outcomes <- as.data.frame(cbind(id_number, go_trial_performance_outcomes, exGaussian_go_outcomes, SSRT_outcomes))
row.names(all_outcomes) <- c() #clear the row name

return(all_outcomes)
}



