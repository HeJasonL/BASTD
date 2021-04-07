#' OSARI_visualize
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
#'OSARI_visualize(OSARI_data) #OSARI visualize
#'
#' @export

OSARI_visualize <- function(data){

  # setup -------------------------------------------------------------------
  osari_data <- data #OSARI_data

  # Convert the readout to universal columns names and values ---------------
  ID <- "Example Participant"
  Block <- osari_data$block  #adding these two columns will give you Block
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
  converted_osari_data$trial_number <- 1:nrow(converted_osari_data)
  block_end <- list()


  unique_blockTypes <- unique(converted_osari_data$TrialType)

  #OSARI's block column is the repetition of that block for the given trial type (or block type), rather than a continuous running value
  #The code below turns it into a continuing running value
  continuous_blocks <- list()
  for(t in 1:length(unique_blockTypes)){
    current_block_type <- converted_osari_data[converted_osari_data$TrialType == unique_blockTypes[t],]
    continuous_blocks[[t]] <- rep(t, nrow(current_block_type))
  }

  converted_osari_data$Block <- unlist(continuous_blocks) + as.numeric(converted_osari_data$Block)

  block_end <- list()
  for(b in 1:length(unique(converted_osari_data$Block))){
    current_block <- converted_osari_data[converted_osari_data$Block==b,]
    block_end[b] <- as.numeric(as.character(current_block$trial_number[nrow(current_block)]))
  }

# analyze osari_data ------------------------------------------------------
  analyzed_osari_data <- BASTD_analyze(converted_osari_data, 1000)

  #The Procedure
  number_of_blocks <- analyzed_osari_data$number_of_blocks
  number_of_go_per_block <- analyzed_osari_data$number_of_go_trials_per_block
  number_of_stop_per_block <- analyzed_osari_data$number_of_stop_trials_per_block

  the_procedure <- cbind(number_of_blocks, number_of_go_per_block, number_of_stop_per_block)

  #Convert all relevant columns to numeric first
  analyzed_osari_data[2:ncol(analyzed_osari_data)] <- lapply(analyzed_osari_data[2:ncol(analyzed_osari_data)], as.numeric)

  #Descriptive statistics
  go_omissions <- analyzed_osari_data$omission_error
  go_accuracy <- analyzed_osari_data$go_trial_accuracy
  mean_go_RT <- round(analyzed_osari_data$go_trial_RT_mean_omissions_replaced,2)
  sd_go_RT <- ifelse(is.null(analyzed_osari_data$go_trial_RT_sd_omissions_replaced), NA, round(analyzed_osari_data$go_trial_RT_sd_omissions_replaced,2))
  mean_SSD <- ifelse(is.null(analyzed_osari_data$mean_presp), NA, round(1000 * analyzed_osari_data$mean_presp,2))
  SSRT <- ifelse(is.null(analyzed_osari_data$correct_go_SSRT_omissions_replaced_integration_method), NA, round(analyzed_osari_data$correct_go_SSRT_omissions_replaced_integration_method,2))
  mean_failed_stop_RT <- ifelse(is.null(analyzed_osari_data$failed_stop_RT_mean),  NA, round(analyzed_osari_data$failed_stop_RT_mean,2))
  sd_failed_stop_RT <- ifelse(is.null(analyzed_osari_data$failed_stop_RT_sd), NA, round(analyzed_osari_data$failed_stop_RT_sd,2))

  plotting_data <- converted_osari_data #subset to all the Go-trials

  # Fix the variable class
  plotting_data$RT <- as.numeric(as.character(plotting_data$RT))
  plotting_data$SSD <- as.numeric(as.character(plotting_data$SSD))
  plotting_data$Trial <- 1:nrow(plotting_data) #change trial so that it is continuous regardless of block
  # plotting_data$TrialType <- TrialType #reapply trial type (conversion to dataframe changed the string to factor)

  unique_blocks <- unique(plotting_data$Block)
  nrow_for_each_block_list <- list()
  for(u in 1:length(unique_blocks)){
    nrow_for_each_block_list[[u]] <- nrow(plotting_data[plotting_data$Block==unique_blocks[u],])
  }

# visualize osari data ----------------------------------------------------
# Panel a -----------------------------------------------------------------
  panel_a_descriptives_text <-  paste("Go Accuracy (%):", go_accuracy, "\n",
                                      "Mean Go RT (ms):", mean_go_RT, "\n",
                                      "SD Go RT (ms):", sd_go_RT, "\n")

  panel_a <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::labs(tag = "a") +
    ggplot2::annotate("text", x = 10, y = 10, label = panel_a_descriptives_text) +
    ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
                   panel.border=ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   plot.tag = ggplot2::element_text(face = "bold"))

# Panel b -----------------------------------------------------------------
    panel_b <- ggplot2::ggplot(plotting_data, ggplot2::aes(x=Trial, y=RT, color = Correct)) +
    ggplot2::geom_point() +ggplot2::theme(legend.position="none") +
    ggplot2::geom_vline(xintercept = c(unlist(block_end)), linetype="dotted") +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::labs(y = "RT (ms)", x = "Trial number", tag = "b") +
    ggplot2::ylim(0,1000) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
             panel.grid.minor = ggplot2::element_blank(),
             panel.background = ggplot2::element_blank(),
             axis.line = ggplot2::element_line(colour = "black"),
             plot.tag = ggplot2::element_text(face = "bold"),
             legend.position="none")

# Panel c -----------------------------------------------------------------
    panel_c <- ggplot2::ggplot(plotting_data, ggplot2::aes(x=RT, color = Correct, fill = Correct)) +
    ggplot2::geom_density(alpha = .3) +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::scale_fill_manual(values = c("red", "blue")) +
    ggplot2::labs(y = "Density", x = "RT (ms)", tag = "c") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            plot.tag = ggplot2::element_text(face = "bold"),
            legend.position="none")


# Panel d -----------------------------------------------------------------
  panel_d_procedure_text <-  paste("Mean SSD (ms):", mean_SSD, "\n",
                                   "SSRT (ms):", SSRT, "\n",
                                   "Mean Failed Stop RT (ms):", mean_failed_stop_RT, "\n",
                                   "SD Failed Stop RT (ms):", sd_failed_stop_RT)

  panel_d <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::labs(tag = "d") +
    ggplot2::annotate("text", x = 10, y = 10, label = panel_d_procedure_text) +
    ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
                   panel.border=ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   plot.tag = ggplot2::element_text(face = "bold"))




# Panel e -----------------------------------------------------------------
stop_data <-  plotting_data[plotting_data$Signal==1,]

panel_e <- ggplot2::ggplot(plotting_data, ggplot2::aes(x=Trial, y= SSD, color = Correct)) +
  ggplot2::geom_point()+
  ggplot2::geom_vline(xintercept = c(unlist(block_end)), linetype="dotted") +
  ggplot2::scale_color_manual(values = c("red", "blue")) +
  ggplot2::scale_fill_manual(values = c("red", "blue")) +
  ggplot2::labs(y = "SSD (ms)", x = "Trial number", tag = "e") +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        plot.tag = ggplot2::element_text(face = "bold"),
        legend.position="none")

# Panel f -----------------------------------------------------------------

  #Create a SSD table
   ssd_data <- as.data.frame(table(plotting_data$SSD, plotting_data$Correct))
   colnames(ssd_data) <- c("SSD", "Correct", "Frequency") #Rename those columns so they make sense
   ssd_data$SSD <- as.numeric(as.character(ssd_data$SSD))
   nrow_dataframe <- nrow(ssd_data)/2 #calculate how many rows makes up half the dataframe to split the ssd table
   ssd_correct <- ssd_data[1:nrow_dataframe,] #create a correct ssd dataframe
   ssd_incorrect <- ssd_data[nrow_dataframe + 1:nrow_dataframe,] #create an incorrect ssd dataframe

   ssd_table <- as.data.frame(ssd_correct$SSD) #create a new dataframe called ssd_table to work from
   ssd_table$correct <- ssd_correct$Frequency #create a column for correct responses
   ssd_table$incorrect <- ssd_incorrect$Frequency #create a column for incorrect responses
   colnames(ssd_table) <- colnames(ssd_data) <- c("SSD", "Correct", "Incorrect") #Rename those columns so they make sense
   ssd_table$Total <- ssd_table$Correct + ssd_table$Incorrect #Calculate the total numnber of times each SSD was presented
   ssd_table$PRespond <- 1 - ssd_table$Incorrect/ssd_table$Total #Estimate the probability of responding across each of the SSDs

   panel_f <- ggplot2::ggplot(ssd_table, ggplot2::aes(x=SSD, y = PRespond)) +
     ggplot2::geom_point(size = 2) + ggplot2::geom_line() +
     ggplot2::labs(y = "P(Respond)", x = "SSD (ms)", tag = "f") +
     ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank(),
                    axis.line = ggplot2::element_line(colour = "black"),
                    plot.tag = ggplot2::element_text(face = "bold"),
                    legend.position="none")

# Combine plots -----------------------------------------------------------
figures_combined <- ggpubr::ggarrange(panel_a, panel_b, panel_c,
                              panel_d, panel_e, panel_f,
                              nrow = 2, ncol = 3)

figures_combined_with_participant_id <- ggpubr::annotate_figure(figures_combined,
                                                        top = ggpubr::text_grob(paste("Participant ID: ",  converted_osari_data$ID[1]), size = 10))

figures_combined_with_participant_id_and_title <- ggpubr::annotate_figure(figures_combined_with_participant_id,
                           top = ggpubr::text_grob("OSARI_visualize(d)",
                                                   color = "black",
                                                   face = "bold",
                                                   size = 15))

final <- ggpubr::annotate_figure(figures_combined_with_participant_id_and_title,
                                  bottom = ggpubr::text_grob("a) Descriptive statistics for Go trials. b) Go trial RT across trials. c) Density plots for Go trial RT. d) Descriptive statistics for Stop trials. e) SSD across trials. f) The inhibition function.\n Note: Integration method used to estimate SSRT. See Verbruggen et al., (2019). Blue = Succesful Go or Stop trial. Red = Failed Stop trial. Dotted lines represent blocks.",
                                  size = 10))

  return(final)
}





