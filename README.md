# BASTD
Batch Analysis of Stop-signal Task Data (BASTD) is a package with a series of functions used to analyze and visualize stop-signal task (SST) data.
Written by Jason L He

The functions of BASTD can be separated into those which analyze and those which visualize.

Those which analyze are: 
BASTD_analyze()
OSARI_analyze() and
STOPIT_analyze()

BASTD_analyze - requires input of data collected from the SST. Given that different versions of the SST have different column names, it is the job of the user to ensure that column names line up with the column names used by BASTD_analyze (ID, Block, Trial, Stimulus, Signal, Correct, Response, RT, RE, SSD, TrialType)





Those which visualize are: 
OSARI_visualize()
OSARI_visualize_all()
STOPIT_visualize()*
STOPIT_visualize_all()*

*Visualization capacity for data collected from STOP IT not yet implemented




Please report any issues on the github page 

