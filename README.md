# BASTD
Batch Analysis of Stop-signal Task Data (BASTD) is a package with a series of functions used to analyze and visualize stop-signal task (SST) data.
Written by Jason L He

The functions of BASTD can be separated into those which analyze and those which visualize.

Those which analyze are: 
BASTD_analyze()
OSARI_analyze() and
STOPIT_analyze()

BASTD_analyze - Requires input of data collected from the SST. Given that different versions of the SST have different column names, it is the job of the user to ensure that column names line up with the column names used by BASTD_analyze. 

The column names are: (ID, Block, Trial, Stimulus, Signal, Correct, Response, RT, RE, SSD, TrialType)

ID: Any Character or String
Trial: Numeric values in increasing order and reset after every block (e.g., 0-64)
Stimulus: 1 or 2 (for choice-reaction variants of the SST)
Signal: 0 or 1 (0 meaning no signal was presenented (i.e., a Go-trial) and 1 meaning a signal was presented (i.e., a Stop-trial)
Correct: 0 or 2 (0 for incorrect, 2 for correct) - *plans are in place for making this compatible with 0 and 1)*
Response: 0 or 1 (0 for no response, 1 for a response being made)
RT: A numerical value for RT in ms (e.g., 300 ms)
RE: *Not currently implemented*
SSD: A numerical value for Stop-signal delay in ms (e.g., 250 ms)
TrialType (Optional): *Not currently implemented*

Provided the column names are consistent with the above, BASTD_analyze should be able to analyze any dataset from a SST




Those which visualize are: 
OSARI_visualize()
OSARI_visualize_all()
STOPIT_visualize()*
STOPIT_visualize_all()*

*Visualization capacity for data collected from STOP IT not yet implemented




Please report any issues on the github page 

