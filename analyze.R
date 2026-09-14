# Analyze South Delta barriers project simulations
#
# Doug Jackson
# doug@QEDAconsulting.com
library(tidyvderse)
library(ggplot2)

####################################################################################################
# Constants
####################################################################################################
workingDir <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

dataDir <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/fromXiao/studies_13sep26"
####################################################################################################
# Run
####################################################################################################
setwd(workingDir)

npFiles <- list.files(dataDir, pattern="np_*")
spFiles <- list.files(dataDir, pattern="sp_*")
northDeltaFiles <- list.files(dataDir, pattern="salmon_nd*")
southDeltaFiles <- list.files(dataDir, pattern="salmon_sd*")

