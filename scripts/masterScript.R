# SETUP
source("scripts/masterLibrary.R")

# READ DATA

source("scripts/readLgr.R")
source("scripts/readFieldSheets.R")


# Diffusive emissions

source("scripts/chamberVolume.R") # calculate chamber headspace volume
source("scripts/plotCleanLgr.R") # select time window for diffusive emission rate modeling
source("scripts/analysis/calculateDiffusion.R") # diffusive emission rates.  

# Ebullition rates

source("scripts/analysis/ebullitionMassFluxFunction.R") # source function
source("scripts/analysis/calculateEbullition.R") # eb_results

# Merge diffusive and ebulitive rates --> calculate total

source("scripts/analysis/calculateTotalEmissions.R")
