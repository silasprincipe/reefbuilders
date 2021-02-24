###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br
#Collaboration: André L. Acosta

### Modeling procedure ----
# This code is used just to concentrate all functions used in the modeling
# First we generate the pseudo absences, than generate the blocks for the
# cross validation and then we tun the modeling. To see how each procedure is
# done, see the functions files.

# Load functions ----
source("functions/blockGen.R")
source("functions/pseudoabGen.R")
source("functions/biomodModelling.R")
set.seed(2932)

# Run the pseudo absence generation ----
lapply(c("muhi", "moca", "side"), pseudoabGen)

# Run the generation of blocks for cross validation ----
blockGen(c("muhi", "moca", "side"))

# Run the modeling procedure ----
lapply(c("muhi", "moca", "side"), 
       biomodModelling,
       round.code = "rbf1") # this is the code that is used by
                            # BIOMOD2 to save the files in the
                            # respective species' folders.

### END