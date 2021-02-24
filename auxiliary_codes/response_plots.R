###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

# Response plots for BIOMOD2
library(biomod2)

# Set species name
species <- 'side'

# Set round code
round.code <- "rb1"

# Load BIOMOD model
load(paste0(species,"/",species,".",species,"_", round.code,".models.out"))

# Remember to change species name and round code here!
biomod.model.out <- side.side_rb1.models.out

GLMs <- BIOMOD_LoadModels(biomod.model.out, models=c('GLM'))
GAMs <- BIOMOD_LoadModels(biomod.model.out, models=c('GAM'))
RFs <- BIOMOD_LoadModels(biomod.model.out, models=c('RF'))
GBMs <- BIOMOD_LoadModels(biomod.model.out, models=c('GBM'))

themodels <- list(GLMs, GAMs, RFs, GBMs)

# Open a PDF file. Remember to set the name you want.
pdf(paste0("response_plots_", species, "_", round.code, ".pdf"))

# Plot 2D response plots
for (i in 1:4) {
        
        resp.plot <-
                response.plot2(
                        models  = themodels[[i]],
                        Data = get_formal_data(biomod.model.out,
                                               'expl.var'),
                        show.variables = get_formal_data(biomod.model.out,
                                                         'expl.var.names'),
                        do.bivariate = FALSE,
                        fixed.var.metric = 'median',
                        col = c("blue", "red", "green", "orange", "gray", "darkblue"),
                        legend = TRUE,
                        data_species = get_formal_data(biomod.model.out, 'resp.var')
                )
}

dev.off()

###END