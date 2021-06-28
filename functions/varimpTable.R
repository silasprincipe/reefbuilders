###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#### Function to return a formated Variable Importance table #####
#This function considers the ensemble variable importance

varimpTable <- function(species, round.code) {

        library(tidyverse)

        #Load variable importance raw file
        var.imp <- read.csv(
                paste0(species,
                        "/evals/",
                        "ensemble_wm_var_imp_",
                        species,
                        "_",
                        round.code,
                        ".csv"
                )
        )
        
        rownames(var.imp) <- var.imp$X
        
        var.imp <- var.imp[,2:ncol(var.imp)]
        
        var.imp <- as.data.frame(t(var.imp))

        #Summarise (mean and SD)
        mean.vimp <- var.imp %>%
                summarise_all(mean, na.rm = TRUE)

        sd.vimp <- var.imp %>%
                summarise_all(sd, na.rm = TRUE)

        #Create the final table
        final.table <- data.frame(matrix(nrow = 1,
                                         ncol = length(mean.vimp)))

        colnames(final.table) <- colnames(mean.vimp)

        #Paste values in the format
        for (i in 1:length(mean.vimp)) {
                final.table[1, i] <- paste0(round(mean.vimp[1, i], digits = 2),
                                            " \U00B1 ",
                                            round(sd.vimp[1, i], digits = 2))

        }

        #Add species name
        #final.table$Species <- species

        #Return the final table
        final.table

}
