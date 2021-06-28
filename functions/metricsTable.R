###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#### Function to get a formated table of evaluation metrics #####

metricsTable <- function(species, round.code) {
        
        library(tidyverse)
        
        #Load evaluation raw data
        biomod.eval <- read_csv(paste0(
                species,
                "/evals/model_eval_",
                species,
                "_",
                round.code,
                ".csv"
        ))
        
        #Summarise data for each metric
        mean.eval.tss <- biomod.eval %>%
                separate(Model.name, sep = "_", into = c("model", "run", "pa")) %>%
                filter(run != "RUN6", Eval.metric == "TSS") %>%
                group_by(model) %>%
                summarise(mean = mean(Testing.data),
                          sd = sd(Testing.data))
        
        mean.eval.auc <- biomod.eval %>%
                separate(Model.name, sep = "_", into = c("model", "run", "pa")) %>%
                filter(run != "RUN6", Eval.metric == "ROC") %>%
                group_by(model) %>%
                summarise(mean = mean(Testing.data),
                          sd = sd(Testing.data))
        
        mean.eval.ss <- biomod.eval %>%
                separate(Model.name, sep = "_", into = c("model", "run", "pa")) %>%
                filter(run != "RUN6", Eval.metric == "TSS") %>%
                group_by(model) %>%
                summarise( mean.ss = mean(Sensitivity),
                           sd.ss = sd(Sensitivity),
                           mean.sp = mean(Specificity),
                           sd.sp = sd(Specificity))
        
        #Create the final table
        final.table <- data.frame(matrix(nrow = length(mean.eval.tss$model), ncol = 5))
        
        colnames(final.table) <- c("Model", "TSS", "AUC", "Specificity", "Sensitivity")
        
        final.table$Model <- mean.eval.tss$model
        
        #Paste final data
        final.table$TSS <- paste0(round(mean.eval.tss$mean, digits = 2),
                                  " \U00B1 ", 
                                  round(mean.eval.tss$sd, digits = 2))
        
        final.table$AUC <- paste0(round(mean.eval.auc$mean, digits = 2),
                                  " \U00B1 ", 
                                  round(mean.eval.auc$sd, digits = 2))
        
        final.table$Sensitivity <- paste0(round(mean.eval.ss$mean.ss, digits = 2),
                                          " \U00B1 ", 
                                          round(mean.eval.ss$sd.ss, digits = 2))
        final.table$Specificity <- paste0(round(mean.eval.ss$mean.sp, digits = 2),
                                          " \U00B1 ", 
                                          round(mean.eval.ss$sd.sp, digits = 2))
        
        #Add name of species for control
        final.table$Species <- species
        
        #Returns the table
        final.table
        
}