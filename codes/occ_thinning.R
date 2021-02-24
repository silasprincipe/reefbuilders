###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#### Run the thinning procedure #####

###Note: this is just a wrapper to run "OccurrenceThinner v1.04" directly
###from R. This code also adjust the files to the format used in the analysis.
###OccurrenceThinner is a java based program which was created by
###Dr. Heroen Verbruggen and is freely available at 
### https://github.com/hverbruggen/OccurrenceThinner

#Note: Thinning by OccurrenceThinner is random. Thus, running this code
#will certainly produce final files different from ours. Backuping to compare
#is advised. Anyway, final results should not be considerably different.

#Load libraries
library(tidyverse)

#Define species acronyms
grp <- c("muhi", "moca", "side")

#Run in loop
for(i in 1:length(grp)){
        
        #Define the working directory
        setwd("data/thinning/")
        
        #Create the running code for the OccurrenceThinner java prog.
        java.run <- paste0("java -jar OccurrenceThinner104.jar -i ", 
                          grp[i], "_occ.csv -r ", grp[i],
                          "_density.asc -nr 1 -t1 0.8 -t2 1")
                        #These are the thresholds/options
        
        #Run the OccurrenceThinner java program
        java.out <- system(java.run, intern = TRUE)
        
        #Save output
        write.table(java.out, paste0(grp[i], "_javaoutput.txt"))
        
        #Rename file produced by OccurrenceThinner
        file.rename(paste0(grp[i], "_occ.csv.thinned1"), 
                    paste0(grp[i], "_occ_thinned.csv"))
        
        #Open the file produced
        pts.t <- read.csv(paste0(grp[i], "_occ_thinned.csv"))
        
        #Create a 'presence' column
        pts.t$pdata <- 1
        
        pts.t <- pts.t[,-1]
        
        #Change column names
        colnames(pts.t) <- c("decimalLongitude", "decimalLatitude", grp[i])
        
        #Return to the original directory
        setwd("../../")
        
        #Open the original points
        pts.o <- read.csv(paste0("data/", grp[i], "/", grp[i], "_cell.csv"))
        
        #Save an object with the number of points (P/A) just to register
        pts.dif <- nrow(pts.o)
        
        #Select only absences (if any)
        pts.o <- pts.o[pts.o[,3] == 0,]
        
        #Bind with the new presences selected by the thinning procedure
        pts.final <- rbind(pts.o, pts.t)
        
        #Get the difference in number of points
        pts.dif <- pts.dif - nrow(pts.final)
        
        #Save the final file
        write.csv(pts.final,
                  paste0("data/", grp[i], "/", grp[i], "_cell_thinned.csv"),
                  row.names = F)
        
        #Print results
        cat(grp[i], "thinning done.", pts.dif, "points were excluded. \n")
        
        #END
}

###END of code

