###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#### Function to get differences between current and future area #####
#This function uses as argument the dataframe returned by the areaChange function.

getDiff <- function(data) {
        #Create a new dataframe that will receive the data
        ftable <- data.frame(matrix(nrow = 8, ncol = 4))
        colnames(ftable) <- c("section", "dif26", "dif45", "dif85")

        #Calculate the differences of area. Also the percentage of change.
        ftable$dif26[1] <- round(data[2, 1] - data[2, 2], 1) * -1
        ftable$dif45[1] <- round(data[2, 1] - data[2, 3], 1) * -1
        ftable$dif85[1] <- round(data[2, 1] - data[2, 4], 1) * -1

        ftable$dif26[2] <- round(((data[2, 2] * 100) / data[2, 1]) - 100, 1)
        ftable$dif45[2] <- round(((data[2, 3] * 100) / data[2, 1]) - 100, 1)
        ftable$dif85[2] <- round(((data[2, 4] * 100) / data[2, 1]) - 100, 1)
        
        ftable$section[1:2] <- "total"
        
        
        ### Brazil
        
        #Calculate the differences of area. Also the percentage of change.
        ftable$dif26[3] <- round(data[4, 1] - data[4, 2], 1) * -1
        ftable$dif45[3] <- round(data[4, 1] - data[4, 3], 1) * -1
        ftable$dif85[3] <- round(data[4, 1] - data[4, 4], 1) * -1
        
        ftable$dif26[4] <- round(((data[4, 2] * 100) / data[4, 1]) - 100, 1)
        ftable$dif45[4] <- round(((data[4, 3] * 100) / data[4, 1]) - 100, 1)
        ftable$dif85[4] <- round(((data[4, 4] * 100) / data[4, 1]) - 100, 1)
        
        ftable$section[3:4] <- "brazil"
        
        ### North
        
        #Calculate the differences of area. Also the percentage of change.
        ftable$dif26[5] <- round(data[6, 1] - data[6, 2], 1) * -1
        ftable$dif45[5] <- round(data[6, 1] - data[6, 3], 1) * -1
        ftable$dif85[5] <- round(data[6, 1] - data[6, 4], 1) * -1
        
        ftable$dif26[6] <- round(((data[6, 2] * 100) / data[6, 1]) - 100, 1)
        ftable$dif45[6] <- round(((data[6, 3] * 100) / data[6, 1]) - 100, 1)
        ftable$dif85[6] <- round(((data[6, 4] * 100) / data[6, 1]) - 100, 1)
        
        ftable$section[5:6] <- "north"
        
        ### Eastern
        
        #Calculate the differences of area. Also the percentage of change.
        ftable$dif26[7] <- round(data[8, 1] - data[8, 2], 1) * -1
        ftable$dif45[7] <- round(data[8, 1] - data[8, 3], 1) * -1
        ftable$dif85[7] <- round(data[8, 1] - data[8, 4], 1) * -1
        
        ftable$dif26[8] <- round(((data[8, 2] * 100) / data[8, 1]) - 100, 1)
        ftable$dif45[8] <- round(((data[8, 3] * 100) / data[8, 1]) - 100, 1)
        ftable$dif85[8] <- round(((data[8, 4] * 100) / data[8, 1]) - 100, 1)
        
        ftable$section[7:8] <- "eastern"
        

        #Information print
        cat(
                "Lines 1, 3, 5 and 7 are area in km2 difference between current and future.
                Lines 2, 4, 6 and 8 are the percentage of change. 
                Positive are increase and negative a decrease. \n
                \n"
        )

        #Return the dataframe
        return(ftable)
}

### END
