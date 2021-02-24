###### Reef-builders modelling #######
#Silas C. Principe - 2020
#silasprincipe@yahoo.com.br

#Kernel density plots of presence/absence points on environmental data
paDensity <- function(x) {

        muhi <- read.csv("data/muhi/muhi_pts.csv")
        moca <- read.csv("data/moca/moca_pts.csv")
        side <- read.csv("data/side/side_pts.csv")
        
        muhi.d <- read.csv("data/muhi/muhi_dsp.csv")
        moca.d <- read.csv("data/moca/moca_dsp.csv")
        side.d <- read.csv("data/side/side_dsp.csv")
        
        muhi.d[is.na(muhi.d)] <- 0
        moca.d[is.na(moca.d)] <- 0
        side.d[is.na(side.d)] <- 0

        r <- raster(x)

        r.val <- data.frame(xyFromCell(r, 1:length(r)))
        colnames(r.val) <- c("decimalLongitude", "decimalLatitude")
        r.val$values <- as.vector(values(r))

        muhi.val <- muhi[,1:2]
        muhi.val$values <- raster::extract(r, muhi[,1:2])

        moca.val <- moca[,1:2]
        moca.val$values <- raster::extract(r, moca[,1:2])

        side.val <- side[,1:2]
        side.val$values <- raster::extract(r, side[,1:2])

        
        muhi.val$code <- ifelse(muhi.d == 1, "MUHI_P", "MUHI_A")
        moca.val$code <- ifelse(moca.d == 1, "MOCA_P", "MOCA_A")
        side.val$code <- ifelse(side.d == 1, "SIDE_P", "SIDE_A")

        r.val$code <- "ALL"

        total <- rbind(r.val, muhi.val, moca.val, side.val)
        total <- total[!is.na(total$values),]
        
        g <- ggplot(total, aes(x = values, colour = code)) +
        geom_density(position="identity", fill = NA, size = 1) +
        scale_x_continuous(name = names(r)) +
        scale_y_continuous(name = "Density") +
        ggtitle(paste0("Density plot of ", names(r))) +
        theme_bw() +
        theme(plot.title = element_text(size = 14,
         face = "bold"),
              text = element_text(size = 12)) +
        scale_color_manual(values = c("#7A7773", "#F14932", "#F97100",
                                      "#48B426", "#8CD575",
                                      "#0C36B0", "#5B7DE1",
                                      "grey", "darkgrey"))

        ggsave(paste0("figures/temp/pa_density_",names(r),".png"), g)
        
        data_summary <- function(x) {
                m <- mean(x)
                med <- median(x)
                ymin <- m-sd(x)
                ymax <- m+sd(x)
                return(c(y=med,ymin=ymin,ymax=ymax))
        }
        
        j <- ggplot(total, aes(x = code, y = values, colour = code))+
                #geom_boxplot()+
                geom_jitter(alpha = 0.4)+
                stat_summary(fun.data=data_summary, color="black")
        
        ggsave(paste0("figures/temp/pa_spread_",names(r),".png"), j)        
}


# Apply the function in all files

env.files <- list.files("data/env/crop_layers/", full.names = TRUE)

lapply(env.files, paDensity)
