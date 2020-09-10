rcpSuit <-
        function(sp,
                 round.code,
                 rcp) {
                orig <- raster(
                        paste0(
                                sp,
                                "/proj_current_",
                                sp,
                                "_",
                                round.code,
                                "/individual_projections/",
                                sp,
                                "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                        )
                )
                
                fut1 <-
                        raster(
                                paste0(
                                        sp,
                                        "/proj_",
                                        rcp[1],
                                        "_",
                                        #year,    ###If working with other periods
                                        #"_",
                                        sp,
                                        "_",
                                        round.code,
                                        "/individual_projections/",
                                        sp,
                                        "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                                )
                        )
                
                
                fut2 <-
                        raster(
                                paste0(
                                        sp,
                                        "/proj_",
                                        rcp[2],
                                        "_",
                                        #year,    ###If working with other periods
                                        #"_",
                                        sp,
                                        "_",
                                        round.code,
                                        "/individual_projections/",
                                        sp,
                                        "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                                )
                        )
                
                
                fut3 <-
                        raster(
                                paste0(
                                        sp,
                                        "/proj_",
                                        rcp[3],
                                        "_",
                                        #year,    ###If working with other periods
                                        #"_",
                                        sp,
                                        "_",
                                        round.code,
                                        "/individual_projections/",
                                        sp,
                                        "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                                )
                        )
                
                #If wanted to use another ensemble method/threshold
                #
                # eval <-
                #         read.csv(paste0(
                #                 sp,
                #                 "/evals/ensemble_eval_",
                #                 sp,
                #                 "_",
                #                 round.code,
                #                 ".csv"
                #         ))
                # 
                # eval <-
                #         eval[eval$Model.name %in% c("EMwmeanByTSS_mergedAlgo_mergedRun_mergedData"),]
                # eval <- eval %>% filter(Eval.metric == 'TSS')
                # thres <- eval$Cutoff
                thres <- 800
                
                orig[orig < thres] <- 0
                orig[orig >= thres] <- 1
                
                fut1[fut1 < thres] <- 0
                fut1[fut1 >= thres] <- 2
                
                fut2[fut2 < thres] <- 0
                fut2[fut2 >= thres] <- 2
                
                fut3[fut3 < thres] <- 0
                fut3[fut3 >= thres] <- 2
                
                new1 <- orig + fut1
                new2 <- orig + fut2
                new3 <- orig + fut3
                
                writeRaster(orig, paste0(sp,"_current.tif"), overwrite = T)
                
                writeRaster(new1, paste0(sp,"_",rcp[1],".tif"), overwrite = T)
                writeRaster(new2, paste0(sp,"_",rcp[2],".tif"), overwrite = T)
                writeRaster(new3, paste0(sp,"_",rcp[3],".tif"), overwrite = T)
                
        }

rcpSuit("side", "rb_1", c("rcp26","rcp45","rcp85"))                





changeSuit <-
        function(round.code,
                 rcp) {
                
                current <- stack()
                
                for (i in 1:3) {
                        sp <- c("muhi", "moca", "side")[i]
                        
                        orig <- raster(
                                paste0(
                                        sp,
                                        "/proj_current_",
                                        sp,
                                        "_",
                                        round.code,
                                        "/individual_projections/",
                                        sp,
                                        "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                                )
                        )
                        
                        # eval <-
                        #         read.csv(
                        #                 paste0(
                        #                         sp,
                        #                         "/evals/ensemble_eval_",
                        #                         sp,
                        #                         "_",
                        #                         round.code,
                        #                         ".csv"
                        #                 )
                        #         )
                        # 
                        # eval <-
                        #         eval[eval$Model.name %in% c("EMwmeanByTSS_mergedAlgo_mergedRun_mergedData"),]
                        # eval <-
                        #         eval %>% filter(Eval.metric == 'TSS')
                        # thres <- eval$Cutoff
                        thres <- 800
                        
                        orig[orig < thres] <- 0
                        orig[orig >= thres] <- 1
                        
                        current <- stack(current, orig)
                }
                
                future <- stack()
                
                for (i in 1:3) {
                        sp <- c("muhi", "moca", "side")[i]
                        
                        fut <-
                                raster(
                                        paste0(
                                                sp,
                                                "/proj_",
                                                rcp,
                                                "_",
                                                #year,
                                                #"_",
                                                sp,
                                                "_",
                                                round.code,
                                                "/individual_projections/",
                                                sp,
                                                "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                                        )
                                )
                        
                        # eval <-
                        #         read.csv(
                        #                 paste0(
                        #                         sp,
                        #                         "/evals/ensemble_eval_",
                        #                         sp,
                        #                         "_",
                        #                         round.code,
                        #                         ".csv"
                        #                 )
                        #         )
                        # 
                        # eval <-
                        #         eval[eval$Model.name %in% c("EMwmeanByTSS_mergedAlgo_mergedRun_mergedData"),]
                        # eval <-
                        #         eval %>% filter(Eval.metric == 'TSS')
                        # thres <- eval$Cutoff
                        thres <- 800
                        
                        fut[fut < thres] <- 0
                        fut[fut >= thres] <- 1
                        
                        future <- stack(future, fut)
                }
                
                #Sum to get species composition
                current <- sum(current)
                
                future <- sum(future)
                
                #Get the difference
                r <- current - future
                
                writeRaster(r, paste0(rcp,"_composition_change.tif"))
                
        }


compSuit <-
        function(round.code,
                 rcp) {
                
                future <- stack()
                
                for (i in 1:3) {
                        sp <- c("muhi", "moca", "side")[i]
                        
                        fut <-
                                raster(
                                        paste0(
                                                sp,
                                                "/proj_",
                                                rcp,
                                                "_",
                                                #year,
                                                #"_",
                                                sp,
                                                "_",
                                                round.code,
                                                "/individual_projections/",
                                                sp,
                                                "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.img"
                                        )
                                )
                        
                        # eval <-
                        #         read.csv(
                        #                 paste0(
                        #                         sp,
                        #                         "/evals/ensemble_eval_",
                        #                         sp,
                        #                         "_",
                        #                         round.code,
                        #                         ".csv"
                        #                 )
                        #         )
                        # 
                        # eval <-
                        #         eval[eval$Model.name %in% c("EMwmeanByTSS_mergedAlgo_mergedRun_mergedData"),]
                        # eval <-
                        #         eval %>% filter(Eval.metric == 'TSS')
                        # thres <- eval$Cutoff
                        thres <- 800
                        
                        fut[fut < thres] <- 0
                        fut[fut >= thres] <-
                                i #MUHI CODE = 1, MOCA = 2, SIDE = 3
                        
                        future <- stack(future, fut)
                }
                
                #We change the code of Siderastrea to enable a unique code
                future[[3]] <- future[[3]] * 2
                
                #Sum for creating the composite
                r <- sum(future)
                
                writeRaster(r, paste0(rcp,"_composition_species.tif"))
                
        }



changeSuit("rb_1", "rcp85")


compSuit("rb_1", "rcp85")
