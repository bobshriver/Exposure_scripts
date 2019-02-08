library("sp")
library('methods')
#library("dplyr")
library(rSOILWAT2)

#These are the functions I need:
# if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", 
#                                                                scaler = 1, FUN = stats::weighted.mean, weights = layers_width, 
#                                                                x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
# if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)

#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/scratch/cma393/AFRI/Historical"
dir.jbHOME <- "/home/jbb239"

regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS") #list.files(dir.AFRI_Historical)

print(regions)
dir.regions <- file.path(dir.AFRI_Historical, regions)
dir.regions_3Runs <- file.path(dir.AFRI_Historical, regions, "3_Runs" )
dir.regions_1Input <- file.path(dir.AFRI_Historical, regions, "1_Input")

print(dir.regions_3Runs)
print(dir.regions_1Input)


VWCtoSWP_simple <- function(vwc, sand, clay){
  #Outputs SWP as negative MPa
  bar_toMPa = -0.1
  bar_conversion = 1024
    
    thetas <- -14.2 * sand - 3.7 * clay + 50.5
    psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
    b <- -0.3 * sand + 15.7 * clay + 3.10
    res <- psis / ((vwc * 100 / thetas) ^ b * bar_conversion) * bar_toMPa
    return(res)
}

# VWCtoSWP_simple(vwc=0.1, sand=.5, clay=0.3)
# 
# dSWP <- as.data.frame(RUN_DATA@SWPMATRIC@Day)
# dSWP[1, c(3:6)]
# dVWC[1, c(3:6)]
# VWCtoSWP_simple(vwc=dVWC[1, 3], sand=sSAND[1], clay=sCLAY[1])
# VWCtoSWP_simple(vwc=dVWC[1, c(3:6)], sand=sSAND[1:4], clay=sCLAY[1:4])
# head(dVWC, 14)
# dINF <- as.data.frame(RUN_DATA@SOILINFILT@Day)
# head(dINF, 14)


#Function for calculating WDD
    calcWDDall <- function(RUN_DATA, name){
      #print("Pre d1")
      #print(Sys.time())
      # s=8
      #   load(file.path(dir.regions_3Runs[1], sites[s], "sw_output_sc1.RData"))
      #   RUN_DATA <- runDataSC
      #   name=sites[s]
      
      lyrsTOP <- c(1, 2)
      dVWC <- as.data.frame(RUN_DATA@VWCMATRIC@Day)
      
      dtemps <- as.data.frame(RUN_DATA@TEMP@Day) 
      dtemps$degspos <- dtemps$avg_C 
      dtemps$degspos[which(dtemps$degspos<0)] <- 0

      s_name <- paste0("Site_", as.integer(substr(name, 1, regexpr('_', name)-1)) )
      sdepths <- as.vector(soildepths[which(soildepths$Label==s_name), -1])
      #str(sdepths)
      maxdepth <- as.integer(sdepths[1])
      #str(maxdepth)
      sdepths[sdepths > maxdepth ] <- NA
      sdepth <- sdepths[-1]
      slyrwidths <- diff(c(0, na.omit(t(sdepth)) ) )

      sSAND <- soilSAND[which(soilSAND$Label==s_name), c(2:(1+length(slyrwidths)))]
      sCLAY <- soilCLAY[which(soilCLAY$Label==s_name), c(2:(1+length(slyrwidths)))]
      sandMEANtop <- weighted.mean(sSAND[lyrsTOP], slyrwidths[lyrsTOP])
      sandMEANbot <- weighted.mean(sSAND[-lyrsTOP], slyrwidths[-lyrsTOP])
      clayMEANtop <- weighted.mean(sCLAY[lyrsTOP], slyrwidths[lyrsTOP])
      clayMEANbot <- weighted.mean(sCLAY[-lyrsTOP], slyrwidths[-lyrsTOP])
      
      
      vwcMEANtop <- apply(dVWC[, lyrsTOP+2], 1, FUN=function(x) weighted.mean(x, slyrwidths[lyrsTOP]))
      vwcMEANbot <- apply(dVWC[, -c(1, 2, lyrsTOP+2)], 1, FUN=function(x) weighted.mean(x, slyrwidths[-lyrsTOP]))
      #   str(dVWC)
      
      dVWC$vwcMEANtop <- vwcMEANtop
      dVWC$vwcMEANbot <- vwcMEANbot
        
      dVWC$swpTOP <- VWCtoSWP_simple(vwc=dVWC$vwcMEANtop, sand=sandMEANtop, clay=clayMEANtop)
      dVWC$swpBOT <- VWCtoSWP_simple(vwc=dVWC$vwcMEANbot, sand=sandMEANbot, clay=clayMEANbot)
      
      d_all <- merge(dtemps, dVWC, by=c("Year", "Day"))
      
      d_all$wddall <- d_all$degspos
      #d_all$wddTOPl[which(d_all$swpTOP <= -1.5)] <- 0
      #d_all$wddBOT[which(d_all$swpBOT <= -1.5)] <- 0
      d_all$wddall[which(d_all$swpBOT <= -1.5 & d_all$swpTOP <= -1.5)] <- 0
      #  d_all[c(300:320), ]
      
      d <- d_all[, c("Year", "wddall")]
      
      d2 <-aggregate(d, by=list(d$Year), FUN=sum, na.rm=TRUE)
      d2 <- d2[, c("Group.1", "wddall")]
      names(d2)[2] <- c(name)
      d3 <- as.data.frame(t(d2))
      rownames(d3) <- c("year", name)
      return(d3)
    }

print("Start Loop")
print(Sys.time())

#Try in parallel
    library("parallel")
    library("foreach")
    library("doParallel")
    #detectCores()

    for (r in 1:5){
      # r=1

        
      sites <- list.files(dir.regions_3Runs[r])
        
        #print(sites[1:10])
        cl<-makeCluster(24)
        registerDoParallel(cl)
        
        wddALLlyrs = foreach(s = sites, .combine = rbind) %dopar% {
          f <- list.files(file.path(dir.regions_3Runs[r], s) )
          if(length(f)==1){
            load(file.path(dir.regions_3Runs[r], s, "sw_output_sc1.RData"))
            d <- calcWDDall(RUN_DATA = runDataSC, name=s)
            d[2,]
          }
        }
        stopCluster(cl)
        
        print(paste("wddALLlyrs", dim(wddALLlyrs)) )
        print(paste(regions[r], "Done"))
        print(Sys.time())
        
        ifelse (r == 1, annualwddALLlyrs <- wddALLlyrs, annualwddALLlyrs <- rbind(annualwddALLlyrs, wddALLlyrs))    
    }

#Final steps
    
    

names(annualwddALLlyrs) <- paste(c(1915:2015))
save(annualwddALLlyrs, file=file.path(dir.jbHOME, "annualwddALLlyrs19152015"))










#DEVELOPMENT

# soildepths <- read.csv(file=file.path(dir.regions_1Input[1],  "SWRuns_InputData_SoilLayers_v9.csv"), header=TRUE )
# 
# soildata <- read.csv(file=file.path(dir.regions_1Input[1], "datafiles",  "SWRuns_InputData_soils_v12.csv"), header=TRUE )
# 
# metadata <- readRDS(file=file.path(dir.regions[1], "SFSW2_project_descriptions.rds") )
# #str(metadata[["sim_time"]])
# isim_time <- metadata[["sim_time"]]
# simTime2 <- metadata[["sim_time"]]$sim_time2_North
# 
# layers_width <- getLayersWidth(layers_depth)
# 
# load(file.path(dir.regions_3Runs[1], sites[1], "sw_output_sc1.RData"))
# dtemps <- as.data.frame(runDataSC@TEMP@Day)
# dVWC <- as.data.frame(runDataSC@VWCMATRIC@Day)
# dwd <- as.data.frame(runDataSC@WETDAY@Day)
# dSM <- as.data.frame(runDataSC@SWPMATRIC@Day)
# str(dSM)
# names(dSM)[c(-1, -2)] <- paste("SM", names(dSM)[c(-1, -2)])
# d_all2 <- merge(d_all, dSM, by=c("Year", "Day"))
# d_all2[c(3050: 3080),]
#dSNOW <- as.data.frame(runDataSC@SNOWPACK@Day)

#dtst <-aggregate(d_all, by=list(d$Year), FUN=length(), na.rm=TRUE)
