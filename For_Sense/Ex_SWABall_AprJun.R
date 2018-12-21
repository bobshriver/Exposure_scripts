library("sp")
library('methods')
#library("dplyr")
#library(rSOILWAT2)

#These are the functions I need:
# if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", 
#                                                                scaler = 1, FUN = stats::weighted.mean, weights = layers_width, 
#                                                                x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
# if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)

#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/cxfs/projects/usgs/ecosystems/sbsc/AFRI/Historical"

dir.jbHOME <- "/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data"



regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS", "Western_Gap")#list.files(dir.AFRI_Historical)

print(regions)
dir.regions <- file.path(dir.AFRI_Historical, regions)
dir.regions_3Runs <- file.path(dir.AFRI_Historical, regions, "3_Runs" )
dir.regions_1Input <- file.path(dir.AFRI_Historical, regions, "1_Input")

print(dir.regions_3Runs)
print(dir.regions_1Input)


#Function for calculating WDD
    calcSWA_AprJun <- function(RUN_DATA, name){
      #print("Pre d1")
      #print(Sys.time())
      # s=1
      #   sites <- list.files(dir.regions_3Runs[1])
      #   load(file.path(dir.regions_3Runs[1], sites[s], "sw_output_sc1.RData"))
      #   RUN_DATA <- runDataSC
      #   name=sites[s]
      
      dSWA <- as.data.frame(RUN_DATA@SWABULK@Month)
      dSWA_AprJun <- dSWA[which(dSWA$Month %in% c(4:6)),]
      head(dSWA_AprJun)
      numlyrs <- dim(dSWA)[2] - 2
      if(numlyrs>1){dSWA_AprJun$Alllyrs <- rowSums(as.matrix(dSWA_AprJun[, c(3:(numlyrs+2))]))} else{
        dSWA_AprJun$Alllyrs <- as.matrix(dSWA_AprJun[, c(3:(numlyrs+2))])}
      
      d <- dSWA_AprJun[, c("Year", "Alllyrs")]
      
      d2 <-aggregate(d, by=list(d$Year), FUN=mean, na.rm=TRUE)
      d2 <- d2[, c("Group.1", "Alllyrs")]
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

    for (r in 1:length(regions)){
      # r=1
 
      #print(str(soildata))
    
      sites <- list.files(dir.regions_3Runs[r])
        
        #print(sites[1:10])
        cl<-makeCluster(20)
        registerDoParallel(cl)
        
        SWA_AprJun = foreach(s = sites, .combine = rbind) %dopar% {
          f <- list.files(file.path(dir.regions_3Runs[r], s) )
          if(length(f)==1){
            load(file.path(dir.regions_3Runs[r], s, "sw_output_sc1.RData"))
            d <- calcSWA_AprJun(RUN_DATA = runDataSC, name=s)
            d[2,]
          }
        }
        stopCluster(cl)
        
        print(paste(regions[r], "Done"))
        print(Sys.time())
        
        ifelse (r == 1, annualSWA_AprJun <- SWA_AprJun, annualSWA_AprJun <- rbind(annualSWA_AprJun, SWA_AprJun))    
    }
    

names(annualSWA_AprJun) <- paste(c(1915:2015))
save(annualSWA_AprJun, file=file.path(dir.jbHOME, "annualSWA_AprJun19152015"))










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
