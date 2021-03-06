library("sp")
library('methods')
library("plyr")
library("dplyr")
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
    
    #Function for calculating average temp on dry days. 

    calcHotDry_JulSep <- function(RUN_DATA, name){
      #print("Pre d1")
      #print(Sys.time())
      # s=1
      #   sites <- list.files(dir.regions_3Runs[1])
      #   load(file.path(dir.regions_3Runs[1], sites[s], "sw_output_sc1.RData"))
      #   RUN_DATA <- runDataSC
      #   name=sites[s]
      
      dVWC <- as.data.frame(RUN_DATA@VWCMATRIC@Day)
      dTemps <- as.data.frame(RUN_DATA@TEMP@Day) 

      dVWC_JulSep <- dVWC[which(dVWC$Day %in% c(182:273)),]
      
      dVWC_JulSep$Temp <- dTemps[which(dTemps$Day %in% c(182:273)),5]

      
      s_name <- paste0("Site_", as.integer(substr(name, 1, regexpr('_', name)-1)) )
      sdepths <- as.vector(soildepths[which(soildepths$Label==s_name), -1])
      #str(sdepths)
      maxdepth <- as.integer(sdepths[1])
      #str(maxdepth)
      sdepths[sdepths > maxdepth ] <- NA
      sdepth <- sdepths[-1]
      slyrwidths <- diff(c(0, na.omit(t(sdepth)) ) )
      numlyrs <- dim(dVWC)[2] - 2
#print(numlyrs)
      
      nlyrs<-if(numlyrs<7){numlyrs} else {6}
      #print(nlyrs)
      if(numlyrs>1 & numlyrs<7 ){dVWC_JulSep$Alllyrs <- apply(as.matrix(dVWC_JulSep[, c(3:(numlyrs+2))]), 1, FUN=function(x) weighted.mean(x, slyrwidths[1:nlyrs]))} 
      if(numlyrs>1 & numlyrs>6 ){dVWC_JulSep$Alllyrs <- apply(as.matrix(dVWC_JulSep[, c(3:(6+2))]), 1, FUN=function(x) weighted.mean(x, slyrwidths[1:nlyrs]))}
      if(numlyrs==1){dVWC_JulSep$Alllyrs <- as.matrix(dVWC_JulSep[, c(3:(numlyrs+2))])}
      
      sSAND <- soilSAND[which(soilSAND$Label==s_name), c(2:(1+length(slyrwidths)))]
      
      sCLAY <- soilCLAY[which(soilCLAY$Label==s_name), c(2:(1+length(slyrwidths)))]
      sandMEANtop <- weighted.mean(sSAND[1:nlyrs], slyrwidths[1:nlyrs])
      clayMEANtop <- weighted.mean(sCLAY[1:nlyrs], slyrwidths[1:nlyrs])
      #dVWC_JulSep$count<-1:length(dVWC_JulSep$Year)
       dVWC_JulSep$SWP <- VWCtoSWP_simple(vwc=dVWC_JulSep$Alllyrs, sand=sandMEANtop, clay=clayMEANtop)
      #print(dVWC_JulSep$SWP[1:5])
	#print(head(dVWC_JulSep))
      d <- dVWC_JulSep[, c("Year", "Alllyrs", "Temp", "SWP")]
      #print(head(d))
      d_all_list<-split(d,d$Year)
      
      
      d_all_list1<- lapply(d_all_list,  FUN=function(x) {mean(x$Temp[order(x$Alllyrs)[1:10]])})
      d_all_list2<- lapply(d_all_list,  FUN=function(x) { if (length(which(x$SWP< -3))>9){mean(x$Temp[order(x$Temp[which(x$SWP< -3)])[1:10]])}else{NA}})

    		d1 <- ldply(d_all_list1, data.frame)
      		d2 <- ldply(d_all_list2, data.frame)
      		 names(d1)[2] <- c(name)
      		 names(d2)[2] <- c(name)
      		d1 <- as.data.frame(t(d1))[2,]
      		d2 <- as.data.frame(t(d2))[2,]
      		
      

      rownames(d1) <- c( name)
   
      rownames(d2) <- c( name)
      out<-list(XX=d1, HottestDays=d2)
      return(out)
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
 
      soildepths <- read.csv(file=file.path(dir.regions_1Input[r],  "SWRuns_InputData_SoilLayers_v9.csv"), header=TRUE )
      print(paste("soildepths", dim(soildepths)) )
      soildata <- read.csv(file=file.path(dir.regions_1Input[r], "datafiles" , "SWRuns_InputData_soils_v12.csv"), header=TRUE )
      print(paste("soildata", dim(soildata)) )
            
      #print(str(soildata))
      
      # metadata <- readRDS(file=file.path(dir.regions[r], "SFSW2_project_descriptions.rds") )
      # #str(metadata[["sim_time"]])
      # isim_time <- metadata[["sim_time"]]
      # simTime2 <- metadata[["sim_time"]]$sim_time2_North

      soilSAND <- soildata[, c(1, grep("Sand", names(soildata))) ]
      soilCLAY <- soildata[, c(1, grep("Clay", names(soildata))) ]
    
      sites <- list.files(dir.regions_3Runs[r])
        
        #print(sites[1:10])
        cl<-makeCluster(20)
       registerDoParallel(cl)
        
        Below3Temp_JulSep = foreach(s = sites, .combine = rbind,.packages=c('plyr','dplyr')) %dopar% {
          f <- list.files(file.path(dir.regions_3Runs[r], s) )
          if(length(f)==1){
            load(file.path(dir.regions_3Runs[r], s, "sw_output_sc1.RData"))
            print(s)
            d <- calcHotDry_JulSep(RUN_DATA = runDataSC, name=s)$HottestDays
            d
            
          }
        }
        stopCluster(cl)
        
        print(paste(regions[r], "Done"))
        print(Sys.time())
        
        ifelse (r == 1, annualBelow3Temp_JulSep <- Below3Temp_JulSep, annualBelow3Temp_JulSep <- rbind(annualBelow3Temp_JulSep, Below3Temp_JulSep))    
    }
    
annualBelow3Temp_JulSep<-as.numeric(as.character(annualBelow3Temp_JulSep))
names(annualBelow3Temp_JulSep) <- paste(c(1915:2015))
save(annualBelow3Temp_JulSep, file=file.path(dir.jbHOME, "Below3Temp_JulSep19152015.Rdata"))










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
