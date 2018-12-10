# module load gcc/6.2.0
library('methods')


#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
#dir.AFRI_Historical <- "/scratch/cma393/AFRI/Historical"
dir.AFRI_Future <- "/scratch/cma393/AFRI/Future"
dir.jbHOME <- "/home/jbb239"

regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS") #list.files(dir.AFRI_Historical)

print(regions)
dir.regions <- file.path(dir.AFRI_Future, regions)
dir.regions_3Runs <- file.path(dir.AFRI_Future, regions, "3_Runs" )
dir.regions_1Input <- file.path(dir.AFRI_Future, regions, "1_Input")

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

#Function for calculating annual value
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



  getWDDallFUT <- function(reg, name){
      #   sites <- list.files(dir.regions_3Runs[1])
      #   name=sites[1]
      #   reg=1
      
      f <- list.files(file.path(dir.regions_3Runs[reg], name) )
      flevs <- substr(f, 11, nchar(f)-6)
      load(file.path(dir.regions_3Runs[reg], name, f[1]))

      dALL3 <- calcWDDall(RUN_DATA = runDataSC, name=name)
      
      dALL3$scLEV <- flevs[1]
      dALL3$site <- name
      
      dALL3 <- dALL3[2,]
      
      for (cf in c(2:45)){
        load(file.path(dir.regions_3Runs[reg], name, f[cf]))
        dcf <- calcWDDall(RUN_DATA = runDataSC, name=name)
        dcf$scLEV <- flevs[cf]
        dcf$site <- name
        
        dALL3 <- rbind(dALL3, dcf[2,])
      }
      return(dALL3) 
      
    }
    
 

print("Start Loops")
print(Sys.time())

#Try in parallel
    
    library("parallel")
    library("foreach")
    library("doParallel")
    #detectCores()

 for (r in 1:5){
   
   #r=1
   soildepths <- read.csv(file=file.path(dir.regions_1Input[r],  "SWRuns_InputData_SoilLayers_v9.csv"), header=TRUE )
   print(paste("soildepths", dim(soildepths)) )
   soildata <- read.csv(file=file.path(dir.regions_1Input[r], "datafiles" , "SWRuns_InputData_soils_v12.csv"), header=TRUE )
   print(paste("soildata", dim(soildata)) )
  
   soilSAND <- soildata[, c(1, grep("Sand", names(soildata))) ]
   soilCLAY <- soildata[, c(1, grep("Clay", names(soildata))) ]
    
  sites <- list.files(dir.regions_3Runs[r])
  cl<-makeCluster(24)
  registerDoParallel(cl)

  annWDDall = foreach(s = sites, .combine = rbind) %dopar% {
    f <- list.files(file.path(dir.regions_3Runs[r], s) )

    if(length(f)==45){
      dsite <- getWDDallFUT(reg = r, name=s)
      dsite[c(1:46),]
    }
  }

  
    stopCluster(cl)

  print(paste(regions[r], "Done"))
  print(Sys.time())

  ifelse (r == 1, annualWDDallFUTURE <- annWDDall, annualWDDallFUTURE <- rbind(annualWDDallFUTURE, annWDDall))
}

names(annualWDDallFUTURE)[1:42] <- paste0("yr", c(1:42))
save(annualWDDallFUTURE, file=file.path(dir.jbHOME, "annualWDDallFUTURE"))



