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

  
#Function for calculating annual value
  calcWatYr_WETDAY_toplyrs <- function(RUN_DATA, name){
    d_all <- as.data.frame(RUN_DATA@WETDAY@Day)
    d_all$Year[which(d_all$DOY >273)] <- d_all$Year[which(d_all$DOY >273)] + 1
    
    d_all$topwet <- apply(d_all[, c(3:4)], 1, max)
    d2 <-aggregate(d_all, by=list(d_all$Year), FUN=sum, na.rm=TRUE)
    d2 <- d2[, c("Group.1", "topwet")]
    names(d2)[2] <- c(name)
    d3 <- as.data.frame(t(d2))
    rownames(d3) <- c("year", name)
    return(d3) 
  }



  getWatYr_WDtopFUT <- function(reg, name){
      #   sites <- list.files(dir.regions_3Runs[1])
      #   name=sites[1]
      #   reg=1
      
      f <- list.files(file.path(dir.regions_3Runs[reg], name) )
      flevs <- substr(f, 11, nchar(f)-6)
      load(file.path(dir.regions_3Runs[reg], name, f[1]))

      dALL3 <- calcWatYr_WETDAY_toplyrs(RUN_DATA = runDataSC, name=name)
      
      dALL3$scLEV <- flevs[1]
      dALL3$site <- name
      
      dALL3 <- dALL3[2,]
      
      for (cf in c(2:45)){
        load(file.path(dir.regions_3Runs[reg], name, f[cf]))
        dcf <- calcWatYr_WETDAY_toplyrs(RUN_DATA = runDataSC, name=name)
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

  sites <- list.files(dir.regions_3Runs[r])
  cl<-makeCluster(24)
  registerDoParallel(cl)

  WatYr_WDtop = foreach(s = sites, .combine = rbind) %dopar% {
    f <- list.files(file.path(dir.regions_3Runs[r], s) )

    if(length(f)==45){
      dsite <- getWatYr_WDtopFUT(reg = r, name=s)
      dsite[c(1:46),]
    }
  }

  
    stopCluster(cl)

  print(paste(regions[r], "Done"))
  print(Sys.time())

  ifelse (r == 1, WatYr_WDtopFUTURE <- WatYr_WDtop, WatYr_WDtopFUTURE <- rbind(WatYr_WDtopFUTURE, WatYr_WDtop))
}

names(WatYr_WDtopFUTURE)[1:42] <- paste0("yr", c(1:42))
save(WatYr_WDtopFUTURE, file=file.path(dir.jbHOME, "WatYr_WDtopFUTURE"))



