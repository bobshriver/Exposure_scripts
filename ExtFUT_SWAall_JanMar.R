# module load gcc/6.2.0
library('methods')
#   citation("rSOILWAT2")
citation()

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

      
      calcSWA_JanMar <- function(RUN_DATA, name){
      #print("Pre d1")
      #print(Sys.time())
      # s=1
      #   sites <- list.files(dir.regions_3Runs[1])
      #   load(file.path(dir.regions_3Runs[1], sites[s], "sw_output_sc1.RData"))
      #   RUN_DATA <- runDataSC
      #   name=sites[s]
      
      dSWA <- as.data.frame(RUN_DATA@SWABULK@Month)
      dSWA_AprJun <- dSWA[which(dSWA$Month %in% c(1:3)),]
      #head(dSWA_AprJun)
      numlyrs <- dim(dSWA)[2] - 2
      dSWA_AprJun$Alllyrs <- rowSums(dSWA_AprJun[, c(3:numlyrs+2)])
      
      d <- dSWA_AprJun[, c("Year", "Alllyrs")]
      
      d2 <-aggregate(d, by=list(d$Year), FUN=mean, na.rm=TRUE)
      d2 <- d2[, c("Group.1", "Alllyrs")]
      names(d2)[2] <- c(name)
      d3 <- as.data.frame(t(d2))
      rownames(d3) <- c("year", name)
      return(d3)
    }
  

  getSWA_JM_FUT <- function(reg, name){
      #   sites <- list.files(dir.regions_3Runs[1])
      #   name=sites[1]
      #   reg=1
      
      f <- list.files(file.path(dir.regions_3Runs[reg], name) )
      flevs <- substr(f, 11, nchar(f)-6)
      load(file.path(dir.regions_3Runs[reg], name, f[1]))

      dALL3 <- calcSWA_JanMar(RUN_DATA = runDataSC, name=name)
      
      dALL3$scLEV <- flevs[1]
      dALL3$site <- name
      
      dALL3 <- dALL3[2,]
      
      for (cf in c(2:45)){
        load(file.path(dir.regions_3Runs[reg], name, f[cf]))
        dcf <- calcSWA_JanMar(RUN_DATA = runDataSC, name=name)
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

  annSWA_JanMar = foreach(s = sites, .combine = rbind) %dopar% {
    f <- list.files(file.path(dir.regions_3Runs[r], s) )

    if(length(f)==45){
      dsite <- getSWA_JM_FUT(reg = r, name=s)
      dsite[c(1:46),]
    }
  }

    stopCluster(cl)

  print(paste(regions[r], "Done"))
  print(Sys.time())

  ifelse (r == 1, annualSWA_JanMarFUTURE <- annSWA_JanMar, annualSWA_JanMarFUTURE <- rbind(annualSWA_JanMarFUTURE, annSWA_JanMar))
}

names(annualSWA_JanMarFUTURE)[1:42] <- paste0("yr", c(1:42))
save(annualSWA_JanMarFUTURE, file=file.path(dir.jbHOME, "annualSWA_JanMarFUTURE"))



