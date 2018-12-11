# module load gcc/6.2.0
library('methods')


#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
#dir.AFRI_Historical <- "/scratch/cma393/AFRI/Historical"
dir.AFRI_Future <- "/lustre/projects/ecosystems/sbsc/SOILWAT_Outputs/AFRI/Future"

dir.jbHOME <- "/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data"



regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS", "Western_Gap")  #list.files(dir.AFRI_Historical)

print(regions)
dir.regions <- file.path(dir.AFRI_Future, regions)
dir.regions_3Runs <- file.path(dir.AFRI_Future, regions, "3_Runs" )
dir.regions_1Input <- file.path(dir.AFRI_Future, regions, "1_Input")

print(dir.regions_3Runs)
print(dir.regions_1Input)

#Function for calculating annual value

    getyearlyTRANSP <- function(reg, name){
      #   sites <- list.files(dir.regions_3Runs[1])
      #   name=sites[1]
      #   reg=1
      
      f <- list.files(file.path(dir.regions_3Runs[reg], name) )
      flevs <- substr(f, 11, nchar(f)-6)
      load(file.path(dir.regions_3Runs[reg], name, f[1]))
      dAll1 <- as.data.frame(runDataSC@TRANSP@Year)
      allTcols <- grep("total", names(dAll1), value=TRUE)
      dAll1$TotalLyrSum <- rowSums(dAll1[, c(allTcols)])
      dAll1 <- dAll1[, c("Year", "TotalLyrSum")]
      dALL2 <- dAll1[,c(1, 2)]
      dALL3 <- as.data.frame(t(dALL2))
      rownames(dALL3) <- c("year", name )
      dALL3$scLEV <- flevs[1]
      dALL3$site <- name
      
      dALL3 <- dALL3[2,]
      
      for (cf in c(2:45)){
        load(file.path(dir.regions_3Runs[reg], name, f[cf]))
        d1 <- as.data.frame(runDataSC@TRANSP@Year)
        aTcols <- grep("total", names(d1), value=TRUE)
        d1$TotalLyrSum <- rowSums(d1[, c(allTcols)])
        d1 <- d1[, c("Year", "TotalLyrSum")]
        d2 <- d1[,c(1, 2)]
        d3 <- as.data.frame(t(d2))
        rownames(d3) <- c("year", name )
        d3$scLEV <- flevs[cf]
        d3$site <- name
        
        dALL3 <- rbind(dALL3, d3[2,])
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

 for (r in 1:length(regions)){
  sites <- list.files(dir.regions_3Runs[r])
  cl<-makeCluster(20)
  registerDoParallel(cl)

  anntransp = foreach(s = sites, .combine = rbind) %dopar% {
    f <- list.files(file.path(dir.regions_3Runs[r], s) )

    if(length(f)==45){
      dsite <- getyearlyTRANSP(reg = r, name=s)
      dsite[c(1:46),]
    }
  }

  
    stopCluster(cl)

  print(paste(regions[r], "Done"))
  print(Sys.time())

  ifelse (r == 1, annualTranspFUTURE <- anntransp, annualTranspFUTURE <- rbind(annualTranspFUTURE, anntransp))
}

names(annualTranspFUTURE)[1:42] <- paste0("yr", c(1:42))
save(annualTranspFUTURE, file=file.path(dir.jbHOME, "annualTranspFUTURE"))



