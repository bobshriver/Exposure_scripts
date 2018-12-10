# module load gcc/6.2.0
library('methods')


#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/scratch/cma393/AFRI/Historical"
dir.jbHOME <- "/home/jbb239"

regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS") #list.files(dir.AFRI_Historical)

print(regions)

dir.regions <- file.path(dir.AFRI_Historical, regions, "3_Runs" )

print(dir.regions)

#sitesSGS <- list.files(dir.regions[5])

#print(sitesSGS[1:3])


#testfiles <- list.files(file.path(dir.regions[5], sitesSGS[1]))
#print(testfiles)

#print("file to load:")
#print(file.path(dir.regions[5], sitesSGS[1], "sw_output_sc1.RData"))
#load(file.path(dir.regions[5], sitesSGS[1], "sw_output_sc1.RData"))
#print(str(runDataSC))


getWatYrPRECIP <- function(RUN_DATA, name){
  #RUN_DATA =runDataSC
  d1 <- as.data.frame(RUN_DATA@PRECIP@Month)
  d1$Year[which(d1$Month %in% c(10, 11, 12))] <- d1$Year[which(d1$Month %in% c(10, 11, 12))] + 1
  d2 <-aggregate(d1, by=list(d1$Year), FUN=sum, na.rm=TRUE)
  
  d2 <- d2[,c("Year", "ppt")]
  names(d2)[2] <- c(name)
  d3 <- as.data.frame(t(d2))
  rownames(d3) <- c("year", name)
  return(d3) 
}
    
print("Start Loops")
print(Sys.time())

#Try in parallel
    
    library("parallel")
    library("foreach")
    library("doParallel")
    #detectCores()

   
    for (r in 1:5){
        print(dir.regions[r])
        sites <- list.files(dir.regions[r])
        #print(dim(sites))
        cl<-makeCluster(24)
        registerDoParallel(cl)

        WYprecip = foreach(s = sites, .combine = rbind) %dopar% {
          f <- list.files(file.path(dir.regions[r], s) )
          if(length(f)==1){
            load(file.path(dir.regions[r], s, "sw_output_sc1.RData"))
            d <- getWatYrPRECIP(RUN_DATA = runDataSC, name=s)
            d[2,]
          }
        }
        stopCluster(cl)

        print(paste(regions[r], "Done"))
        print(Sys.time())
        
        ifelse (r == 1, WatYrprecip <- WYprecip, WatYrprecip <- rbind(WatYrprecip, WYprecip))    
        #names(WYprecip) <- paste(c(1915:2015))
    }

names(WatYrprecip) <- paste(c(1915:2015))
save(WatYrprecip, file=file.path(dir.jbHOME, "WatYrprecip19152015"))



