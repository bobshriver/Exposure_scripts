# module load gcc/6.2.0
library('methods')


#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/lustre/projects/ecosystems/sbsc/SOILWAT_Outputs/AFRI/Historical"

dir.jbHOME <- "/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data"



regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS", "Western_Gap")  #list.files(dir.AFRI_Historical)

print(regions)

dir.regions <- file.path(dir.AFRI_Historical, regions, "3_Runs" )

print(dir.regions)


#Function for calculating annual value

     getWatYrTEMP <- function(RUN_DATA, name){
      #RUN_DATA =runDataSC
      d1 <- as.data.frame(RUN_DATA@TEMP@Month)
      d1<-d1(which(d1$Month>4 & d1$Month<11 ))
      d2 <-aggregate(d1, by=list(d1$Year), FUN=mean, na.rm=TRUE)
      
      d2 <- d2[,c("Year", "avg_C")]
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

   


for (r in 1:length(regions)){
  sites <- list.files(dir.regions[r])
  cl<-makeCluster(20)
  registerDoParallel(cl)

  WYtemp = foreach(s = sites, .combine = rbind) %dopar% {
    f <- list.files(file.path(dir.regions[r], s) )
    if(length(f)==1){
    load(file.path(dir.regions[r], s, "sw_output_sc1.RData"))
    d <- getWatYrTEMP(RUN_DATA = runDataSC, name=s)
    d[2,]
    }
  }
  
    stopCluster(cl)

#print(str(WYtemp))
#if(r>1) print(str(WatYrtemp))

  print(paste(regions[r], "Done"))
  print(Sys.time())

  ifelse (r == 1, WatYrtemp <- WYtemp, WatYrtemp <- rbind(WatYrtemp, WYtemp))
  #names(WatYrtemp) <- paste(c(1915:2015))
}

names(WatYrtemp) <- paste(c(1915:2015))
save(WatYrtemp, file=file.path(dir.jbHOME, "WatYrtemp19152015"))



