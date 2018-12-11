# module load gcc/6.2.0
library('methods')


#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/lustre/projects/ecosystems/sbsc/SOILWAT_Outputs/AFRI/Historical"

dir.jbHOME <- "/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data"



regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS", "Western_Gap")  #list.files(dir.AFRI_Historical)

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

#Function for calculating annual value
 
    
    getWatYrTransp <- function(RUN_DATA, name){
      #RUN_DATA =runDataSC
      d1 <- as.data.frame(RUN_DATA@TRANSP@Month)
      d1$Year[which(d1$Month %in% c(10, 11, 12))] <- d1$Year[which(d1$Month %in% c(10, 11, 12))] + 1
      allTcols <- grep("total", names(d1), value=TRUE)
      d1$TotalLyrSum <- rowSums(d1[, c(allTcols)])
      
      d1 <- d1[, c("Year", "TotalLyrSum")]
      
      d2 <-aggregate(d1, by=list(d1$Year), FUN=sum, na.rm=TRUE)
      
      d2 <- d2[,c("Year", "TotalLyrSum")]
      names(d2)[2] <- c(name)
      d3 <- as.data.frame(t(d2))
      rownames(d3) <- c("year", name)
      return(d3) 
    }
    #head(d1, 20)

    #range(d1$Year)
    
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

  WYtransp = foreach(s = sites, .combine = rbind) %dopar% {
    f <- list.files(file.path(dir.regions[r], s) )
    if(length(f)==1){
      load(file.path(dir.regions[r], s, "sw_output_sc1.RData"))
      d <- getWatYrTransp(RUN_DATA = runDataSC, name=s)
      d[2,]
    }
  }

    stopCluster(cl)

  print(paste(regions[r], "Done"))
  print(Sys.time())

  ifelse (r == 1, WatYrtransp <- WYtransp, WatYrtransp <- rbind(WatYrtransp, WYtransp))
  names(WYtransp) <- paste(c(1915:2015))

}


names(WatYrtransp) <- paste(c(1915:2015))
save(WatYrtransp, file=file.path(dir.jbHOME, "WatYrtransp19152015"))


# 
# 
# 
# 
# 
# for (r in 1:5){
#   sites <- list.files(dir.regions[r])
#   cl<-makeCluster(24)
#   registerDoParallel(cl)
#   
# 
#   annwetday = foreach(s = sites, .combine = rbind) %dopar% {
#     load(file.path(dir.regions[r], s, "sw_output_sc1.RData"))
#     d <- getyearlyWETDAY(RUN_DATA = runDataSC, name=s)
#     d[2,]
#   }
#   stopCluster(cl)
# 
#   print(paste(regions[r], "Done"))
#   print(Sys.time())
# 
#   ifelse (r == 1, annualwetday <- annwetday, annualwetday <- rbind(annualwetday, annwetday))
#   names(annwetday) <- paste(c(1915:2015))
# 
# }
# 
# names(annualwetday) <- paste(c(1915:2015))
# save(annualwetday, file=file.path(dir.jbHOME, "annualwetday19152015"))


