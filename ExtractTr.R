# module load gcc/6.2.0
library('methods')


#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/lustre/projects/ecosystems/sbsc/SOILWAT_Outputs/AFRI/Historical"

dir.jbHOME <- "/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data"



regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS", "Western_Gap") #list.files(dir.AFRI_Historical)

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
 
    getyearlyTRANSP <- function(RUN_DATA, name){
      d_all <- as.data.frame(runDataSC@TRANSP@Year)
      allTcols <- grep("total", names(d_all), value=TRUE)
      d_all$TotalLyrSum <- rowSums(d_all[, c(allTcols)])
      d1 <- d_all[, c("Year", "TotalLyrSum")]
      print(str(d1))
      d2 <- d1[,c(1, 2)]
      print(str(d2))
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

  anntransp = foreach(s = sites, .combine = rbind) %dopar% {
    f <- list.files(file.path(dir.regions[r], s) )
    if(length(f)==1){
      load(file.path(dir.regions[r], s, "sw_output_sc1.RData"))
      d <- getyearlyTRANSP(RUN_DATA = runDataSC, name=s)
      d[2,]
    }
  }

    stopCluster(cl)

  print(paste(regions[r], "Done"))
  print(Sys.time())

  ifelse (r == 1, annualtransp <- anntransp, annualtransp <- rbind(annualtransp, anntransp))
  names(anntransp) <- paste(c(1915:2015))

}


names(annualtransp) <- paste(c(1915:2015))
save(annualtransp, file=file.path(dir.jbHOME, "annualtransp19152015"))


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


