# module load gcc/6.2.0
library('methods')
library(plyr)


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
      ####For Years
      d_all <- as.data.frame(RUN_DATA@TRANSP@Month)
  d_all$Year[which(d_all$Month %in% c(10, 11, 12))] <- d_all$Year[which(d_all$Month %in% c(10, 11, 12))] + 1
  d_all <-aggregate(d_all, by=list(d_all$Year), FUN=sum, na.rm=TRUE)
  d_all$Year<-d_all[,1]
  
      allTcols <- grep("total", names(d_all), value=TRUE)
      d_all$TotalLyrSum <- rowSums(d_all[, c(allTcols)])
      d1 <- d_all[, c("Year", "TotalLyrSum")]
     
     ####For Days 
     d_allD <- as.data.frame(runDataSC@TRANSP@Day)
      allTcolsD <- grep("total", names(d_allD), value=TRUE)
      d_allD$TotalLyrSum <- rowSums(d_allD[, c(allTcolsD)])
      d_all_list<-split(d_allD,d_allD$Year)
      
      d_all_list<- lapply(d_all_list,  FUN=cumsum)
    		d_all_df <- ldply(d_all_list, data.frame)
     d_allD$TotalLyrSum<-d_all_df$TotalLyrSum
     YearbyDay<-merge(d_all[,c("Year","TotalLyrSum")],d_allD[,c("Year","Day","TotalLyrSum")], by="Year", all.y=T)
     YearbyDay$Q<-YearbyDay$TotalLyrSum.y/YearbyDay$TotalLyrSum.x
     
      YearbyDay<-split(YearbyDay,YearbyDay$Year)
	YearbyDayQ<- ldply(lapply(YearbyDay,  FUN=function(x){min(which(x$Q>.1))}),data.frame)		
     
      d2 <- YearbyDayQ
      #print(str(d2))
      names(d2)[2] <- c(name)
      #print(str(d2))
      d3 <- as.data.frame(t(d2))
      #print(str(d3))
      rownames(d3) <- c("year", name)
      #print(str(d3))
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

  anntransp = foreach(s = sites, .combine = rbind,.packages=c('plyr')) %dopar% {
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
  names(anntransp) <- paste(c(1915:2016))

}


names(annualtransp) <- paste(c(1915:2016))
save(annualtransp, file=file.path(dir.jbHOME, "WatYrtransp10_19152015"))


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


