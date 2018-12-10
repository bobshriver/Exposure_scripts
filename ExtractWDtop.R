# module load gcc/6.2.0
library('methods')
#library('dplyr')

#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/scratch/cma393/AFRI/Historical"
dir.jbHOME <- "/home/jbb239"

regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS") #list.files(dir.AFRI_Historical)

print(regions)

dir.regions <- file.path(dir.AFRI_Historical, regions, "3_Runs" )

# print(dir.regions)
# print("test")
# lp = (.packages())
#  print(lp)

 
 #Function for calculating annual value
    getyearlyWETDAY_toplyrs <- function(RUN_DATA, name){
      d_all <- as.data.frame(RUN_DATA@WETDAY@Day)
      d_all$topwet <- apply(d_all[, c(3:4)], 1, max)
      d2 <-aggregate(d_all, by=list(d_all$Year), FUN=sum, na.rm=TRUE)
      d2 <- d2[, c("Group.1", "topwet")]
      names(d2)[2] <- c(name)
      d3 <- as.data.frame(t(d2))
      rownames(d3) <- c("year", name)
      return(d3) 
    }

    getyearlyWETDAY_bottomlyrs <- function(RUN_DATA, name){
      d_all <- as.data.frame(RUN_DATA@WETDAY@Year)
      d_all$bottomwet <- apply(d_all[, -c(1:4)], 1, max)
      d2 <-aggregate(d_all, by=list(d_all$Year), FUN=sum, na.rm=TRUE)
      d2 <- d2[, c("Group.1", "bottomwet")]
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

# sites <- list.files(dir.regions[1])
# load(file.path(dir.regions[1], sites[1], "sw_output_sc1.RData"))
# d_all <- as.data.frame(runDataSC@WETDAY@Day)

for (r in 1:5){
  sites <- list.files(dir.regions[r])
  cl<-makeCluster(24)
  registerDoParallel(cl)
  
  annwetday_top = foreach(s = sites, .combine = rbind) %dopar% {
    f <- list.files(file.path(dir.regions[r], s) )
    if(length(f)==1){
      load(file.path(dir.regions[r], s, "sw_output_sc1.RData"))
      d <- getyearlyWETDAY_toplyrs(RUN_DATA = runDataSC, name=s)
      d[2,]
    }
  }
  stopCluster(cl)
  ifelse (r == 1, annualwetday_top <- annwetday_top, annualwetday_top <- rbind(annualwetday_top, annwetday_top))    
  
  
  # cl<-makeCluster(24)
  # registerDoParallel(cl)
  # 
  # annwetday_bottom = foreach(s = sites[1:5], .combine = rbind) %dopar% {
  #   f <- list.files(file.path(dir.regions[r], s) )
  #   if(length(f)==1){
  #     load(file.path(dir.regions[r], s, "sw_output_sc1.RData"))
  #     d <- getyearlyWETDAY_bottomlyrs(RUN_DATA = runDataSC, name=s)
  #     d[2,]
  #   }
  # }
  # stopCluster(cl)
  # ifelse (r == 1, annualwetday_bottom <- annwetday_bottom, annualwetday_bottom <- rbind(annualwetday_bottom, annwetday_bottom))
  # 
  
  
  print(paste(regions[r], "Done"))
  print(Sys.time())
  
}


names(annualwetday_top) <- paste(c(1915:2015))
save(annualwetday_top, file=file.path(dir.jbHOME, "annualwetday_top19152015"))
# 
# names(annualwetday_bottom) <- paste(c(1915:2015))
# save(annualwetday_bottom, file=file.path(dir.jbHOME, "annualwetday_bottom19152015"))

