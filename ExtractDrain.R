# module load gcc/6.2.0
library('methods')


#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/scratch/cma393/AFRI/Historical"
dir.jbHOME <- "/home/jbb239"

regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS") #list.files(dir.AFRI_Historical)

print(regions)
dir.regions <- file.path(dir.AFRI_Historical, regions)
dir.regions_3Runs <- file.path(dir.AFRI_Historical, regions, "3_Runs" )
dir.regions_1Input <- file.path(dir.AFRI_Historical, regions, "1_Input")

print(dir.regions_3Runs)
print(dir.regions_1Input)


#testfiles <- list.files(file.path(dir.regions[5], sitesSGS[1]))
#print(testfiles)

#print("file to load:")
#print(file.path(dir.regions[5], sitesSGS[1], "sw_output_sc1.RData"))
#load(file.path(dir.regions[5], sitesSGS[1], "sw_output_sc1.RData"))
#print(str(runDataSC))

#Function for calculating annual value

    getyearlyDRAIN <- function(RUN_DATA, name){
      #   sites <- list.files(dir.regions_3Runs[1])
      #   load(file.path(dir.regions_3Runs[1], sites[1], "sw_output_sc1.RData"))
      #   RUN_DATA <- runDataSC     str(RUN_DATA)
      #   name=sites[s]
      
      d1 <- as.data.frame(RUN_DATA@LYRDRAIN@Year)
     
      #print(str(d1))
      d2 <- d1[,c(1, dim(d1)[2])]
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

    library("parallel")
    library("foreach")
    library("doParallel")
    #detectCores()

for (r in 1:5){
  sites <- list.files(dir.regions_3Runs[r])
  cl<-makeCluster(24)
  registerDoParallel(cl)

  annDRAIN = foreach(s = sites, .combine = rbind) %dopar% {
    f <- list.files(file.path(dir.regions_3Runs[r], s) )
    if(length(f)==1){
      load(file.path(dir.regions_3Runs[r], s, "sw_output_sc1.RData"))
      d <- getyearlyDRAIN(RUN_DATA = runDataSC, name=s)
      d[2,]
    }
  }
  
    stopCluster(cl)

  print(paste(regions[r], "Done"))
  print(Sys.time())

  ifelse (r == 1, annualDRAIN <- annDRAIN, annualDRAIN <- rbind(annualDRAIN, annDRAIN))
  names(annDRAIN) <- paste(c(1915:2015))
}

names(annualDRAIN) <- paste(c(1915:2015))
save(annualDRAIN, file=file.path(dir.jbHOME, "annualDRAIN19152015"))



