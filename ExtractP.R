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
    getyearlyPRECIP <- function(RUN_DATA, name){

      d1 <- as.data.frame(RUN_DATA@PRECIP@Year)
      #print(str(d1))
      d2 <- d1[,c(1, 2)]
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
        print(dir.regions[r])
        sites <- list.files(dir.regions[r])
        #print(dim(sites))
        cl<-makeCluster(20)
        registerDoParallel(cl)

        annprecip = foreach(s = sites, .combine = rbind) %dopar% {
          f <- list.files(file.path(dir.regions[r], s) )
          if(length(f)==1){
            load(file.path(dir.regions[r], s, "sw_output_sc1.RData"))
            d <- getyearlyPRECIP(RUN_DATA = runDataSC, name=s)
            d[2,]
          }
        }
        stopCluster(cl)

        print(paste(regions[r], "Done"))
        print(Sys.time())
        
        ifelse (r == 1, annualprecip <- annprecip, annualprecip <- rbind(annualprecip, annprecip))    
        names(annprecip) <- paste(c(1915:2015))
    }

names(annualprecip) <- paste(c(1915:2015))
save(annualprecip, file=file.path(dir.jbHOME, "annualprecip19152015"))



