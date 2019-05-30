library("sp")
library('methods')
#library("dplyr")
#library(rSOILWAT2)


SWPtoVWC <- function(swp, sand, clay) {
		#Cosby, B. J., G. M. Hornberger, R. B. Clapp, and T. R. Ginn. 1984. A statistical exploration of the relationships of soil moisture characteristics to the physical properties of soils. Water Resources Research 20:682-690.
	
			#1. SWP in MPa [single value] + sand and clay in fraction [single values] --> VWC in fraction [single value]
			#2. SWP in MPa [single value] + sand and clay in fraction [vectors of length d] --> VWC in fraction [vector of length d]
			#3. SWP in MPa [vector of length l] + sand and clay in fraction [single values] --> VWC in fraction [vector of length l]
			#4. SWP in MPa [vector of length l] + sand and clay in fraction [vectors of length d] --> VWC in fraction [matrix with nrow=l and ncol=d, SWP vector repeated for each column]: probably not used
			#5. SWP in MPa [matrix with nrow=l and ncol=d] + sand and clay in fraction [single values] --> VWC in fraction [matrix with nrow=l and ncol=d]
			#6. SWP in MPa [matrix with nrow=l and ncol=d] + sand and clay in fraction [vectors of length d] --> VWC in fraction [matrix with nrow=l and ncol=d, sand/clay vector repeated for each row]
	
			stopifnot(length(sand) == length(clay))
			na.act <- na.action(na.exclude(apply(data.frame(sand, clay), MARGIN=1, FUN=sum)))
	
			if(length(sand) > length(na.act)){
				na.index <- as.vector(na.act)
		
				if(length(na.index) > 0){
					sand <- sand[-na.index]
					clay <- clay[-na.index]
				}
		
				thetas <- -14.2 * sand - 3.7 * clay + 50.5
				psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
				b <- -0.3 * sand + 15.7 * clay + 3.10
				if(any(b <= 0)) stop("b <= 0")
		
				bar_conversion <- 1024
				MPa_toBar <- -10
		
				get_vector <- function(swp, sand, clay, thetas=thetas, psis=psis, b=b){#either swp or sand/clay needs be a single value
					vwc <- ifelse(!is.na(swp) & swp <= 0 & sand <= 1 & sand >= 0 & clay <= 1 & clay >= 0, thetas * (psis / (swp * MPa_toBar * bar_conversion))^(1/b) / 100, NA)
					if( length(na.index) > 0){
						vwc <- napredict(na.act, vwc)
					}
					return(vwc)
				}
		
				if(is.null(dim(swp))){
					if(length(swp) == 1 & length(sand) >= 1 | length(swp) >= 1 & length(sand) == 1){ #cases 1-3		
						vwc <- get_vector(swp, sand, clay, thetas=thetas, psis=psis, b=b)
					} else if(length(swp) > 1 & length(sand) > 1){ #case 4
						vwc <- t(sapply(1:length(swp), FUN=function(d) get_vector(swp[d], sand, clay, thetas=thetas, psis=psis, b=b)))
					}
				} else {
					if(length(sand) == 1){ #case 5
						vwc <- sapply(1:ncol(swp), FUN=function(d) get_vector(swp[, d], sand, clay, thetas=thetas, psis=psis, b=b))
					} else { #case 6
						sand <- napredict(na.act, sand)
						clay <- napredict(na.act, clay)
						stopifnot(ncol(swp) == length(sand))
						psis <- napredict(na.act, psis)
						thetas <- napredict(na.act, thetas)
						b <- napredict(na.act, b)
						vwc <- sapply(1:ncol(swp), FUN=function(d) get_vector(swp[, d], sand[d], clay[d], thetas=thetas[d], psis=psis[d], b=b[d]))
					}
				}
			} else {
				vwc <- swp
				vwc[!is.na(vwc)] <- NA
			}
			return(vwc) #fraction m3/m3 [0, 1]
		}


#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/cxfs/projects/usgs/ecosystems/sbsc/AFRI/Historical"

dir.jbHOME <- "/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data"



regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS", "Western_Gap") #list.files(dir.AFRI_Historical)

print(regions)
dir.regions <- file.path(dir.AFRI_Historical, regions)
dir.regions_3Runs <- file.path(dir.AFRI_Historical, regions, "3_Runs" )
dir.regions_1Input <- file.path(dir.AFRI_Historical, regions, "1_Input")

print(dir.regions_3Runs)
print(dir.regions_1Input)


#Function for calculating WDD
    calcSWA_JanMar <- function(RUN_DATA, name){
      
#print("Pre d1")
      #print(Sys.time())
      # s=1
      #   sites <- list.files(dir.regions_3Runs[1])
      #   load(file.path(dir.regions_3Runs[1], sites[s], "sw_output_sc1.RData"))
      #   RUN_DATA <- runDataSC
      #   name=sites[s]
      
      dSWA <- as.data.frame(RUN_DATA@VWCMATRIC@Month)
      dSWA_AprJun <- dSWA[which(dSWA$Month %in% c(1:3)),]
     
     
      s_name <- paste0("Site_", as.integer(substr(name, 1, regexpr('_', name)-1)) )
      sdepths <- as.vector(soildepths[which(soildepths$Label==s_name), -1])
      #str(sdepths)
      maxdepth <- as.integer(sdepths[1])
      #str(maxdepth)
      sdepths[sdepths > maxdepth ] <- NA
      sdepth <- sdepths[-1]
      slyrwidths <- diff(c(0, na.omit(t(sdepth)) ) )
      numlyrs <- dim(dSWA)[2] - 2
     
      sSAND <- soilSAND[which(soilSAND$Label==s_name), c(2:(1+length(slyrwidths)))]
      sCLAY <- soilCLAY[which(soilCLAY$Label==s_name), c(2:(1+length(slyrwidths)))]
      
      threshold<-unlist((SWPtoVWC(-3.9,sSAND,sCLAY)))
      
     dSWA_AprJun[,3:(numlyrs+2)]<-t((t(dSWA_AprJun[,3:(numlyrs+2)])-threshold)*slyrwidths) 
     dSWA_AprJun[which(dSWA_AprJun<0, arr.ind=T)]<-0
     
     
      
      head(dSWA_AprJun)
      numlyrs <- dim(dSWA)[2] - 2
        if(numlyrs>1){dSWA_AprJun$Alllyrs <- rowSums(as.matrix(dSWA_AprJun[, c(3:(numlyrs+2))]))} else{
        dSWA_AprJun$Alllyrs <- as.matrix(dSWA_AprJun[, c(3:(numlyrs+2))])}
      
      d <- dSWA_AprJun[, c("Year", "Alllyrs")]
      
      d2 <-aggregate(d, by=list(d$Year), FUN=mean, na.rm=TRUE)
      d2 <- d2[, c("Group.1", "Alllyrs")]
      names(d2)[2] <- c(name)
      d3 <- as.data.frame(t(d2))
      rownames(d3) <- c("year", name)
      return(d3)
    }

print("Start Loop")
print(Sys.time())

#Try in parallel
    library("parallel")
    library("foreach")
    library("doParallel")
    #detectCores()

    for (r in 1:length(regions)){
      # r=1
 
      #print(str(soildata))
    
        soildepths <- read.csv(file=file.path(dir.regions_1Input[r],  "SWRuns_InputData_SoilLayers_v9.csv"), header=TRUE )
      print(paste("soildepths", dim(soildepths)) )
      soildata <- read.csv(file=file.path(dir.regions_1Input[r], "datafiles" , "SWRuns_InputData_soils_v12.csv"), header=TRUE )
      print(paste("soildata", dim(soildata)) )
            
      #print(str(soildata))
      
      # metadata <- readRDS(file=file.path(dir.regions[r], "SFSW2_project_descriptions.rds") )
      # #str(metadata[["sim_time"]])
      # isim_time <- metadata[["sim_time"]]
      # simTime2 <- metadata[["sim_time"]]$sim_time2_North

      soilSAND <- soildata[, c(1, grep("Sand", names(soildata))) ]
      soilCLAY <- soildata[, c(1, grep("Clay", names(soildata))) ]
      
      sites <- list.files(dir.regions_3Runs[r])
        
        #print(sites[1:10])
        cl<-makeCluster(20)
        registerDoParallel(cl)
        
        SWA_JanMar = foreach(s = sites, .combine = rbind) %dopar% {
          f <- list.files(file.path(dir.regions_3Runs[r], s) )
          if(length(f)==1){
            load(file.path(dir.regions_3Runs[r], s, "sw_output_sc1.RData"))
            d <- calcSWA_JanMar(RUN_DATA = runDataSC, name=s)
            d[2,]
          }
        }
        stopCluster(cl)
        
        print(paste(regions[r], "Done"))
        print(Sys.time())
        
        ifelse (r == 1, annualSWA_JanMar <- SWA_JanMar, annualSWA_JanMar <- rbind(annualSWA_JanMar, SWA_JanMar))    
    }
    

names(annualSWA_JanMar) <- paste(c(1915:2015))
save(annualSWA_JanMar, file=file.path(dir.jbHOME, "annualSWA_JanMar19152015_2"))










#DEVELOPMENT

# soildepths <- read.csv(file=file.path(dir.regions_1Input[1],  "SWRuns_InputData_SoilLayers_v9.csv"), header=TRUE )
# 
# soildata <- read.csv(file=file.path(dir.regions_1Input[1], "datafiles",  "SWRuns_InputData_soils_v12.csv"), header=TRUE )
# 
# metadata <- readRDS(file=file.path(dir.regions[1], "SFSW2_project_descriptions.rds") )
# #str(metadata[["sim_time"]])
# isim_time <- metadata[["sim_time"]]
# simTime2 <- metadata[["sim_time"]]$sim_time2_North
# 
# layers_width <- getLayersWidth(layers_depth)
# 
# load(file.path(dir.regions_3Runs[1], sites[1], "sw_output_sc1.RData"))
# dtemps <- as.data.frame(runDataSC@TEMP@Day)
# dVWC <- as.data.frame(runDataSC@VWCMATRIC@Day)
# dwd <- as.data.frame(runDataSC@WETDAY@Day)
# dSM <- as.data.frame(runDataSC@SWPMATRIC@Day)
# str(dSM)
# names(dSM)[c(-1, -2)] <- paste("SM", names(dSM)[c(-1, -2)])
# d_all2 <- merge(d_all, dSM, by=c("Year", "Day"))
# d_all2[c(3050: 3080),]
#dSNOW <- as.data.frame(runDataSC@SNOWPACK@Day)

#dtst <-aggregate(d_all, by=list(d$Year), FUN=length(), na.rm=TRUE)
