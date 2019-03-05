library("sp")
library('methods')
library("plyr")
library("dplyr")
#library(rSOILWAT2)

#These are the functions I need:
# if (!exists("vwcmatric.dy")) vwcmatric.dy <- get_Response_aggL(swof["sw_vwcmatric"], tscale = "dy", 
#                                                                scaler = 1, FUN = stats::weighted.mean, weights = layers_width, 
#                                                                x = runDataSC, st = isim_time, st2 = simTime2, topL = topL, bottomL = bottomL)
# if (!exists("swpmatric.dy")) swpmatric.dy <- get_SWPmatric_aggL(vwcmatric.dy, texture, sand, clay)

#dir.AFRI_Historical <- "/projects/ecogis/SOILWAT2_Projects/AFRI/Historical"
dir.AFRI_Historical <- "/cxfs/projects/usgs/ecosystems/sbsc/AFRI/Historical"

dir.jbHOME <- "/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data"



regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS", "Western_Gap")#list.files(dir.AFRI_Historical)

print(regions)
dir.regions <- file.path(dir.AFRI_Historical, regions)
dir.regions_3Runs <- file.path(dir.AFRI_Historical, regions, "3_Runs" )
dir.regions_1Input <- file.path(dir.AFRI_Historical, regions, "1_Input")
load("/cxfs/projects/usgs/ecosystems/sbsc/drylandeco/AFRI/Exposure_Data/points_Focal.Rdata")

print(dir.regions_3Runs)
print(dir.regions_1Input)



    VWCtoSWP_simple <- function(vwc, sand, clay){
      #Outputs SWP as negative MPa
      bar_toMPa = -0.1
      bar_conversion = 1024
      
      thetas <- -14.2 * sand - 3.7 * clay + 50.5
      psis <- 10 ^ (-1.58 * sand - 0.63 * clay + 2.17)
      b <- -0.3 * sand + 15.7 * clay + 3.10
      res <- psis / ((vwc * 100 / thetas) ^ b * bar_conversion) * bar_toMPa
      return(res)
    }
    








SWPdeep_Focal<-SWPshallow_Focal<-Temp_Focal<-Precip_Focal<-array(NA,dim=c(101,12,length(points_Focal[,1])))## Dims= [year,month,site]Create storage arrays for data. One is created for each variable 

	
	Extract_Focal<- function(RUN_DATA, name){

	temp<-matrix(as.data.frame(RUN_DATA@TEMP@Month)[,5],101,12, byrow=T)
	precip<-matrix(as.data.frame(RUN_DATA@PRECIP@Month)[,3],101,12, byrow=T)
	
	      dVWC <- as.data.frame(RUN_DATA@VWCMATRIC@Month)
	 s_name <- paste0("Site_", as.integer(substr(name, 1, regexpr('_', name)-1)) )
      sdepths <- as.vector(soildepths[which(soildepths$Label==s_name), -1])
      maxdepth <- as.integer(sdepths[1])
      sdepths[sdepths > maxdepth ] <- NA
      sdepth <- sdepths[-1]
      slyrwidths <- diff(c(0, na.omit(t(sdepth)) ) )
      numlyrs <- dim(dVWC)[2] - 2
      
      nlyrs<-if(numlyrs<4){numlyrs} else {3}
      print(nlyrs)
      if(numlyrs>1 & numlyrs<4 ){dVWC$shallow<- apply(as.matrix(dVWC[, c(4:(numlyrs+2))]), 1, FUN=function(x) weighted.mean(x, slyrwidths[2:nlyrs]))} 
      if(numlyrs>1 & numlyrs>3 ){dVWC$shallow<- apply(as.matrix(dVWC[, c(4:(3+2))]), 1, FUN=function(x) weighted.mean(x, slyrwidths[2:nlyrs]))}
      if(numlyrs==1){dVWC$shallow <- NA} ###as.matrix(dVWC[, c(3:(numlyrs+2))])
      
      
      # if(numlyrs>1){
      #sSAND <- soilSAND[which(soilSAND$Label==s_name), c(2:(1+length(slyrwidths)))]
      #sCLAY <- soilCLAY[which(soilCLAY$Label==s_name), c(2:(1+length(slyrwidths)))]
      #sandMEANtop <- weighted.mean(sSAND[2:nlyrs], slyrwidths[2:nlyrs])
      #clayMEANtop <- weighted.mean(sCLAY[2:nlyrs], slyrwidths[2:nlyrs])
       #dVWC$SWPshallow <- VWCtoSWP_simple(vwc=dVWC$Alllyrs, sand=sandMEANtop, clay=clayMEANtop)
#} else{ dVWC$shallow <-NA}


  nlyrs<-if(numlyrs<7 ){numlyrs} else {6}
      #print(nlyrs)
      if(numlyrs>4 & numlyrs<7 ){dVWC$deep <- apply(as.matrix(dVWC[, c(7:(numlyrs+2))]), 1, FUN=function(x) weighted.mean(x, slyrwidths[5:nlyrs]))} 
      if(numlyrs>4 & numlyrs>6 ){dVWC$deep <- apply(as.matrix(dVWC[, c(7:(6+2))]), 1, FUN=function(x) weighted.mean(x, slyrwidths[5:nlyrs]))}
      if(numlyrs<5){dVWC$deep <- NA} ###as.matrix(dVWC[, c(3:(numlyrs+2))])
      
     
     # if(numlyrs>4){
      #sSAND <- soilSAND[which(soilSAND$Label==s_name), c(2:(1+length(slyrwidths)))] 
      #sCLAY <- soilCLAY[which(soilCLAY$Label==s_name), c(2:(1+length(slyrwidths)))]
      #sandMEANtop <- weighted.mean(sSAND[5:nlyrs], slyrwidths[5:nlyrs])
     # clayMEANtop <- weighted.mean(sCLAY[5:nlyrs], slyrwidths[5:nlyrs])
       #dVWC$SWPdeep <- VWCtoSWP_simple(vwc=dVWC$Alllyrs, sand=sandMEANtop, clay=clayMEANtop)
#} else{ dVWC$SWPdeep<-NA}
	
	shallow<-matrix(dVWC$shallow,101,12, byrow=T)
	deep<-matrix(dVWC$deep,101,12, byrow=T)
	
	my_list <- list("shallow" = shallow, "deep" = deep, "precip" = precip, 'temp'=temp)
return(my_list)

}


for (i in 1:dim(points_Focal)[1]){
	
	  soildepths <- read.csv(file=file.path(dir.AFRI_Historical,points_Focal$region[i],"1_Input",  "SWRuns_InputData_SoilLayers_v9.csv"), header=TRUE )
      print(paste("soildepths", dim(soildepths)) )
      #soildata <- read.csv(file=file.path(dir.AFRI_Historical,points_Focal$region[i],"1_Input", "datafiles" , "SWRuns_InputData_soils_v12.csv"), header=TRUE )
      #print(paste("soildata", dim(soildata)) )
            
  

      #soilSAND <- soildata[, c(1, grep("Sand", names(soildata))) ]
      #soilCLAY <- soildata[, c(1, grep("Clay", names(soildata))) ]
    
     f <- list.files(file.path(dir.AFRI_Historical,points_Focal$region[i],"3_Runs",  points_Focal$AFRIpoint[i]) )
          if(length(f)==1){
            load(file.path(dir.AFRI_Historical,points_Focal$region[i],"3_Runs",  points_Focal$AFRIpoint[i], "sw_output_sc1.RData"))
            print(i)
          out<-Extract_Focal(RUN_DATA = runDataSC, name= points_Focal$AFRIpoint[i])
          	SWPshallow_Focal[,,i]<-out$shallow
			SWPdeep_Focal[,,i]<-out$deep
			
	Temp_Focal[,,i]<-out$temp
	Precip_Focal[,,i]<-out$precip
            
          }

       
	
}


save('Temp_Focal','Precip_Focal','SWPshallow_Focal','SWPdeep_Focal','points_Focal',file=file.path(dir.jbHOME, "Focal_Data.Rdata"))