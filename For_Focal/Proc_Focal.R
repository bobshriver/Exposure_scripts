SpatialMean<-function(Var,county){
	apply(Var[,,which(points_Focal$UniqueStCo==county)],c(1,2),mean, na.rm=T) ##average over all points in county	
}

dir<-'Google Drive/USGS Research/AFRI/Exposure_Data'
load(file.path(dir,'Focal_Data.Rdata'))

FocalCT<-c( 'San Luis Obispo California', 'Santa Barbara California',
 'Humboldt Nevada',  'Rich Utah',  'Garfield Utah', 'Kane Utah',
 'Weld Colorado', 'Logan Colorado', 'Morgan Colorado', 'Luna New Mexico', 'Sierra New Mexico', 'Grant New Mexico', 'Hidalgo New Mexico', 'Gila Arizona',
 'Cherry Nebraska', 'Grant Nebraska')


###data is formatted as each point in a county, this loops averages over space###
Precip_SpatialMean<-Temp_SpatialMean<-SWPshallow_SpatialMean<-SWPdeep_SpatialMean<-array(NA,c(101,12,length(FocalCT)),dimnames = list(c(1915:2015),
                            c("Jan", "Feb", "Mar","Apr",'May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                            FocalCT))
for (i in 1:length(FocalCT)){
	Precip_SpatialMean[,,i]<-SpatialMean(Precip_Focal,FocalCT[i])
	Temp_SpatialMean[,,i]<-SpatialMean(Temp_Focal,FocalCT[i])
	SWPshallow_SpatialMean[,,i]<-SpatialMean(SWPshallow_Focal,FocalCT[i])
	SWPdeep_SpatialMean[,,i]<-SpatialMean(SWPdeep_Focal,FocalCT[i])

	
	
}




