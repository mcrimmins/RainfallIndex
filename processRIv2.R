# Process CPC CONUS PRECIP data for Rainfall Index Study
# 11/8/2018
# 11 interval version

library(ncdf4)
library(raster)

# load daily cropped AZ/NM data
# from AZ/NM bbox -115.268555,30.921076,-102.568359,37.230328
# e <- extent(-115.268555, -102.568359, 30.921076, 37.230328)
cpcPrecip<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/AZNM_cpc_precip_conus_1948_2017.grd")

#plot(cpcPrecip[[25568]])

# create date list
dateList<-as.data.frame(seq(as.Date("1948/1/1"), as.Date("2017/12/31"), "days"))
colnames(dateList)<-'date'
dateList$index<-as.integer(as.Date(paste0(format(dateList$date, format = "%m"),"/01/",format(dateList$date, format = "%Y")),
                             format='%m/%d/%Y'))+8037

#https://gis.stackexchange.com/questions/237272/mean-by-month-on-r-stacked-raster

#monthly totals - sum layers
moSumPrecip<- stackApply(cpcPrecip, dateList$index, fun = sum, na.rm = TRUE)
names(moSumPrecip) <- seq(as.Date("1948/1/1"), as.Date("2017/12/1"), "months")

#monthly averages - sum layers
indices <- format(as.Date(names(moSumPrecip), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)
moAvg<- stackApply(moSumPrecip, indices, fun = mean, na.rm = TRUE)
  names(moAvg) <- month.abb
twoMoSumClimo1<- stackApply(moAvg, c(1,1,2,2,3,3,4,4,5,5,6,6), fun = sum, na.rm = TRUE)
  names(twoMoSumClimo1)<-c('Jan-Feb','Mar-Apr','May-Jun','Jul-Aug','Sep-Oct','Nov-Dec')
twoMoSumClimo2<- stackApply(moAvg, c(1,2,2,3,3,4,4,5,5,6,6,1), fun = sum, na.rm = TRUE)  
  names(twoMoSumClimo2)<-c('Dec-Jan','Feb-Mar','Apr-May','Jun-Jul','Aug-Sep','Oct-Nov')
twoMoSumClimo<-stack(twoMoSumClimo1,twoMoSumClimo2)  
twoMoSumClimo<-subset(twoMoSumClimo, order(c(1,3,5,7,9,11,12,2,4,6,8,10)))
#plot(twoMoSumClimo, zlim=c(0,200))

# 2-month index - average moving window of two rasters
# percent of average 2 month index
twoMoSumPrecip <- stack()
for(i in 1:(nlayers(moSumPrecip)-1)){
  twoMoSumPrecip <- stack( twoMoSumPrecip , moSumPrecip[[i]]+moSumPrecip[[i+1]] )
}
names(twoMoSumPrecip) <- seq(as.Date("1948/1/1"), as.Date("2017/11/1"), "months")

# percent of average 2 month index
percAvgPrecip <- stack()
  avgIndex<-c(rep(1:12,69),c(1:11))
  for(i in 1:839){
    percAvgPrecip <- stack( percAvgPrecip , twoMoSumPrecip[[i]]/twoMoSumClimo[[avgIndex[i]]] )
}
names(percAvgPrecip)<-seq(as.Date("1948/1/1"), as.Date("2017/11/1"), "months")  

# write out data layers
writeRaster(moSumPrecip,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/moSumPrecip_AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )
writeRaster(moAvg,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/moAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )
writeRaster(twoMoSumClimo,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2AvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )
writeRaster(percAvgPrecip,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2percAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )
writeRaster(twoMoSumPrecip,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2sumAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )



  