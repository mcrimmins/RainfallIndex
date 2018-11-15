# Process CPC CONUS PRECIP data for Rainfall Index Study
# 11/8/2018

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
twoMoSumClimo<- stackApply(moAvg, c(6,1,1,2,2,3,3,4,4,5,5,6), fun = sum, na.rm = TRUE)
  names(twoMoSumClimo)<-c('Dec-Jan','Feb-Mar','Apr-May','Jun-Jul','Aug-Sep','Oct-Nov')
twoMoSumClimo<-subset(twoMoSumClimo, order(c(6,1,2,3,4,5)))

  
# create 2 month index  
monthsDate<-as.data.frame(seq(as.Date("1948/1/1"), as.Date("2017/12/1"), "months"))
  colnames(monthsDate)<-"date"
  monthsDate$month<-format(monthsDate$date, format = "%m")
  moIndex<-c(0,rep(c(1,1,2,2,3,3,4,4,5,5,6,6),70))
  monthsDate$index2mo<-moIndex[1:840]
  stackIndex<-c(0,rep(1:840, each=2))
  monthsDate$stackIndex<-stackIndex[1:840]
# trim first/last incomplete 2 month periods
  monthsDate<-monthsDate[2:839,]
  trimMoSumPrecip<-moSumPrecip[[2:839]]
twoMoSumPrecip<- stackApply(trimMoSumPrecip, monthsDate$stackIndex, fun = sum, na.rm = TRUE)  

# percent of average 2 month index
percAvgPrecip <- stack()
  avgIndex<-c(rep(1:6,69),c(1:5))
  for(i in 1:419){
    percAvgPrecip <- stack( percAvgPrecip , twoMoSumPrecip[[i]]/twoMoSumClimo[[avgIndex[i]]] )
}
names(percAvgPrecip)<-seq(as.Date("1948/2/1"), as.Date("2017/11/1"), "2 months")  

# write out data layers
writeRaster(moSumPrecip,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/moSumPrecip_AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )
writeRaster(moAvg,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/moAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )
writeRaster(twoMoSumClimo,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2AvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )
writeRaster(percAvgPrecip,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2percAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )



  