# process ClimGrid into CPC timescale and percent of avg format
# 11/19/18 MAC
# w/RESAMPLE TO CPC Version

# make sure precip in inches?

library(raster)
library(prism)

# set rasteroptions
rasterOptions(progress = 'text')

# dates 1895-2017 climgrid data
dates=seq(as.Date("1895-01-01"), as.Date("2017-12-1"), by="month")

# precip
prec<-stack('/scratch/crimmins/climgrid/processed/WESTmonthly.prcp.conus.pnt_1895_2017.grd')
  prec<-prec[[which(dates=="1948-01-01"):which(dates=="2017-12-01")]]

# CROP - AZ/NM bbox -115.268555,30.921076,-102.568359,37.230328
e <- extent(-115.268555, -102.568359, 30.921076, 37.230328)
prec <- crop((prec), e)

# RESAMPLE to CPC GRID
#moAvg<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/moAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd")
#cpcGrid<-moAvg[[1]]
#prec <- resample(prec,cpcGrid,method='bilinear')
# add names
names(prec) <- seq(as.Date("1948/1/1"), as.Date("2017/12/1"), "months")

#monthly averages - sum layers
indices <- format(as.Date(names(prec), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)
moAvg<- stackApply(prec, indices, fun = mean, na.rm = TRUE)
  names(moAvg) <- month.abb
# 2-month interval average
twoMoSumClimo1<- stackApply(moAvg, c(1,1,2,2,3,3,4,4,5,5,6,6), fun = sum, na.rm = TRUE)
  names(twoMoSumClimo1)<-c('Jan-Feb','Mar-Apr','May-Jun','Jul-Aug','Sep-Oct','Nov-Dec')
twoMoSumClimo2<- stackApply(moAvg, c(1,2,2,3,3,4,4,5,5,6,6,1), fun = sum, na.rm = TRUE)  
  names(twoMoSumClimo2)<-c('Dec-Jan','Feb-Mar','Apr-May','Jun-Jul','Aug-Sep','Oct-Nov')
twoMoSumClimo<-stack(twoMoSumClimo1,twoMoSumClimo2)  
twoMoSumClimo<-subset(twoMoSumClimo, order(c(1,3,5,7,9,11,12,2,4,6,8,10)))

# 2-month index - average moving window of two rasters
# percent of average 2 month index
twoMoSumPrecip <- stack()
for(i in 1:(nlayers(prec)-1)){
  twoMoSumPrecip <- stack(twoMoSumPrecip , prec[[i]]+prec[[i+1]] )
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
writeRaster(prec,filename="/scratch/crimmins/climgrid/processed/AZNM_RainIndex/moSumPrecip_AZNM_climgrid_1948_2017.grd", overwrite=TRUE )
writeRaster(moAvg,filename="/scratch/crimmins/climgrid/processed/AZNM_RainIndex/moAvgPrecip_AZNM_climgrid_1948_2017.grd", overwrite=TRUE )
writeRaster(twoMoSumClimo,filename="/scratch/crimmins/climgrid/processed/AZNM_RainIndex/mo2AvgPrecip_AZNM_climgrid_1948_2017.grd", overwrite=TRUE )
writeRaster(percAvgPrecip,filename="/scratch/crimmins/climgrid/processed/AZNM_RainIndex/mo2percAvgPrecip_AZNM_climgrid_1948_2017.grd", overwrite=TRUE )
writeRaster(twoMoSumPrecip,filename="/scratch/crimmins/climgrid/processed/AZNM_RainIndex/mo2sumAvgPrecip_AZNM_climgrid_1948_2017.grd", overwrite=TRUE )




