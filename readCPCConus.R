# Read in CPC CONUS PRECIP data for Rainfall Index Study
# 8/16/2018


# get CPC precip from netcdf files
# need to convert lon values to negative, something in raster will do this
# how to extract bands or average them up to monthly values
library(ncdf4)
library(raster)

yr1<-1948
yr2<-2017

for(i in yr1:yr2){
  paste0(i)
  cpc.prcp.file <- paste0("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/precip.V1.0.",i,".nc")
  prcp.tmp <- brick(cpc.prcp.file, varname="precip",  ncdf=TRUE)

  # store tempGrids in stack  
  if (i==yr1){
    tempGrid2 <-  prcp.tmp
  }else{
    tempGrid2 <- stack(tempGrid2,  prcp.tmp) # brick or stack?
  }
  print(i)
  
}

# write out to disk
writeRaster(tempGrid2,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/cpc_precip_conus_1948_2017.grd", overwrite=TRUE )

# crop to SW
cpcPrecip<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/cpc_precip_conus_1948_2017.grd")

# crop extent
# AZ/NM bbox -115.268555,30.921076,-102.568359,37.230328
e <- extent(-115.268555, -102.568359, 30.921076, 37.230328)
cpcPrecip <- crop(rotate(cpcPrecip), e)

# write out to disk
writeRaster(cpcPrecip,filename="/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/AZNM_cpc_precip_conus_1948_2017.grd", overwrite=TRUE )


