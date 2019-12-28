# PRF Rainfall Index grid cell query
# get CPC precip and RCC-ACIS stations for specified grid cell
# MAC 12/16/2019

library(ncdf4)
# look at https://publicwiki.deltares.nl/display/OET/Reading+NetCDF+directly+from+OPeNDAP+using+R

#url<-'https://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/cpc_us_precip/RT/precip.V1.0.2019.nc.dods?lat[0:1:119],lon[0:1:299],time[0:1:348],precip[0:1:100][0:1:0][0:1:0]'

url<-'https://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/cpc_us_precip/RT/precip.V1.0.2019.nc'


#download.file(url, destfile="test.nc", mode="wb")

test<-nc_open(url)

## Look what's inside
str(ncatt_get(test, "lon"))
str(ncatt_get(test, "lat"))
str(ncatt_get(test, "time"))
str(ncatt_get(test, "precip"))


## Get time series data
time <- ncvar_get(test,"time")
precip <- ncvar_get(test,"precip")

## Convert G.time to real date
date <- as.Date(G.time, origin = "1970-01-01 00:00:00 +01:00")



map<-brick("SIx_average_leaf_prism.nc")
