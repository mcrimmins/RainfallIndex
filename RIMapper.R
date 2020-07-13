# Rainfall Index mapper - with stations
# MAC 01/28/19

library(raster)
library(RCurl)
library(leaflet)
library(htmltools)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(leafem)
library(htmlwidgets)

# functions
add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]

# get specified date period
date1<-"2020-05-01"
date2<-as.character(add.months(as.Date(date1),2)-1)

# get current year
currYr<-format(Sys.time(), "%Y")
prevYr<-as.numeric(format(Sys.time(), "%Y"))-1
# get US CPC precip
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/cpc_us_precip/RT/precip.V1.0.",prevYr,".nc")
download.file(URL, destfile = paste0("/home/crimmins/RProjects/RainfallIndex/temp",prevYr,".nc"), method="curl")
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/cpc_us_precip/RT/precip.V1.0.",currYr,".nc")
download.file(URL, destfile = paste0("/home/crimmins/RProjects/RainfallIndex/temp",currYr,".nc"), method="curl")

# load data into rasters
precipPrev<-stack(paste0("/home/crimmins/RProjects/RainfallIndex/temp",prevYr,".nc"))
precipCurr<-stack(paste0("/home/crimmins/RProjects/RainfallIndex/temp",currYr,".nc"))
 
allPrecip<-rotate(stack(precipPrev,precipCurr))

# clip out to AZ
e <- extent(-115, -109, 31, 37)
subPrecip <- crop(allPrecip, e)

# develop mesh grid
#RIGrid<-rasterToPolygons(subPrecip[[1]])
#RIGrid <- spTransform(RIGrid, CRS("+init=epsg:4326")) # WGS

# get specified date period
#date1<-"2019-10-01"
#date2<-"2019-11-30"

# sum up RI precip
lyr1<-paste0("X",format(as.Date(date1), "%Y"),".",format(as.Date(date1), "%m"),".",format(as.Date(date1), "%d"))
lyr2<-paste0("X",format(as.Date(date2), "%Y"),".",format(as.Date(date2), "%m"),".",format(as.Date(date2), "%d"))
sumPrecip<-calc(subPrecip[[which(names(subPrecip)==lyr1):which(names(subPrecip)==lyr2)]], sum)/25.4 

# develop mesh grid
RIGrid<-rasterToPolygons(sumPrecip[[1]], na.rm = TRUE)
  #RIGrid$layer<-RIGrid$layer/25.4
#RIGrid <- spTransform(RIGrid, CRS("+init=epsg:4326")) # WGS
#RIGrid <- spTransform(RIGrid, CRS("+init=epsg:3857"))
#sumPrecip <- projectRaster(sumPrecip, crs=CRS("+init=epsg:3857"))

# get station data from RCC-ACIS
networkCodes<-as.data.frame(cbind(c(1,2,3,4,5,6,7,9,10,29), c("WBAN","COOP","FAA","WMO","ICAO","GHCN","NWSLI","THRDX","CoCoRAHS","CADX")))
colnames(networkCodes)<-c("code","netName") 
  networkCodes$code<-(as.character(networkCodes$code))
  networkCodes$netName<-as.character(networkCodes$netName)  
# get query
jsonQuery<-paste0('{"state":"AZ","sdate":"',date1,'","edate":"',date2,'","elems":[{"name":"pcpn","interval":"dly","duration":1,"add":"n","smry":{"add":"mcnt","reduce":"sum"},"smry_only":"1"}]}')
out<-postForm("http://data.rcc-acis.org/MultiStnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
# json to lists
out<-fromJSON(out, simplifyVector = TRUE, simplifyDataFrame = TRUE)

# full dataframe
#rawDF<-as.data.frame(out)

# wrangle into dataframe
ll<-out$data$meta$ll
  ll[unlist(lapply(ll,is.null))] <- NA
  ll<-data.frame(do.call(rbind, ll))
# station ids  
sids<-as.data.frame(t(sapply(out$data$meta$sids, '[', seq(max(sapply(out$data$meta$sids, length))))))

# parse out network numbers
  sids<-separate(data = sids, col = V1, into = c("sid", "network"), sep = " ")
  sids<-separate(data = sids, col = V2, into = c("sid2", "network2"), sep = " ")
  # diff join
  sids<-left_join(sids,networkCodes,by = c("network" = "code"))
  sids<-left_join(sids,networkCodes,by = c("network2" = "code"))
  # join network names
  #sids<-merge(sids,networkCodes, by.x="network",by.y="code", all.x=TRUE)
  #sids<-merge(sids,networkCodes, by.x="network2",by.y="code", all.x=TRUE)
  #sids$names<-names
  # station names and values
names<-out$data$meta$name
precipVals<-data.frame(matrix(unlist(out$data$smry), nrow=nrow(out$data), byrow=T))
  cols.num <- c(1,2)
  precipVals[cols.num] <- sapply(precipVals[cols.num],as.character)
# build final dataframe
fullDF<-data.frame(names=names,
                   lat=ll$X2,
                   lon=ll$X1,
                   sid=sids$sid,
                   netName1=sids$netName.x,
                   netName2=sids$netName.y,
                   totalPrecip=as.numeric(as.character(precipVals$X1)),
                   missing=as.numeric(as.character(precipVals$X2)))

# station labs
labs <- lapply(seq(nrow(fullDF)), function(i) {
  paste0( '<p> <b> Station name:', fullDF[i, "names"], '</b></p>', 
          '<p> Network Name:', fullDF[i, "netName1"], '</p>',
          '<p> Network Name2:', fullDF[i, "netName2"], '</p>',
          '<p> <b> <font color="green"> Total Precip (in):', fullDF[i, "totalPrecip"], '</b></font></p>',
          '<p> <font color="red"> Missing days:', fullDF[i, "missing"], '</font></p>') 
})

# create leaflet map
#pal <- colorNumeric(heat.colors(3, 1, rev = FALSE), values(sumPrecip),
#                     na.color = "transparent")

pal <- colorNumeric(
  palette = colorRampPalette(c('blue','green','red'))(length(fullDF$totalPrecip)), 
  domain = fullDF$totalPrecip)

pal1 <- colorNumeric(
  palette = colorRampPalette(c('blue','green','red'))(length(RIGrid$layer)), 
  domain = RIGrid$layer)

my_title <- tags$p(tags$style("p {color: black; font-size:12px}"),
                   tags$b("Rainfall Index Grid and"),
                   tags$br(),
                   tags$b("Weather Stations"),
                   tags$br(),
                   tags$b(paste0(date1," to ",date2)))

RIlabels<-as.character(paste0("RI ",round(RIGrid$layer,2)," in."))


leafMap<-leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(-111.839389, 33.178586, zoom = 8) %>%
  addRasterImage(sumPrecip, colors = pal, opacity = 0.1, layerId = "RI total precip (in)", project = FALSE) %>%
  #addMouseCoordinates() %>%
  addImageQuery(sumPrecip, type="mousemove", layerId = "RI total precip (in)", prefix = "", digits = 2)  %>%
  addPolygons(data=RIGrid, color = pal1(RIGrid$layer), weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.3,
              fill =  pal1(RIGrid$layer),
              highlightOptions = highlightOptions(color = "red", weight = 2,
                                                  bringToFront = FALSE),
              label = RIlabels) %>%
  addControl(my_title, position = "topright" ) %>%
  addCircleMarkers(fullDF$lon, fullDF$lat,
                   radius = 4,
                   color =  pal(fullDF$totalPrecip),
                   label = lapply(labs, htmltools::HTML))

saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/RainfallIndex/maps/RIGridMap_",date1,"to",date2,".html"), selfcontained = FALSE)


# create Website with markdown ----
library(rmarkdown)
render('/home/crimmins/RProjects/RainfallIndex/maps/RIMaps.Rmd', output_file='RainfallIndexMaps.html',
       output_dir='/home/crimmins/RProjects/RainfallIndex/maps', clean=TRUE)

# to do: reset leaflet zoom level/center, make grid more transparent, circles less transparent..