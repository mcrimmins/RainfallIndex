# Extract, plot, analyze gridded precip time series
# 11/21/18 MAC 

# compare to USDA tool https://prodwebnlb.rma.usda.gov/apps/prf

library(raster)
library(rasterVis)
library(reshape2)
library(cowplot)

# set rasteroptions
rasterOptions(progress = 'text')

# Download States boundaries (might take time)
#states <- getData('GADM', country='United States', level=1)
#counties<-getData('GADM', country='USA', level=2)


# load CPC data layers
cpc.moSumPrecip<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/moSumPrecip_AZNM_cpc_precip_conus_1948_2017.grd")
cpc.moAvg<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/moAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd")
cpc.twoMoSumClimo<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2AvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd")
cpc.percAvgPrecip<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2percAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd")
cpc.twoMoSumPrecip<-stack("/scratch/crimmins/cpc_conus/Datasets/cpc_us_precip/RainIndex/mo2sumAvgPrecip_AZNM_cpc_precip_conus_1948_2017.grd")

# load ClimGrid data layers
clim.moSumPrecip<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_moSumPrecip_AZNM_climgrid_1948_2017.grd")
clim.moAvg<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_moAvgPrecip_AZNM_climgrid_1948_2017.grd")
clim.twoMoSumClimo<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_mo2AvgPrecip_AZNM_climgrid_1948_2017.grd")
clim.percAvgPrecip<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_mo2percAvgPrecip_AZNM_climgrid_1948_2017.grd")
clim.twoMoSumPrecip<-stack("/scratch/crimmins/climgrid/processed/AZNM_RainIndex/resamp_mo2sumAvgPrecip_AZNM_climgrid_1948_2017.grd")

# load ClimGrid data layers
prism.moSumPrecip<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_moSumPrecip_AZNM_PRISM_1948_2017.grd")
prism.moAvg<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_moAvgPrecip_AZNM_PRISM_1948_2017.grd")
prism.twoMoSumClimo<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_mo2AvgPrecip_AZNM_PRISM_1948_2017.grd")
prism.percAvgPrecip<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_mo2percAvgPrecip_AZNM_PRISM_1948_2017.grd")
prism.twoMoSumPrecip<-stack("/scratch/crimmins/PRISM/monthly/processed/AZNM_RainIndex/resamp_mo2sumAvgPrecip_AZNM_PRISM_1948_2017.grd")

# extract time series
#Grid ID: 15383 from PRF tool
#Latitude: 32.89030°
#Longitude: -109.36740°
x<--109.36740
y<-32.89030

# create dates
twoMoDates<-as.Date(names(cpc.twoMoSumPrecip), format = "X%Y.%m.%d")
twoMoMonths<-as.numeric(format(as.Date(names(cpc.twoMoSumPrecip), format = "X%Y.%m.%d"), format = "%m"))
twoMoYears<-as.numeric(format(as.Date(names(cpc.twoMoSumPrecip), format = "X%Y.%m.%d"), format = "%Y"))

# extract time series - two month precip
cpc.twoMoSumPrecip.ts<-t(raster::extract(cpc.twoMoSumPrecip, cellFromXY(cpc.twoMoSumPrecip, c(x,y))))
clim.twoMoSumPrecip.ts<-t(raster::extract(clim.twoMoSumPrecip, cellFromXY(clim.twoMoSumPrecip, c(x,y))))
prism.twoMoSumPrecip.ts<-t(raster::extract(prism.twoMoSumPrecip, cellFromXY(prism.twoMoSumPrecip, c(x,y))))

twoMoPrecip<-as.data.frame(cbind(twoMoDates,twoMoMonths,twoMoYears,cpc.twoMoSumPrecip.ts,clim.twoMoSumPrecip.ts,prism.twoMoSumPrecip.ts))
colnames(twoMoPrecip)<-c("date","month","year","CPC","ClimGrid","PRISM")

twoMoPrecipMelted<-melt(twoMoPrecip,measure.vars = c("CPC","ClimGrid","PRISM"))

# extract time series - two month percent of average
cpc.percAvgPrecip.ts<-t(raster::extract(cpc.percAvgPrecip, cellFromXY(cpc.percAvgPrecip, c(x,y))))
clim.percAvgPrecip.ts<-t(raster::extract(clim.percAvgPrecip, cellFromXY(clim.percAvgPrecip, c(x,y))))
prism.percAvgPrecip.ts<-t(raster::extract(prism.percAvgPrecip, cellFromXY(prism.percAvgPrecip, c(x,y))))

twoMoPercAvg<-as.data.frame(cbind(twoMoDates,twoMoMonths,twoMoYears,cpc.percAvgPrecip.ts,clim.percAvgPrecip.ts,prism.percAvgPrecip.ts))
colnames(twoMoPercAvg)<-c("date","month","year","CPC","ClimGrid","PRISM")

twoMoPercAvgMelted<-melt(twoMoPercAvg,measure.vars = c("CPC","ClimGrid","PRISM"))

# month codes
mo.code<-NULL
mo.code$abb<-paste0(month.abb[seq(1,12,1)],".",month.abb[c(seq(2,12,1),1)])
mo.code$month<-seq(1,12,1)
twoMoPrecipMelted<-merge(twoMoPrecipMelted, mo.code, key="month")
twoMoPercAvgMelted<-merge(twoMoPercAvgMelted, mo.code, key="month")
twoMoPercAvg<-merge(twoMoPercAvg, mo.code, key="month")
# reorder
twoMoPrecipMelted$abb <- ordered(twoMoPrecipMelted$abb,
                                levels = mo.code$abb,
                                labels = mo.code$abb)
twoMoPercAvgMelted$abb <- ordered(twoMoPercAvgMelted$abb,
                                 levels = mo.code$abb,
                                 labels = mo.code$abb)
twoMoPercAvg$abb <- ordered(twoMoPercAvg$abb,
                                  levels = mo.code$abb,
                                  labels = mo.code$abb)

  # plot two month total precip
ggplot(twoMoPrecipMelted, aes(year,value/25.4,color=variable))+
  geom_line()+
  facet_wrap(.~abb, nrow=3)+
  ylab("inches")+
  ggtitle("Two-month total precip 1948-2017 - Grid #15383")

# plot two month total precip
ggplot(twoMoPercAvgMelted, aes(year,value,color=variable))+
  geom_line()+
  facet_wrap(.~abb, nrow=3)+
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 0.5)

# scatter plot of perc of average
ggplot(twoMoPercAvg, aes(CPC*100,PRISM*100))+
  geom_point()+
  facet_wrap(.~abb, nrow=3)+
  geom_abline(intercept = 0, slope = 1)+
  ylim(0,400)+
  xlim(0,400)+
  ggtitle("Two-month % of Average 1948-2017 - Grid #15383")+
  coord_fixed()

# time series of CPC-PRISM % and CPC-ClimGrid
# heat maps of occurrence of 70/80/90 % of average
# categorical map of CPC,PRISM,ClimGrid occurence at each threshold for each two mo period/year
# frequency map of < 70/80/90 % of avg
# frequency bar plot of < 70/80/90 % of avg by interval
# compare number of PRISM cells < threshold at 4km relative to CPC estimate at 12km resolution. How spatially uniform is drought at diff resolutions

# CPC heat map
twoMoPercAvg$moLabel <- ordered(twoMoPercAvg$month,
                     levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = names(clim.twoMoSumClimo))

twoMoPercAvg$cpc.lt90 <- ifelse(twoMoPercAvg$CPC < 0.7, 1, 0)# set 0 to NA
twoMoPercAvg$prism.lt90 <- ifelse(twoMoPercAvg$PRISM < 0.7, 10, 0) # set 0 to NA
twoMoPercAvg$both.lt90<- twoMoPercAvg$cpc.lt90+twoMoPercAvg$prism.lt90

# single grid occurrence
ggplot(twoMoPercAvg, aes(moLabel,year))+
  geom_tile(aes(fill=cpc.lt90), color="grey")+
  scale_fill_gradient(low = "orange",high = "orange", na.value="white")+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+
  guides(fill=FALSE)+
  xlab("Two-month Interval")+
  ggtitle("Drought Events (<70% of Avg) 1948-2017 - CPC Grid #15383")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# both grid occurrence
twoMoPercAvg$both.lt90[twoMoPercAvg$both.lt90 == 0] <- NA
twoMoPercAvg$both.lt90<-as.factor(twoMoPercAvg$both.lt90)
ggplot(twoMoPercAvg, aes(moLabel,year))+
  geom_tile(aes(fill=both.lt90), color="grey")+
  scale_fill_manual(values = c("orange", "blue","darkgreen"), na.value="white", name="Event",
                    labels=c("CPC", "PRISM","Both", "No Drought"))+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+
  #guides(fill=FALSE)+
  xlab("Two-month Interval")+
  ggtitle("Drought Events (<70% of Avg) 1948-2017 - CPC and PRISM Grid #15383")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p <- ggplot(nba.m, aes(variable, Name)) + 
    geom_tile(aes(fill = rescale),colour = "white") + 
    scale_fill_gradient(low = "white",high = "steelblue")

